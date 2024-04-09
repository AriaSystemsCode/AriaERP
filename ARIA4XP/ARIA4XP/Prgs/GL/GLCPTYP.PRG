*****************************************************************************
*: Program file        : GLCPTYP.PRG
*: Program desc.       : Copy Type and Range
*: Module              : General Ledger (GL)
*: System		       : Aria Apparel System (A4xp)
*: Developer	       : Saber Saber [SAB] 
*: Tracking Job Number : E303272.EXE
*: Date                : 10/09/2012
*****************************************************************************
*Modifications :
*****************************************************************************

#INCLUDE R:\Aria4xp\PRGs\GL\GLCOPY.H

SET PROCEDURE TO ADDBS(oAriaApplication.ProgramHome)+'GL\GL.FXP' ADDITIVE

*- lcTmpGridSrc  ===>    Temp file name for selected accounts
*- lc_Temp1      ===>    Temp file name for group lines
*- lc_Temp2      ===>    Temp file name for selected criteria
*- lcAlTypes     ===>    Alias name to GLTYPES  in source company
*- lcAlAcHar     ===>    Alias name to GLACCHAR in source company
*- lnNoOfAcc     ===>    var. to store number of selected accounts
*- lc_Types      ===>    
PRIVATE lcTmpGridSrc, lc_Temp1, lc_Temp2, lcAlTypes, lcAlAcHar
STORE '' TO lcTmpGridSrc, lc_Temp1, lc_Temp2, lcAlTypes, lcAlAcHar

lc_Temp1   = gfTempName()
lc_Temp2   = gfTempName()
lcAlTypes  = gfTempName()
lcAlAcHar  = gfTempName()

*- Open used files and Create temp files
=lfPrepFls()

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
=lfAddProp(loFormSet,'lc_Temp1', lc_Temp1)
=lfAddProp(loFormSet,'lc_Temp2', lc_Temp2)
=lfAddProp(loFormSet,'lcAlTypes', lcAlTypes)
=lfAddProp(loFormSet,'lcAlAcHar', lcAlAcHar)

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
  .aSourceArray[1,1]    = 'No company selected'
  .aSourceArray[1,2]    = '*'
  ACOPY(laComLst, .aSourceArray, 1, -1, 3)
  .Value = '*'
ENDWITH

*- Copy Option Button Caption
loFormSet.AriaForm1.cmdCopyOption.Caption = ''
loFormSet.AriaForm1.cmdCopyOption.Visible = .F.

*- Adjust Grid Control Source
WITH loFormSet.AriaForm1.grdCopyRecords
  LOCAL lcTmpGridSrc
  lcTmpGridSrc  = loFormSet.lcTmpGridSrc
  .ColumnCount  = 4
  .RecordSource = ''
  .RecordSource = loFormSet.lcTmpGridSrc
  LOCAL lnI,lcI
  lnI = 0
  oGrid = loFormSet.AriaForm1.grdCopyRecords
 
  *Inc, cTypeCode, SUBSTR(cTypeDesc,1,36), CopyStat
  =lfAddColumn(@lnI, oGrid, "&lcTmpGridSrc..Inc", '')
  =lfAddColumn(@lnI, oGrid, "&lcTmpGridSrc..cTypeCode", LANG_GLCPTYP_AutType, 50)                  &&"Type Code"  
  =lfAddColumn(@lnI, oGrid, "SUBSTR(&lcTmpGridSrc..cTypeDesc, 1, 36)", LANG_GLCPTYP_AutDesc, 300)  &&"Type Description"
  =lfAddColumn(@lnI, oGrid, "&lcTmpGridSrc..CopyStat"    ,LANG_GLCPTYP_CopyStat, 150)              &&"Copy Status"
    
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

=gfCloseTable('GLSETUP')
=gfCloseTable('GLTYPES')
=gfCloseTable('ACCOD')
=gfCloseTable('SYCCOMP')
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
=gfOpenTable('GLSETUP','','SH')    && CAUTTYPE+CAUTCODE
=gfOpenTable('GLTYPES','TYPECODE','SH')    && CTYPECODE
=gfOpenTable('ACCOD','ACCSEGNO','SH')      && ALLTRIM(STR(NACSSEGNO))
=gfOpenTable(ADDBS(ADDBS(oAriaApplication.ClientA27Path)+'SysFiles')+'SYCCOMP','CCOMP_ID','SH')     && CCOMP_ID
=gfOpenTable(ADDBS(ADDBS(oAriaApplication.ClientA27Path)+'SysFiles')+'SYDFIELD','CFLD_NAME','SH')   && CFLD_NAME

*- Create empty temp file (lcTmpGridSrc) with the same structure of GLTYPES + Inc, CopyStat,cStatus.
lcTmpGridSrc = gfTempName()
SELECT GLTYPES
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+3, 18]

laFileStru[lnFileStru+1, 1] = 'Inc'
laFileStru[lnFileStru+1, 2] = 'L'
laFileStru[lnFileStru+1, 3] = 1
laFileStru[lnFileStru+1, 4] = 0

laFileStru[lnFileStru+2, 1] = 'CopyStat'
laFileStru[lnFileStru+2, 2] = 'C'
laFileStru[lnFileStru+2, 3] = 17
laFileStru[lnFileStru+2, 4] = 0

laFileStru[lnFileStru+3, 1] = 'cStatus'
laFileStru[lnFileStru+3, 2] = 'C'
laFileStru[lnFileStru+3, 3] = 1
laFileStru[lnFileStru+3, 4] = 0

FOR lnLoop = 1 to 3
  STORE ' ' TO  laFileStru[lnFileStru+lnLoop, 7] ,laFileStru[lnFileStru+lnLoop, 8],;
                laFileStru[lnFileStru+lnLoop, 9] ,laFileStru[lnFileStru+lnLoop,10],;
                laFileStru[lnFileStru+lnLoop,11] ,laFileStru[lnFileStru+lnLoop,12],;
                laFileStru[lnFileStru+lnLoop,13] ,laFileStru[lnFileStru+lnLoop,14],;
                laFileStru[lnFileStru+lnLoop,15] ,laFileStru[lnFileStru+lnLoop,16]
  STORE 0 TO  laFileStru[lnFileStru+lnLoop,17] ,laFileStru[lnFileStru+lnLoop,18]
ENDFOR

CREATE TABLE ADDBS(oAriaApplication.Workdir)+(lcTmpGridSrc)+'.DBF' FROM ARRAY laFileStru

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

LOCAL lcAlTypes, lcComPath
lcAlTypes = loFormSet.lcAlTypes
lcComPath = ADDBS(ALLTRIM(loFormSet.AriaForm1.cboFromCompany.Value))

IF USED(lcAlTypes)
  SELECT (lcAlTypes)
ELSE   
  SELECT 0   
ENDIF  
USE &lcComPath.GLTypes  AGAIN ALIAS  &lcAlTypes ORDER TAG TYPECODE

=lfvCopyOption(loFormSet)

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

lcTmpGridSrc = loFormSet.lcTmpGridSrc

SELECT (loFormSet.lcTmpGridSrc)
ZAP

SELECT (loFormSet.lcAlTypes)
GO TOP
IF EOF()
  USE
  SELECT (lcTmpGridSrc)
  =gfModalGen("TRM02119B00000","Dialog","types")          
  RETURN .F.
ENDIF

SELECT (loFormSet.lcAlTypes)  
SCAN
  SCATTER MEMO MEMVAR
  m.Inc      = .F.
  m.CopyStat = SPACE(14)
  m.cStatus  = 'S'
  SELECT (loFormSet.lcTmpGridSrc)
  APPEND BLANK
  GATHER MEMO MEMVAR
ENDSCAN

SELECT (lcTmpGridSrc)
INDEX ON cTypeCode TAG cTypeCode
SET ORDER TO TAG cTypeCode 
*- set relation between cTypeCode in the temp file and  GlTypes (account types) in the source company path.
SET RELATION TO &lcTmpGridSrc..cTypecode INTO &lcAlTypes 

SELECT (loFormSet.lcTmpGridSrc)
LOCATE
loFormSet.AriaForm1.grdCopyRecords.Refresh()

ENDFUNC 
*- End of lfvCopyOption


*!*************************************************************
*! Name      : lfvCopy
*! Developer : Saber A.Razek (SAB)
*! Date      : 10/09/2012
*! Purpose   : Function to Copy selected records from the grid to active Company
*!*************************************************************
FUNCTION  lfvCopy
LPARAMETERS loFormSet

PRIVATE lcTmpGridSrc, lc_Temp1, lc_Temp2, lcAlTypes, lcAlAcHar, lcComPath, lcWorkDir, lcDataDir
lcTmpGridSrc = loFormSet.lcTmpGridSrc
lc_Temp1     = loFormSet.lc_Temp1
lc_Temp2     = loFormSet.lc_Temp2
lcAlTypes    = loFormSet.lcAlTypes
lcAlAcHar    = loFormSet.lcAlAcHar
lcComPath    = ADDBS(ALLTRIM(loFormSet.AriaForm1.cboFromCompany.Value))
lcWorkDir    = oAriaApplication.WorkDir
lcDataDir    = oAriaApplication.DataDir

lnTotalRecs = 0
SELECT (lcTmpGridSrc)
lnRecNum = RECNO()

*- start copy selected types
SELECT (lcTmpGridSrc)
*- check if there is any selected types to copyied
LOCATE  FOR Inc
*- case there is no selected types to copyied
IF !FOUND()
   *** Message "No included accounts to copy."
   ***               < Ok >
   =gfModalGen("TRM02103B00000","DIALOG","copy")   
  RETURN .F.
ENDIF

*- count the number of included types to the variable lnTotRecs
COUNT FOR Inc TO lnTotRecs

*- initialize no. of copied types , current type variable.
lnCopyTypes = 0               && init. the no of copied types.
lnCurRec    = 0               && init. the current type variable.

SELECT (lcTmpGridSrc)
SCAN FOR !EMPTY(Inc)
  *- Tracking current record
  lnCurRec = lnCurRec + 1

  *- cheking if the copied type Already exists
  IF SEEK (&lcTmpGridSrc..cTypeCode, "GLTYPES")
    REPLACE &lcTmpGridSrc..CopyStat WITH 'Already exists'
  ELSE
    IF lfCheckTyp()
      *- Update GLTYPES
      SELECT (lcTmpGridSrc)    
      *- Collecting types from temporary file to memory variables
      SCATTER MEMVAR
      SCATTER FIELDS Llok_Stat,Clok_user,Dlok_Date,Clok_time MEMVAR BLANK
      
      *- inserting selected types into "gltypes" file
      INSERT INTO &lcDataDir.GLTYPES FROM MEMVAR
      
      *- Adding audit information to "GLTYPES" files
      =gfAdd_Info('GLTYPES')
      =gfReplace('')
      
      *- Adjusting copied types counter
      lnCopyTypes = lnCopyTypes + 1
      
      *- Replacing copystat with proper variable 
      REPLACE &lcTmpGridSrc..CopyStat WITH 'Copied'
    ENDIF
  ENDIF  
  SELECT (lcTmpGridSrc)
ENDSCAN

SELECT GLTYPES
=gfTableUpdate()

*- Refreshing the list
SELECT (lcTmpGridSrc)
REPLACE INC WITH .F. FOR !EMPTY(&lcTmpGridSrc..CopyStat)

*** message with no of types copied.
*WAIT ALLTRIM(STR(lnCopyTypes))+" Types copied from source company..." ;
     WINDOW NOWAIT
=gfWait("02240","NOWAIT",ALLTRIM(STR(lnCopyTypes)))

IF lnCopyTypes > 0
  IF SEEK('',"ACCOD") 
   REPLACE ACCOD.lAcsUsed WITH .T.
  ENDIF
ENDIF

loFormSet.AriaForm1.cboFromCompany.Value = '*'
loFormSet.AriaForm1.cboFromCompany.Valid()

ENDFUNC 
*-End of lfvCopy


*!*************************************************************
*! Name      : lfCheckTyp
*! Developer : Saber A.Razek (SAB)
*! Date      : 10/09/2012
*! Purpose   : Function validate that the copied type is not deleted from the 
*!             source file and type ranges does not overlap with type ranges 
*!             in the target company if it is not main type (without ranges) 
*!*************************************************************
FUNCTION  lfCheckTyp

*- checking if selected types are deleted from source company
IF EOF(lcAlTypes)       
  REPLACE &lcTmpGridSrc..CopyStat WITH 'Source deleted'
  RETURN .F.
ENDIF  

*- Checking that the selected type range is not overlaping with any other ranges in the target company file
SELECT GLTYPES
LOCATE FOR GLTYPES.cTypeCode <> &lcTmpGridSrc..cTypeCode .AND. (BETWEEN(VAL(&lcTmpGridSrc..cTypUacno),VAL(GLTYPES.cTyplacno),VAL(GLTYPES.cTypuacno));
  .OR. BETWEEN(VAL(&lcTmpGridSrc..cTypLacno),VAL(GLTYPES.cTyplacno),VAL(GLTYPES.cTypuacno)))

*- checking that the selected type is not a primary one otherwise bypassing range checking.
IF FOUND() .AND. (!RIGHT(&lcTmpGridSrc..cTypeCode,2)= "00")
  REPLACE &lcTmpGridSrc..CopyStat WITH 'Ranges Overlaped'
  RETURN .F.
ELSE
  RETURN .T.
ENDIF  

SELECT (lcTmpGridSrc)

ENDFUNC
*- End of lfCheckTyp
