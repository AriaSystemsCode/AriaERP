*:************************************************************************
*:  Program File: \ARIA4XP\PRGS\SM\SMNMCNT.Prg
*:  Module      : System Manager 
*: Program desc. : Numeric Sequence Control
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 02/18/2013
*:  Reference   : *E303354,1 
*:************************************************************************
lcRunScx = lfGetScx("SM\SMNMCNT.scx")
DO FORM (lcRunScx) 

************************************************************
*! Name      : lfGetScx
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/05/2012
*! Purpose   : Get the scx path to run in SaaS environemt
************************************************************
FUNCTION lfGetScx
PARAMETERS lcScx
LOCAL lcRunScx
IF oAriaApplication.Multiinst AND FILE(oAriaApplication.clientscreenhome+lcScx)
  lcRunScx = oAriaApplication.clientscreenhome+lcScx
ELSE
  lcRunScx = oAriaApplication.screenhome+lcScx
ENDIF   
RETURN lcRunScx
 *- End of lfGetScx.

*!*************************************************************
*! Name      : lfFormInit
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/05/2012
*! Purpose   : called from the Screen init Method
*!*************************************************************
FUNCTION lfFormInit
PARAMETERS loFormSet

loFormSet.AddProperty('lcProgName','SMNMCNT')

*- Set functions to the APMAIN.FXP
lcPath = oAriaapplication.ApplicationHome
SET PROCEDURE TO (lcPath+'AP\APMAIN.FXP') ADDITIVE 
SET PROCEDURE TO (lcPath+'GL\GL.FXP') ADDITIVE 

*- Open tables 
=lfOpenPRGFILES(loFormSet.lcProgName)

IF !lfDefineVars(loFormSet)
  RETURN .F.
ENDIF   

*- set row Source for the companies popup
WITH loFormSet.Ariaform1
  .laCompany.RowSource = 'Thisformset.laCompany'
  .laDivArray.RowSource = 'Thisformset.laDivArray'
ENDWITH 

oGrp = loFormSet.Ariaform1.laDivArray
STORE ' ' TO oGrp.Value, oGrp.OldValue
oCmp = loFormSet.Ariaform1.laCompany
oCmp.OldValue = ' '
oCmp.Value = oAriaApplication.ActiveCompanyID
=lfvCompany(loFormset,oCmp)

*- End of lfFormInit.

************************************************************
*! Name      : lfDefineVars
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 02/18/2013
*! Purpose   : Define screen variables
************************************************************
FUNCTION lfDefineVars
PARAMETERS loFormSet


loFormSet.AddProperty('laDivArray[1,1]')
loFormSet.AddProperty('laCompany[1,4]')

*-- Declare multi session saved variables [Begin]
loFormSet.AddProperty('lcBrowTitl','')
loFormSet.AddProperty('lcGroup','')
loFormSet.AddProperty('lcOldValue','')
loFormSet.AddProperty('lcTmpSeq','')
loFormSet.AddProperty('lcTemplate','')

loFormSet.AddProperty('lcDivStat','ENABLE')

loFormSet.AddProperty('lcTopBtn','DISABLE')
loFormSet.AddProperty('lcBotBtn','DISABLE')
loFormSet.AddProperty('lcNxtBtn','DISABLE')
loFormSet.AddProperty('lcPrvBtn','DISABLE')

loFormSet.AddProperty('lnTop',0)
loFormSet.AddProperty('lnBottom',0)
loFormSet.AddProperty('lnSelRec',0)

loFormSet.AddProperty('lcCompCode', oAriaApplication.ActiveCompanyID )
loFormSet.AddProperty('lcCompDir', oAriaApplication.DataDir )
loFormSet.AddProperty('lcCompMod', oAriaApplication.companyinstalledmodules )

loFormSet.AddProperty('llNewGrp'   , .F. ) && add New Group flag. [Enable New Button if .T. and select or view modes.]
loFormSet.AddProperty('llNewMode'  , .F. ) && add mode flag.      [Control save and cancel process]
loFormSet.AddProperty('llDivBased' , .F. ) && if Sequence based on division
loFormSet.AddProperty('llEmptyGrp' , .F. ) && if there is one group but it is empty group.
loFormSet.AddProperty('llFindPrev' , .F. ) && if we find previous group in current company.
loFormSet.AddProperty('llVldComp'  , .T. ) && .T. if user press tab when passing company popup.

loFormSet.AddProperty('llFromEdit',.F.)

loFormSet.AddProperty('lnMarker'   , 1  )  && Browse marker

loFormSet.AddProperty('lnCompany'  , 1  )  && Default company item is equal 1
loFormSet.AddProperty('lnGroup'    , 1  )  && Default group item is equal 1

SELECT ccomp_id+" - "+cCom_Name,PADR(gfGetDataDir(ALLT(cCom_dDir)),LEN(cCom_dDir)),mModlSet,cComp_ID ;
  FROM syccomp ;
  INTO ARRAY loFormSet.laCompany ;
  ORDER BY 1
  
IF _TALLY > 0
  loFormSet.Ariaform1.laCompany.Value = oAriaApplication.ActiveCompanyID
ELSE

  *-- Message : 'There are no companies installed in system'
  *--                           <Ok>
  =gfModalGen('INM00339B00000','DIALOG')
  RETURN .F.
    
ENDIF  

loFormSet.lcTmpSeq    = gfTempName()
loFormSet.lcTemplate  = gfTempName()

lcBrowTitl  = 'Sequences'

CREATE TABLE (oAriaApplication.WorkDir+loFormSet.lcTmpSeq) ;
    (cComp_Id C(2),cFile_Nam C(8),cFld_Name C(10),nFld_Wdth N(3,0),nSeq_no N(12,0),   ;
    nOldSeq N(12,0),cFld_Head C(25),mFld_Des M(10), cSeq_Type C(10) , cSeq_Group C(3),;
    nSeqRec N(7,0),cData_Typ C(1),cFile_Tag C(10),lPerComp L(1),lNoEdit L(1),         ; 
    mModExpr M(10),cAdd_User C(10),dAdd_Date D(8),cAdd_Time C(11),lLok_Stat L(1),     ;
    cLok_User C(10),dLok_date D(8),cLok_Time C(8),cseq_chr  c(1))

INDEX ON cComp_Id + cSeq_Group + cFld_Name TAG (loFormSet.lcTmpSeq) 
APPEND BLANK  && Append dummy record because initially we does not have data in temp. file

= AFIELDS(laTempStru)
CREATE CURSOR (loFormSet.lcTemplate) ;
     FROM ARRAY laTempStru
     
SELECT (loFormSet.lcTemplate)   
ZAP
INDEX ON cSeq_Type TAG (loFormSet.lcTemplate) OF (oAriaApplication.WorkDir+loFormSet.lcTemplate+'.CDX')
  
=lfOpenFls(loFormSet)

lcBaseFile = loFormSet.lcTmpSeq  && Navigate in Temporary file.

*- End of lfDefineVars.


************************************************************
*! Name      : lfSetGridDataSource
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 22/05/2012
*! Purpose   : Set the Grid Data Source
************************************************************
FUNCTION lfSetGridDataSource
PARAMETERS loFormSet
oGrd = loFormSet.Ariaform1.grdSMNMCNT
WITH oGrd
  .RecordSource = ''
  .RecordSource = loFormSet.lcTmpSeq
ENDWITH   

lcTmpSeq = loFormSet.lcTmpSeq
lfSetColumnsProp('1',"&lcTmpSeq..cfld_head",'Transaction',380 ,oGrd)
lfSetColumnsProp('2',"&lcTmpSeq..nSeq_no","Sequence #"   ,100 ,oGrd)
oGrd.ReadOnly = .T.

*- End of lfSetGridDataSource.

************************************************************
*! Name      : lfSetColumnsProp
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 05/23/2012
*! Purpose   : Set Columns Properties
************************************************************
FUNCTION lfSetColumnsProp
PARAMETERS lcCol,lcSrc,lcHeader,lnWidth,loGrd
lnWidth = IIF(EMPTY(lnWidth),50,lnWidth)
WITH loGrd
  .Column&lcCol..Header1.Caption = lcHeader
  .Column&lcCol..ControlSource   = lcSrc
  .Column&lcCol..Width           = lnWidth
ENDWITH 
*- End of lfSetColumnsProp.

************************************************************
*! Name      : lfFormAfterRowColumnChange
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 02/20/2013
*! Purpose   : grid interactive 
************************************************************
FUNCTION lfFormAfterRowColumnChange
PARAMETERS loFormSet
lcTmpSeq = loFormSet.lcTmpSeq
WITH loFormset.AriaForm1
  .lcDesc.Value = &lcTmpSeq..mFld_Des
  .Refresh()
ENDWITH   

************************************************************
*! Name      : lfFormDestroy
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 02/19/2013
*! Purpose   : Destroy event
*-- Close any used files in this session.
************************************************************
FUNCTION lfFormDestroy
PARAMETERS loFormSet

IF USED(loFormSet.lcTemplate)
  USE IN (loFormSet.lcTemplate)  && Close Templete cursor.
ENDIF  
  
=lfClos_Fil('Seq_File')

IF USED(loFormSet.lcTmpSeq)  && We are going to close the Temp Seq and then Erase it.
  USE IN (loFormSet.lcTmpSeq)
ENDIF   && End of IF
 
IF FILE(oAriaApplication.WorkDir + loFormSet.lcTmpSeq + '.DBF')
  ERASE (oAriaApplication.WorkDir+loFormSet.lcTmpSeq+'.DBF')          && Erase the Temp file.
ENDIF    && End of IF FILE(oAriaApplication.WorkDir + lcTmpSeq + '.DBF')
  
IF FILE(oAriaApplication.WorkDir + loFormSet.lcTmpSeq + '.CDX')
  ERASE (oAriaApplication.WorkDir + loFormSet.lcTmpSeq +'.CDX')          && Erase the Temp index.
ENDIF    && End of IF FILE(oAriaApplication.WorkDir + lcTmpSeq + '.CDX')

*- End of lfFormDestroy.

************************************************************
*! Name      : lfFormActivate
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 02/19/2013
*! Purpose   : lfFormActivate
************************************************************
FUNCTION lfFormActivate
PARAMETERS loFormSet
IF loFormSet.ActiveMode = 'S'
  loFormSet.oToolbar.cmdSelect.Enabled = .T.
ENDIF   

*- End of lfFormActivate.

*!*************************************************************
*! Name      : lfStrtInfo
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/02/98
*! Purpose   : Get start information.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : lfDivInfo,lfSelGroup
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfStrtInfo()
*!*************************************************************
*!
FUNCTION lfStrtInfo
PARAMETERS loFormSet
=lfDivInfo(loFormSet)   && Get division group info.

*-- if system have only one company.
IF !loFormSet.llDivBased OR ALEN(loFormSet.laCompany,1) = 1
   
  = lfSelGroup(loFormSet)  && Detect coming screen mode and switch to it.

ENDIF
*-- end of lfStrtInfo.


*!*************************************************************
*! Name      : lfValidSeq
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/02/98
*! Purpose   : Valid Entered sequence Number.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : gfModalGen
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfValidSeq()
*!*************************************************************
*!
FUNCTION lfValidSeq
PARAMETERS loFormSet,loFld

IF loFld.Value  = loFld.OldValue
  RETURN
ENDIF 
  
lcTmpSeq = loFormSet.lcTmpSeq
IF (loFormSet.ActiveMode = 'E' OR loFormSet.ActiveMode = 'A') AND loFormSet.llDivBased AND ;
   !('ALL' $ loFormSet.laDivArray[loFormSet.lnGroup,1]) AND (nSeq_no <> nOldSeq)
  PRIVATE lcAlias
  lcAlias = ALIAS()

  *-- Scan the same sequences in another groups if based on division.
  SELECT Seq_File
  SCAN FOR cseq_type+cseq_group = &lcTmpSeq..cFld_Name AND (cseq_group <> &lcTmpSeq..cseq_group)
    IF ABS(&lcTmpSeq..nSeq_No - nSeq_No) < 10000
      *-- Message : Another XXX Transaction in group YYY occupied the same sequence range,
      *-- this sequence may be redundant in the future.
      *--                                   <Ok>
      =gfModalGen('INM00337B00000','DIALOG',ALLTRIM(&lcTmpSeq..cFld_Head) + '|' + ALLTRIM(cseq_group))
      EXIT
    ENDIF
  ENDSCAN
  SELECT (lcAlias)
  REPLACE nOldSeq WITH nSeq_no
ENDIF  
RETURN(.T.)
*-- end of lfValidSeq.


*!*************************************************************
*! Name      : lpShow
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/02/98
*! Purpose   : Show gets screen procedure.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : lfFillData
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpShow
*!*************************************************************
*!
PROCEDURE lpShow
PARAMETERS loFormSet

IF TYPE('loFormSet.lcProgName')='U'
  RETURN 
ENDIF 

lcTmpSeq = loFormSet.lcTmpSeq
loFormSet.Ariaform1.grdSMNMCNT.ReadOnly = .T.

DO CASE
  CASE loFormSet.ActiveMode = 'S'  && Select Mode

    IF loFormSet.llDivBased
      *-- User Come from add mode by press cancel [begin]
      IF loFormSet.llNewMode AND !('ALL' $ loFormSet.laDivArray[loFormSet.lnGroup,1]) AND ALEN(loFormSet.laDivArray,1) > 2 AND ;
         !EMPTY(&lcTmpSeq..cSeq_Type) AND ;
         !SEEK(PADR(&lcTmpSeq..cSeq_Type,10)+&lcTmpSeq..cSeq_Group,'Seq_File')
        DIMENSION loFormSet.laDivArray[ALEN(loFormSet.laDivArray,1) - 1,1]
        loFormSet.llNewGrp  = .T.
      ENDIF
      *-- User Come from add mode by press cancel [end]
    
    ELSE  && not based on division code
      *-- Select Company is defaulted.
      lnCompany = 1
      loFormSet.Ariaform1.laCompany.Value = ' '
      loFormSet.laDivArray[1,1] = "N/A"
    ENDIF  

    loFormSet.llNewMode = .F.

    loFormSet.llVldComp = .T.
    STORE 'ENABLE' TO lcCompStat,loFormSet.lcDivStat
    loFormSet.lnGroup   = 1
    loFormSet.Ariaform1.laDivArray.ListIndex = loFormSet.lnGroup
    lcDesc = ''

    loFormSet.Ariaform1.laCompany.Enabled = .T.
    loFormSet.Ariaform1.laDivArray.Enabled = loFormSet.lcDivStat = 'ENABLE'
    SELECT (loFormSet.lcTmpSeq)
    ZAP
    loFormSet.Ariaform1.grdSMNMCNT.RecordSource = ''    
    loFormSet.Ariaform1.laCompany.Setfocus()
    KEYBOARD '{ALT+DNARROW}'    


  CASE loFormSet.ActiveMode = 'V'  && View Mode
    
    IF loFormSet.llFromEdit
      = lfFillData(loFormSet)
      loFormSet.llFromEdit = .F.
    ENDIF  
    lfSetGridDataSource(loFormSet)

  CASE loFormSet.ActiveMode = 'E'  && Edit Mode

    loFormSet.llFromEdit = .T.
    WITH loFormSet.Ariaform1
      .grdSMNMCNT.Readonly = .F.
      .grdSMNMCNT.Column1.Readonly = .T.
      .grdSMNMCNT.Column2.Readonly = .F.
      .laCompany.Enabled = .F.
      .laDivArray.Enabled = .F.
    ENDWITH 

ENDCASE

IF loFormSet.llDivBased AND loFormSet.llNewGrp AND (loFormSet.ActiveMode = 'V' OR (loFormSet.ActiveMode = 'S' AND !EMPTY(loFormSet.Ariaform1.laCompany.Value) ))
  lcNewBtn = 'ENABLE'
ELSE
  lcNewBtn = 'DISABLE'
ENDIF
loFormSet.oToolbar.cmdAdd.Enabled = lcNewBtn = 'ENABLE'
loFormSet.Ariaform1.Refresh()
*-- end of lpshow.


*!*************************************************************
*! Name      : lfvCompany
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/02/98
*! Purpose   : Validation code for select company popup.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : lfSelGroup
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvCompany()
*!*************************************************************
FUNCTION lfvCompany
PARAMETERS loFormSet,loFld

loFormSet.llVldComp = .F.
  =lfDivInfo(loFormSet)
IF !EMPTY(loFld.Value)
  = lfSelGroup(loFormSet)
ENDIF  
=lfFormAfterRowColumnChange(loFormSet)
*-- end of lfvCompany.

*!*************************************************************
*! Name      : lfDivInfo
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/02/98
*! Purpose   : Selected division information.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : gfGetMemVar
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfDivInfo()
*!*************************************************************
*!
FUNCTION lfDivInfo
PARAMETERS loFormSet

*-- if it's select company item
IF EMPTY(loFormSet.Ariaform1.laCompany.Value)

  DECLARE loFormSet.laDivArray [1 , 1] 
  loFormSet.laDivArray[1,1] = ' N/A '

ELSE  && User select specific company.

  *-- Open Default Company Sequence file.
  lnCompany = loFormSet.Ariaform1.laCompany.ListIndex
  loFormSet.lcCompCode = PADR(loFormSet.laCompany[lnCompany,1],2)
  lcCompDir  = LOWER(ALLTRIM(loFormSet.laCompany[lnCompany,2]))
  loFormSet.lcCompMod  = loFormSet.laCompany[lnCompany,3]
  loFormSet.llDivBased = (gfGetMemVar('M_DIV_SEQ',loFormSet.lcCompCode) = 'Y')
  
  IF loFormSet.llDivBased
    DECLARE loFormSet.laDivArray [2 , 1] 
    loFormSet.laDivArray[1,1] = ' N/A '
    loFormSet.laDivArray[2,1] = 'ALL'
  ELSE
    DECLARE loFormSet.laDivArray [1 , 1] 
    loFormSet.laDivArray[1,1] = 'ALL'
  ENDIF  
  WITH loFormSet.Ariaform1
  .laDivArray.RowSource = 'Thisformset.laDivArray'
  ENDWITH 


  =lfClos_Fil('Seq_File')

  =lfOpen_Fil(lcCompDir+'SEQUENCE',lcCompDir+'Cseq_type','Seq_File','SH')  && Open Current Company Sequence file.

  IF loFormSet.llDivBased
    *-- Collect all division group data from sequence file. [begin]
    SELECT Seq_File
    SET RELATION TO cSeq_Type INTO (loFormSet.lcTemplate)
   lcTemplate = loFormSet.lcTemplate
    SCAN FOR !(&lcTemplate..lPerComp OR EOF(lcTemplate))

      IF EMPTY(cSeq_Group)  
 
        loFormSet.llEmptyGrp   = .T.
        lcSeq_Grp    = '001'

      ELSE

        lcSeq_Grp = PADR(cSeq_Group,3)

      ENDIF

      IF ASCAN(loFormSet.laDivArray,lcSeq_Grp,1)  = 0
        DECLARE loFormSet.laDivArray [ALEN(loFormSet.laDivArray,1) + 1 , 1] 
        loFormSet.laDivArray [ALEN(loFormSet.laDivArray,1) , 1] = lcSeq_Grp
      ENDIF

    ENDSCAN

    SET RELATION OFF INTO (loFormSet.lcTemplate)
    *-- Collect all division group data from sequence file. [end]
  ENDIF  

  llFindPrev = ALEN(loFormSet.laDivArray,1) > 2  && yes i find previous group.

  loFormSet.llNewGrp   = loFormSet.llDivBased OR !(llFindPrev OR loFormSet.llEmptyGrp)
  loFormSet.llNewMode  = loFormSet.llNewGrp

  lcNewBtn   = IIF(loFormSet.llNewGrp,'ENABLE','DISABLE')

ENDIF    && end if it's select company item.
*-- end of lfDivInfo.

*!*************************************************************
*! Name      : lfSelGroup
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/02/98
*! Purpose   : Select group1.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : lfFillData
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfSelGroup()
*!*************************************************************
*!
FUNCTION lfSelGroup
PARAMETERS loFormSet
*-- Default Select mode value.

loFormSet.lnGroup   = 1
loFormSet.Ariaform1.laDivArray.ListIndex = loFormSet.lnGroup

IF loFormSet.llDivBased
  loFormSet.Ariaform1.laDivArray.Enabled = .T.  &&lcDivStat = 'ENABLE'
  loFormSet.Ariaform1.laDivArray.Setfocus()
 
ELSE
  =lfvGroup(loFormset,loFormset.AriaForm1.laDivArray)
ENDIF  
*-- end of lfSelGroup.

*!*************************************************************
*! Name      : lfvGroup
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/02/98
*! Purpose   : Division group popup valid fn.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : lfFillData
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvGroup()
*!*************************************************************
*!
FUNCTION lfvGroup
PARAMETERS loFormSet,loFld

IF !loFormSet.llDivBased OR (loFld.Value <> loFld.OldValue)

   = lfFillData(loFormSet)
  loFormSet.llFromEdit = .F.
  loFormSet.ChangeMode('V')
ENDIF  
*-- end of lfvGroup.

*!*************************************************************
*! Name      : lfOpenFls
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/02/98
*! Purpose   : Open required files and set relation between them.
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfOpenFls()
*!*************************************************************
*!
FUNCTION lfOpenFls
PARAMETERS loFormSet

PRIVATE llFlFld,llFields,llFiles,lcFlFldOrd,lcFieldOrd,lcFilesOrd

STORE .F. TO llFlFld,llFields,llFiles          && Flags used in closing files.
STORE '' TO lcFlFldOrd,lcFieldOrd,lcFilesOrd   && variables used to set order of files.

IF USED('SYDFLFLD')
  lcFlFldOrd = ORDER('SYDFLFLD')
  SET ORDER TO TAG Cfld_name IN SYDFLFLD
ELSE
  llFlFld = gfOpenFile(oAriaApplication.SysPath+'SYDFLFLD',oAriaApplication.SysPath+'Cfld_name','SH')
ENDIF

IF USED('SYDFIELD')
  lcFieldOrd = ORDER('SYDFIELD')
  SET ORDER TO TAG Cfld_name IN SYDFIELD
ELSE
  llFields = gfOpenFile(oAriaApplication.SysPath+'SYDFIELD',oAriaApplication.SysPath+'Cfld_name','SH')
ENDIF

IF USED('SYDFILES')
  lcFilesOrd = ORDER('SYDFILES')
  SET ORDER TO TAG Cfile_nam IN SYDFILES
ELSE
  llFiles = gfOpenFile(oAriaApplication.SysPath+'SYDFILES',oAriaApplication.SysPath+'Cfile_nam','SH')
ENDIF

SELECT SYDFLFLD
SET RELATION TO cfld_name INTO Sydfield
SET RELATION TO cfile_nam INTO Sydfiles ADDITIVE

=lfFillTmpl(loFormSet)

SELECT SYDFLFLD
SET RELATION OFF INTO Sydfield
SET RELATION OFF INTO Sydfiles

IF llFlFld
  USE IN SYDFLFLD
ELSE
  SET ORDER TO (lcFlFldOrd) IN SYDFLFLD
ENDIF
  
IF llFields
  USE IN SYDFIELD
ELSE
  SET ORDER TO (lcFieldOrd) IN SYDFIELD
ENDIF
  
IF llFiles
  USE IN SYDFILES
ELSE
  SET ORDER TO (lcFilesOrd) IN SYDFILES
ENDIF
*-- end of lfOpenFls.

*!*************************************************************
*! Name      : lfFillTmpl
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/02/98
*! Purpose   : Fill template cursor template data.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfFillTmpl()
*!*************************************************************
*!
FUNCTION lfFillTmpl
PARAMETERS loFormSet

SELECT SYDFLFLD
SCAN FOR lEnumerate AND !EOF("SYDFIELD") .AND. !SEEK(PADR(cFld_Name,10) , loFormSet.lcTemplate)
  SCATTER MEMVAR
  m.cSeq_Type  = PADR(m.cFld_Name,10)
  m.nFld_Wdth  = SYDFIELD.nFld_Wdth
  m.cFld_Head  = IIF(cFld_Name = 'CPAYDOCNO','Bank Adjustments',SYDFIELD.cFld_Head)
  m.mFld_Des   = SYDFIELD.mFld_Des
  m.cData_Typ  = SYDFIELD.cData_Typ
  m.cFile_Tag  = SYDFILES.cFile_Tag
  m.lPerComp   = 'SEQPERCOMP' $ ALLTRIM(UPPER(SYDFIELD.mCodeInfo))
  m.lNoEdit    = 'DISABLESEQ' $ ALLTRIM(UPPER(SYDFIELD.mCodeInfo))
  m.cSeq_Group = IIF(m.lPerComp,'ALL','')
  m.mModExpr   = '.T.'
  
  STORE nDef_Seq TO m.nSeq_No,m.nOldSeq
  
  INSERT INTO (loFormSet.lcTemplate) FROM MEMVAR

ENDSCAN

*-- add hard coded records... [Begin]
DIMENSION laHardFlds[3,6]
laHardFlds[1,1] = 'CREDIT'
laHardFlds[2,1] = 'DEBIT'
laHardFlds[3,1] = 'CCLOSENT'

laHardFlds[1,2] = 'Credit Adjustments'
laHardFlds[2,2] = 'Debit Adjustments'
laHardFlds[3,2] = 'Closing Entry'

laHardFlds[1,3] = 6
laHardFlds[2,3] = 6
laHardFlds[3,3] = 8

laHardFlds[1,4] = 'C'
laHardFlds[2,4] = 'C'
laHardFlds[3,4] = 'C'

laHardFlds[1,5] = 'TF'
laHardFlds[2,5] = 'TF'
laHardFlds[3,5] = 'TF'

laHardFlds[1,6] = [('AR' $ loFormSet.lcCompMod) OR ('PS' $ loFormSet.lcCompMod)]
laHardFlds[2,6] = [('AR' $ loFormSet.lcCompMod) OR ('PS' $ loFormSet.lcCompMod)]
laHardFlds[3,6] = ['SM' $ loFormSet.lcCompMod]

FOR lnI = 1 TO ALEN(laHardFlds,1)

  STORE PADR(laHardFlds[lnI,1],10) TO m.cFld_Name,m.cSeq_Type
  m.cFld_Head  = laHardFlds[lnI,2]
  m.nFld_Wdth  = laHardFlds[lnI,3]
  m.cData_Typ  = laHardFlds[lnI,4]
  lcPerComp    = '.' + LEFT(laHardFlds[lnI,5],1) + '.'
  lcNoEdit     = '.' + RIGHT(laHardFlds[lnI,5],1) + '.'
  m.lPerComp   = EVALUATE(lcPerComp)
  m.lNoEdit    = EVALUATE(lcNoEdit)
  m.cSeq_Group = IIF(m.lPerComp,'ALL','')
  m.mModExpr   = laHardFlds[lnI,6]

  INSERT INTO (loFormSet.lcTemplate) FROM MEMVAR

ENDFOR
*-- add hard coded records... [End]
*-- end of lfFillTmpl.

*!*************************************************************
*! Name      : lfFillData
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/02/98
*! Purpose   : Fill temp. file with selected company division data.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfFillData()
*!*************************************************************
*!
FUNCTION lfFillData
PARAMETERS loFormSet,llAddMode

PRIVATE lcGrpCode

lnSelRec = 0

lcGrpCode = IIF(!loFormSet.llDivBased OR ('ALL' $ loFormSet.laDivArray[loFormSet.lnGroup,1]) ,'   ' , loFormSet.laDivArray[loFormSet.lnGroup,1])

*-- If you find this selection data before.
IF SEEK(loFormSet.lcCompCode+loFormSet.laDivArray[loFormSet.lnGroup,1],loFormSet.lcTmpSeq)

  *-- Only adjust sequences if another user change it.  
  SELECT (loFormSet.lcTmpSeq)
  SCAN REST FOR cComp_Id + cSeq_Group + cFld_Name = loFormSet.lcCompCode + loFormSet.laDivArray[loFormSet.lnGroup,1]

    lnSelRec = lnSelRec + 1
    IF SEEK(cFld_Name+IIF(loFormSet.llEmptyGrp,'  ',lcGrpCode),'Seq_File')
    
      REPLACE nSeq_No WITH Seq_File.nSeq_No ,;
              nOldSeq WITH Seq_File.nSeq_No
    
    ENDIF
  
  ENDSCAN

ELSE  && else first time you enter selection locations.

  IF USED('Seq_File')
    PRIVATE lcForExp

    IF loFormSet.llDivBased
      lcForExp = IIF(EMPTY(lcGrpCode),'lPerComp','!lPerComp') + ' AND EVALUATE(ALLTRIM(mModExpr))'
    ELSE
      lcForExp = 'EVALUATE(ALLTRIM(mModExpr))'
    ENDIF  

    SELECT (loFormSet.lcTemplate)
    SET FILTER TO 
    LOCATE 
    SCAN FOR &lcForExp

      lnSelRec = lnSelRec + 1

      SCATTER MEMVAR MEMO
      m.cComp_Id   = loFormSet.lcCompCode
      m.cSeq_Group = loFormSet.laDivArray[loFormSet.lnGroup,1]
      m.cSeq_chr=chr(0)
      IF !llAddMode AND SEEK(cFld_Name+IIF(loFormSet.llEmptyGrp,'  ',lcGrpCode),'Seq_File') 
        STORE Seq_File.nSeq_No TO m.nSeq_No,m.nOldSeq
        m.nSeqRec = RECNO('Seq_File')
      ENDIF

      INSERT INTO (loFormSet.lcTmpSeq) FROM MEMVAR
    ENDSCAN
    
    SET FILTER TO cComp_Id + cSeq_Group + cFld_Name = loFormSet.Ariaform1.laCompany.Value + loFormSet.laDivArray[loFormSet.lnGroup,1]
    GO BOTTOM
    lnBottom = RECNO(loFormSet.lcTmpSeq)
  ENDIF
ENDIF

SELECT (loFormSet.lcTmpSeq)
LOCATE
DELETE FOR EMPTY(cComp_ID)
LOCATE


lnTop = RECNO(loFormSet.lcTmpSeq) 

KEYBOARD '{ALT+B}' CLEAR PLAIN  && Top screen is browse screen.
*-- end of lfFillData.


*!*************************************************************
*! Name      : lpSavScr
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/02/98
*! Purpose   : Transfer sequence data to sequence file.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : gfAdd_Info
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpSavScr
*!*************************************************************
*!
PROCEDURE lpSavScr
PARAMETERS loFormSet

PRIVATE lcGrpCode,lcCurrCode
lcGrpCode = IIF('ALL' $ loFormSet.laDivArray[loFormSet.lnGroup,1] ,'   ' , loFormSet.laDivArray[loFormSet.lnGroup,1])
lcTmpSeq = loFormSet.lcTmpSeq
lcCurrCode = &lcTmpSeq..cComp_Id + &lcTmpSeq..cSeq_Group + &lcTmpSeq..cFld_Name
 
= SEEK(loFormSet.lcCompCode+loFormSet.laDivArray[loFormSet.lnGroup,1],loFormSet.lcTmpSeq) 

*-- Scan current group in sequence file.  
SELECT (loFormSet.lcTmpSeq)

SCAN REST FOR cComp_Id + cSeq_Group + cFld_Name = loFormSet.lcCompCode + loFormSet.laDivArray[loFormSet.lnGroup,1]

  *-- Modify exist record.
  IF SEEK(cFld_Name+IIF(loFormSet.llEmptyGrp,'  ',lcGrpCode),'Seq_File')
    SELECT Seq_File

    =RLOCK()
    REPLACE cSeq_Group WITH lcGrpCode ,;
            nSeq_No WITH &lcTmpSeq..nSeq_No
    UNLOCK
    
  ELSE  && New Record.
    
    SCATTER MEMVAR
    m.cSeq_Group = lcGrpCode
    INSERT INTO ('SEQ_FILE') FROM MEMVAR
    
  ENDIF

  SELECT Seq_File
  *-- update user information.
  
  =gfAdd_Info('SEQ_FILE')
  SELECT (lcTmpSeq)

ENDSCAN

= SEEK(lcCurrCode,lcTmpSeq) 

loFormSet.llEmptyGrp = .F.
llFindPrev = .T.
loFormSet.llNewMode  = .F.
*-- end of lpSavScr.

*!*************************************************************
*! Name      : lpDelScr
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/02/98
*! Purpose   : Delete current group data from sequence file.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpDelScr
*!*************************************************************
*!
FUNCTION lpDelScr
PRIVATE llDelIt,lcAlias,lcGrpCode
lcAlias = SELECT(0)
llDelIt = .T.

lcGrpCode = IIF(loFormSet.llEmptyGrp,'   ',loFormSet.laDivArray[loFormSet.lnGroup,1])

IF loFormSet.llEmptyGrp

  loFormSet.llEmptyGrp = .F.

ELSE

  =lfOpen_Fil(lcCompDir+'CODES',lcCompDir+'Idrltfname','Code_File','SH')  && Open Current Company Codes file.
  IF SEEK('N'+'Y'+'CDIVISION','Code_File')
    SELECT Code_File
    LOCATE REST FOR (CRLTD_NAM = 'DIVGROUP  ') AND (ALLTRIM(loFormSet.laDivArray[loFormSet.lnGroup,1]) $ cRltd_Vlu)
    IF FOUND()
      llDelIt = .F.
      *-- Message : One or more Division(s) have been assigned to this group. Cannot remove.
      *--                                     <Ok>
      =gfModalGen('INM00338B00000','DIALOG')
    ENDIF
  ENDIF

ENDIF


*-- Delete current division group.
IF llDelIt
  
  *-- Delete data from sequence file.
  SELECT Seq_File
  BLANK FOR cSeq_Group = ALLTRIM(lcGrpCode)
  DELETE FOR EMPTY(cSeq_Type)
  GO TOP IN Seq_File

  *-- delete data from temp. sequence file.
  SELECT (loFormSet.lcTmpSeq)
  BLANK FOR cComp_Id + cSeq_Group + cFld_Name = loFormSet.lcCompCode + ALLTRIM(loFormSet.laDivArray[loFormSet.lnGroup,1])
  
  *-- remove array element
  = ADEL(loFormSet.laDivArray,loFormSet.lnGroup)
  DIMENSION loFormSet.laDivArray[ALEN(loFormSet.laDivArray,1)-1,1]

  loFormSet.lnGroup = 1
  loFormSet.Ariaform1.laDivArray.ListIndex = loFormSet.lnGroup
  IF ALEN(loFormSet.laCompany,1) = 1
    loFormSet.lcDivStat = 'ENABLE'
  ENDIF  
  loFormSet.Ariaform1.laDivArray.Enabled = loFormSet.lcDivStat = 'ENABLE'
  
  IF ALEN(loFormSet.laDivArray,1) = 2
    loFormSet.llNewGrp  = .T.
    loFormSet.llNewMode = loFormSet.llNewGrp
  ENDIF

  loFormSet.ChangeMode('S')

ENDIF

=lfClos_Fil('Code_File')

SELECT (lcAlias)
*-- end of lpDelScr.

*!*************************************************************
*! Name      : lfOpen_Fil
*! Developer : Mohamed Badran (MAB)
*! Date      : 03/01/99
*! Purpose   : Open Files in another alias name
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfOpen_Fil()
*!*************************************************************
*!
FUNCTION lfOpen_Fil
PARAMETERS lcFileNam,lcTagNam,lcAliasNam,lcOpnStat
*-- Open file in another alias name.

=gfOpenFile(lcFileNam,lcTagNam,lcOpnStat,lcAliasNam)

*-- end of lfOpen_Fil.

*!*************************************************************
*! Name      : lfClos_Fil
*! Developer : Mohamed Badran (MAB)
*! Date      : 03/01/99
*! Purpose   : Close files oppened in another alias name.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfClos_Fil()
*!*************************************************************
*!
FUNCTION lfClos_Fil
PARAMETERS lcFileName
IF USED(lcFileName)
  USE IN (lcFileName)
ENDIF 
*-- end of lfClos_Fil.