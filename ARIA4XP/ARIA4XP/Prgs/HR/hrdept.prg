*:************************************************************************
*:  Program File: ARIA4XP\PRGS\HR\HRDEPT.FXP
*:  Module      : PIECE WORK
*:  Desc.       : Department Screen
*:  System      : Aria 4XP
*:  Developer   : HES - Hesham Elmasry
*:  Date        : 05/03/2012
*:  Reference   : E303152
*:************************************************************************
* Documentation:
*!* E303166, Add more validation [PW Project]
**************************************************************************
lcProg = JUSTSTEM(SYS(16))

ON ERROR
*- Call the screen
lcRunScx = lfGetScx("HR\&lcProg..scx")
DO FORM (lcRunScx)

************************************************************
*! Name      : lfGetScx
*! Developer : HES - Hesham Elmasry
*! Date      : 05/03/2012
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
  *! Developer : HES - Hesham Elmasry
  *! Date      : 05/03/2012
  *! Purpose   : called from the Screen init Method
  *!*************************************************************
FUNCTION lfFormInit
  PARAMETERS loFormSet

  lcProg = JUSTSTEM(SYS(16))
  loFormSet.ADDPROPERTY('lcProgName',lcProg)
  loFormSet.ADDPROPERTY('CalendID',0)
  loFormSet.ADDPROPERTY('BrowWkCntr',gfTempName())

  lcPath = oAriaApplication.ApplicationHome

  *- Open tables
  =lfOpenPRGFILES(loFormSet.lcProgName)

  *** Load program base file
  loFormSet.ADDPROPERTY('lcBaseFile',ALLTRIM(sydObjct.cBaseFile))

  *- initializations
  WITH loFormSet
    .cbrowsetabledbengine   = "SQL"
    .nWorkArea                            = .lcBaseFile
    .otoolbar.nWorkArea                   = .lcBaseFile
    .DATAENVIRONMENT.INITIALSELECTEDALIAS = .lcBaseFile
    .cBrowseFileName        = .lcBaseFile
    .cBrowseIndexExpression = "CDEPTID"
    .cBrowseIndexFields     = "CDEPTID"
    .cBrowseIndexName       = "PEDEPART"
    .cBrowseAliasName       = .lcBaseFile
    .cBrowseTableName       = .lcBaseFile
    .cBrowseFilter          = ""
    .BrowseTitle 		  	  = ALLTRIM(sydObjct.CPRGLNAME)
    .AriaBrFields.edtBrowseFields.VALUE = "CDEPTID :H='Department ID',CDEPTNAME :H='Department Name'"
  ENDWITH

  *- set the input mask of the keyfield
  WITH loFormSet
    .Ariaform1.AriaKeyField1.KeyTextbox.INPUTMASK = REPLICATE('!',FSIZE(.cBrowseIndexFields,.lcBaseFile))
    .Ariaform1.TXTName.INPUTMASK = REPLICATE('X',FSIZE('CDEPTNAME',.lcBaseFile))
  ENDWITH

  loFormSet.HasMemo = .F.

  loFormSet.ChangeMode('S')
  *- End of lfFormInit.


  ************************************************************
  *! Name      : lfFormActivate
  *! Developer : HES - Hesham Elmasry
  *! Date      : 05/03/2012
  *! Purpose   : Form Activate
  ************************************************************
FUNCTION lfFormActivate
  PARAMETERS loFormSet

  *- End of lfFormActivate.

  ************************************************************
  *! Name      : lfChangeMode
  *! Developer : HES - Hesham Elmasry
  *! Date      : 04/24/2012
  *! Purpose   : Change Mode
  ************************************************************
FUNCTION lfChangeMode
  PARAMETERS loFormSet
  LOCAL lnSlct
  lnSlct = SELECT(0)

  IF TYPE('loFormSet.lcBaseFile')='U'
    RETURN
  ENDIF

  loFormSet.Ariaform1.AriaKeyField1.ENABLED = .F.

  DO CASE
    CASE loFormSet.ActiveMode = 'S'
      WITH loFormSet.Ariaform1
        .AriaKeyField1.ENABLED = .T.
        .AriaKeyField1.KeyTextbox.SETFOCUS()
      ENDWITH 

    CASE loFormSet.ActiveMode = 'V'
      WITH loFormSet.Ariaform1
        SELECT PEDEPART
        .AriaKeyField1.KeyTextbox.VALUE = CDEPTID
        .TXTName.VALUE = CDEPTNAME
      ENDWITH

      loFormSet.CheckNavigation()
      loFormSet.otoolbar.Navrefresh()

    CASE loFormSet.ActiveMode = 'A'
    
    CASE loFormSet.ActiveMode = 'E'
    
  ENDCASE
  SELECT (lnSlct)

  *- End of lfChangeMode.

  ************************************************************
  *! Name      : lfFormSavefiles
  *! Developer : HES - Hesham Elmasry
  *! Date      : 05/03/2012
  *! Purpose   : Save process
  ************************************************************
FUNCTION lfFormSavefiles
  PARAMETERS loFormSet
  LOCAL lnSlct
  lnSlct = SELECT(0)

  SELECT (loFormSet.lcBaseFile)
  IF loFormSet.ActiveMode = 'A'
    APPEND BLANK
  ENDIF

  gfAdd_Info(loFormSet.lcBaseFile)
  WITH loFormSet.Ariaform1
    REPLACE CDEPTID WITH loFormSet.Ariaform1.AriaKeyField1.KeyTextbox.VALUE ;
      CDEPTNAME  WITH  loFormSet.Ariaform1.TXTName.VALUE
  ENDWITH

  SELECT (loFormSet.lcBaseFile)
  =gfReplace('')
  =gfTableUpdate()

  SELECT (lnSlct)
  *- End of lfFormSavefiles.

  ************************************************************
  *! Name      : lfFormDelete
  *! Developer : HES - Hesham Elmasry
  *! Date      : 05/03/2012
  *! Purpose   : lfFormDelete
  ************************************************************
FUNCTION lfFormDelete
  PARAMETERS loFormSet
  LOCAL lnSlct
  lnSlct = SELECT(0)

   
  
  dimension laDBFFiles[1]
  dimension laERPFiles[1] 
  if !used('SYDFLFLD') 
    use oAriaApplication.SysPath+'SYDFLFLD' in 0 shared
  endif 
  
  select cfile_nam from SYDFLFLD where cfld_Name = loFormSet.cBrowseIndexExpression into array laDBFFiles  
  use in sydflfld
  
  =gfOpenTable(oAriaApplication.SysPath+'SYCINST','','SH')
  lcA4SysDir = ADDBS(SYCINST.cA4SysDir)
  use lcA4SysDir +'SYDFLFLD' in 0 shared
  select cfile_nam from SYDFLFLD where cfld_Name = loFormSet.cBrowseIndexExpression into array laSQLFiles
  use in sydflfld

  lnDBFCnt = iif(type('laDBFFiles') <> 'L',ALEN(laDBFFiles,1),0)
  lnSQLCnt = iif(type('laSQLFiles') <> 'L',ALEN(laSQLFiles,1),0)
  
  lcKey = loFormSet.cBrowseIndexExpression
  lcBaseFile = loFormSet.lcBaseFile
  llFound = .F.
  lcTable = ''
  FOR lnX = 1 to lnDBFCnt 
    if !used('alltrim(laSQLFiles[1,lnX])')
      =gfOpenTable(alltrim(laSQLFiles[1,lnX]),alltrim(laSQLFiles[1,lnX]),'SH')
    endif     
    if !(alltrim(laDBFFiles[1,lnX]) == alltrim(lcBaseFile))
      select alltrim(laDBFFiles[1,lnX])
      gfSeek('')
      locate for &lcKey = eval(lcbasefile+'.&lcKey')
      if found()
        llFound = .T.
        lcTable = alltrim(laDBFFiles[1,lnX])
        exit
      endif       
    endif 
  ENDFOR
  
  FOR lnX = 1 to lnSQLCnt 
    if !(alltrim(laSQLFiles[1,lnX]) == alltrim(lcBaseFile))
      if !used('alltrim(laSQLFiles[1,lnX])')
        =gfOpenTable(alltrim(laSQLFiles[1,lnX]),alltrim(laSQLFiles[1,lnX]),'SH')
      endif 
      select alltrim(laSQLFiles[1,lnX])
      gfSeek('')
      locate for &lcKey = eval(lcbasefile+'.&lcKey')
      if found()
        llFound = .T.
        lcTable = alltrim(laSQLFiles[1,lnX])
        exit        
      endif 
    endif 
  ENDFOR  
  
  if !llFound 
    SELECT (loFormSet.lcBaseFile)
    gfDelete()
    gfTableUpdate()
  else 
    wait windows "This entity has a reference in "+alltrim(lcTable )+ " table, can't delete it."
    return .F.
  endif 

  SELECT(lnSlct)
  *- End of lfFormDelete.

  *!* E303166, Add more validation [Start]
  ************************************************************
  *! Name      : lfFormBeforeSave
  *! Developer : HES - Hesham Elmasry
  *! Date      : 05/03/2012
  *! Purpose   : Before Save
  ************************************************************
FUNCTION lfFormBeforeSave
  PARAMETERS loFormSet
  
  IF empty(alltrim(loFormSet.Ariaform1.txtName.VALUE))
    wait window "You must enter a department name.."
    loFormSet.Ariaform1.txtName.setfocus()
    return .F.
  endif 
  
  RETURN .T.

  *- End of lfFormBeforeSave.
  *!* E303166, Add more validation [End  ]  
  
  ************************************************************
  *! Name      : lfvKeyField
  *! Developer : HES - Hesham Elmasry
  *! Date      : 05/03/2012
  *! Purpose   : Valid function for Machine_ID
  ************************************************************
FUNCTION lfvKeyField
  PARAMETERS loFormSet,loFld
  LOCAL lnSlct
  lnSlct = SELECT(0)

  WITH loFld.KeyTextbox
    .VALUE     = ALLTRIM(.VALUE)
    .VALUE     = PADR(.VALUE,LEN(.INPUTMASK))
  ENDWITH
  lcFile_Ttl = loFormSet.BrowseTitle
  lcBrFields = loFormSet.AriaBrFields.edtBrowseFields.VALUE
  llView = .F.
  lcBaseFile = loFormSet.lcBaseFile
  llBrowse = loFld.Selectedfrombrowse

  SELECT (lcBaseFile)
  IF llBrowse .OR. !gfSeek(loFld.KeyTextbox.VALUE) .OR. ATC("?",loFld.KeyTextbox.VALUE) > 0
    IF llBrowse .OR. ATC("?",loFld.KeyTextbox.VALUE) > 0
      IF loFormSet.otoolbar.cmdFind.CLICK()
        llView = .T.
      ELSE
        loFld.KeyTextbox.VALUE = loFld.KeyTextbox.OldValue
      ENDIF
    ELSE
      lnOption  = gfModalGen('QRM00001B00001','Dialog',;
        +ALLTRIM(loFld.KeyTextbox.VALUE))

      DO CASE
        CASE lnOption = 1
          IF loFormSet.otoolbar.cmdFind.CLICK()
            llView = .T.
          ELSE
            loFld.KeyTextbox.VALUE = loFld.KeyTextbox.OldValue
          ENDIF
        CASE lnOption = 2
          loFormSet.ChangeMode('A')
          RETURN

        CASE lnOption = 3
          loFld.KeyTextbox.VALUE = loFld.KeyTextbox.OldValue
          RETURN .F.
      ENDCASE
    ENDIF
  ELSE
    loFld.KeyTextbox.VALUE = &lcBaseFile..CDEPTID
    llView = .T.
  ENDIF

  IF llView = .T.
    loFormSet.ChangeMode('V')
    =lfChkNav()
  ENDIF

  SELECT (lnSlct)
  *- End of lfvPlant.

  ************************************************************
  *! Name      : lfChkNav
  *! Developer : HES - Hesham Elmasry
  *! Date      : 05/03/2012
  *! Purpose   : lfChkNav
  ************************************************************
FUNCTION lfChkNav

  LOCAL cNativeDBID
  cNativeDBID = oAriaApplication.cNativeDBID
  oAriaApplication.cNativeDBID = 'SQL'
  =gfSeek('',loFormSet.lcBaseFile)
  SELECT (loFormSet.lcBaseFile)
  =SEEK(loFld.KeyTextbox.VALUE)
  loFormSet.CheckNavigation()
  loFormSet.otoolbar.Navrefresh()
  oAriaApplication.cNativeDBID = cNativeDBID

  *- End of lfChkNav.

  ************************************************************
  *! Name      : lfOpenPRGFILES
  *! Developer : HES - Hesham Elmasry
  *! Date      : 02/15/2012
  *! Purpose   : Open tables
  ************************************************************
FUNCTION lfOpenPRGFILES
  LPARAMETERS lcProg

  LOCAL lcPath,i
  lcPath = ''
  lcProg = PADR(UPPER(lcProg),10)
  SET MULTILOCKS ON
  lnRemResult = oAriaApplication.RemoteSystemData.Execute;
    ("Select * from SYDOBJCT WHERE CAPOBJNAM= '&lcProg'",'',"SYDOBJCT","",oAriaApplication.cAria4SysFiles,3,"",SET("Datasession"))

  =gfOpenTable(oAriaApplication.SysPath+'sydFiles','CFILE_NAM','SH')   && CFILE_NAM
  =gfOpenTable(oAriaApplication.SysPath+'sydField','CFLD_NAME','SH')   && CFLD_NAME

  lcFiles = sydObjct.MPRGFILES
  DIME laFiles[2],laTbl[1],laIndx[1]
  STORE '' TO laFiles,laTbl,laIndx
  =gfSubStr(lcFiles,@laFiles,'|')
  =gfSubStr(laFiles[1],@laTbl,',')
  =gfSubStr(laFiles[2],@laIndx,',')
  FOR i = 1 TO ALEN(laTbl)
    =gfOpenTable(laTbl[i],laIndx[i],'SH')
  ENDFOR

  *** Load program base file
  lcBaseFile = ALLTRIM(sydObjct.cBaseFile)

  *- End of lfOpen.
