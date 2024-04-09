*:************************************************************************
*:  Program File: \ARIA4XP\PRGS\SM\SMFSYER.Prg
*:  Module      : System Manager 
*:  Desc.       : Fiscal Year Screen
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 02/10/2013 
*:  Reference   : *E303350,1
*:************************************************************************
PARAMETERS pcComp_ID

*- Get the screen , call it 
lcRunScx = lfGetScx("SM\SMFSYER.scx")
IF !EMPTY(pcComp_ID)
  PRIVATE oScr
  DO FORM (lcRunScx) WITH pcComp_ID NAME oScr NOSHOW 
  IF TYPE('oScr')='O' AND !ISNULL(oScr)
    oScr.Show(1)
  ENDIF
ELSE 
  DO FORM (lcRunScx) WITH pcComp_ID
ENDIF   

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

loFormSet.AddProperty('lcProgName','SMFSYER')

*- Set functions to the APMAIN.FXP
lcPath = oAriaapplication.ApplicationHome
SET PROCEDURE TO (lcPath+'AP\APMAIN.FXP') ADDITIVE 
SET PROCEDURE TO (lcPath+'GL\GL.FXP') ADDITIVE 

*- Open tables 
=lfOpenPRGFILES(loFormSet.lcProgName)

*** Load program base file 
=lfAddProp(loFormSet,'lcBaseFile',ALLTRIM(sydObjct.cBaseFile))

*- just disable the ctrl+E definition by th following line
ON KEY LABEL CTRL+E wait window nowait ''

WITH loFormSet
  .cbrowsetabledbengine   = "NATIVE"
  .nWorkArea                            = .lcBaseFile 
  .otoolbar.nWorkArea                   = .lcBaseFile
  .DataEnvironment.InitialSelectedAlias = .lcBaseFile
  .cBrowseFileName        = .lcBaseFile
  .cBrowseIndexExpression = "COMPFYEAR"
  .cBrowseIndexFields     = "COMPFYEAR"
  .cBrowseIndexName       = "CFISFYEAR"
  .cBrowseAliasName       = .lcBaseFile
  .cBrowseTableName       = .lcBaseFile
  .cBrowseFilter          = ""
  *.BrowseTitle            = LANG_COMPANY_INFORMATION && Company Information

  *.lcFile_Ttl = LANG_COMPANY_INFORMATION
  *.ariaBrFields.edtBrowseFields.Value = .GetBrowseFields(.lcBaseFile)
ENDWITH

lfDefineVars(loFormSet)

IF USED('SYCCOMP')
  USE IN SYCCOMP
ENDIF 

IF EMPTY(loFormSet.pcComp_ID)
  =gfopenfile(oAriaApplication.SysPath+'SYCCOMP','CCOMP_ID')
   *laCtrStat[10] = 'DISABLE'
  *** See if there is any companies available in the system. ***
  SELECT SYCCOMP
  SET FILTER TO EMPTY(syccomp.ccompprnt)
  GO TOP
  IF EOF()
    *** No companies available. ***
    *** <  Ok  > ***
    =gfModalGen("TRM00189B00000","DIALOG","companies")
    RETURN .F.
  ENDIF
ENDIF

*** To know if the program called from the menu ***
*** or called from another program
SELECT SYCCOMP.cComp_Id+" "+SYCCOMP.cCom_Name ;
      FROM (oAriaApplication.SysPath+'SYCCOMP');
      INTO ARRAY loFormSet.laCompany


*- Initialize screen fields with blank
=gfOpenTable(oAriaApplication.DataDir+'FISHD','COMPFYEAR','SH')
SELECT FISHD
lcScFields = loFormSet.lcScFields
SCATTER FIELDS &lcScFields MEMO TO loFormSet.laData BLANK 

*- Set ControlSource      
WITH loFormSet.Ariaform1
  .laData2.ControlSource = 'Thisformset.laData[2]'
  .laData4.ControlSource = 'Thisformset.laData[4]'
  .laData5.ControlSource = 'Thisformset.laData[5]'
  .laData6.ControlSource = 'Thisformset.laData[6]'
  .laData7.ControlSource = 'Thisformset.laData[7]'
  .laData8.ControlSource = 'Thisformset.laData[8]'
  .laData9.ControlSource = 'Thisformset.laData[9]'
      
  .puComp.RowSource = 'Thisformset.laCompany'
  .puComp.ControlSource = 'Thisformset.puComp'
  .puComp.DisplayValue = "Select Company"
  
  .cbDay_1.ControlSource = 'Thisformset.cbDay_1'
  .cbDay_2.ControlSource = 'Thisformset.cbDay_2'
  .cbDay_3.ControlSource = 'Thisformset.cbDay_3'
  .cbDay_4.ControlSource = 'Thisformset.cbDay_4'
  .cbDay_5.ControlSource = 'Thisformset.cbDay_5'
  .cbDay_6.ControlSource = 'Thisformset.cbDay_6'
  .cbDay_7.ControlSource = 'Thisformset.cbDay_7'

  .laData8.MaxLength = LEN(loFormSet.laData[8])
  .laData9.MaxLength = LEN(loFormSet.laData[9])
ENDWITH 
 
loFormSet.lcCursor  = gfTempName()
loFormSet.lc_TempHd = gfTempName()    && Temporary file of the holiday
loFormSet.lc_TempPR = gfTempName()    && Temporary file of the periods

CREATE CURSOR (loFormSet.lcCursor) (mMisc M (10))
APPEND BLANK

IF !EMPTY(loFormSet.pcComp_ID)
  loFormSet.lcComp_ID  = LEFT(loFormSet.pcComp_ID,2)
  IF SEEK(loFormSet.lcComp_ID,"SYCCOMP")
    loFormSet.puComp = SYCCOMP.cComp_Id+" "+SYCCOMP.cCom_Name
    loFormSet.Ariaform1.Refresh()
    =lfOpenCmpData(loFormSet)
  ENDIF
  
  loFormSet.llComIdFlg = .T.
ELSE
  loFormset.puComp = ' '
  loFormSet.Ariaform1.Refresh()
  loFormSet.Ariaform1.puComp.DisplayValue = "Select Company"
  loFormSet.llComIdFlg = .F.
ENDIF  
loFormSet.lcBaseFile = 'FISHD'

IF EMPTY(loFormSet.pcComp_ID)
  SELECT SYCCOMP
  SET FILTER TO EMPTY(syccomp.ccompprnt)
ENDIF

loFormSet.ChangeMode('S')

************************************************************
*! Name      : lfFormdestroy 
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 02/10/2013
*! Purpose   : unloading the form
************************************************************ 
FUNCTION lfFormdestroy 
PARAMETERS loFormSet

lcCursor = loFormSet.lcCursor
lc_TempHd = loFormSet.lc_TempHd
lc_TempPr = loFormSet.lc_TempPr
gcWorkDir = oAriaApplication.WorkDir

IF USED(lcCursor)
  USE IN ALIAS(lcCursor)
ENDIF

IF USED(lc_TempHd)
  USE IN ALIAS(lc_TempHd)
ENDIF
  
ERASE (gcWorkDir+lc_TempHd+".DBF")
ERASE (gcWorkDir+lc_TempHd+".FPT")
ERASE (gcWorkDir+lc_TempHd+".CDX")

IF USED(lc_TempPr)
  USE IN ALIAS(lc_TempPr)
ENDIF
ERASE (gcWorkDir+lc_TempPr+".DBF")
ERASE (gcWorkDir+lc_TempPr+".FPT")
ERASE (gcWorkDir+lc_TempPr+".CDX")
IF USED('FISHD')
  USE IN FISHD
ENDIF
  
IF USED('FSHLD')
  USE IN FSHLD
ENDIF
  
IF USED('FSPRD')
  USE IN FSPRD
ENDIF

*- End of lfFormdestroy .

************************************************************
*! Name      : lfDefineVars
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 02/10/2013
*! Purpose   : Define needed variables in the screen
************************************************************
FUNCTION lfDefineVars
PARAMETERS loFormSet

lcScFields = 'COWNER,CFISFYEAR,CFISYSTAT,CFISNOPRD,SYCCOMP.CCURR_PRD,DFISBGDAT,DFISENDAT,CFISSHEAD,CFISLHEAD,CFISNONWD,MFISCOMNT'
loFormSet.AddProperty('lcScFields',lcScFields)
lcCnt = ALLTRIM(STR(OCCURS(',',lcScFields)+1))
loFormSet.AddProperty('laData[&lcCnt.]')

loFormSet.AddProperty('laBlanks[1,1]')
loFormSet.AddProperty('laComp[1,1]')
loFormSet.AddProperty('laTStat[4,1]')
loFormSet.AddProperty('laCompInfo[3,1]')
loFormSet.AddProperty('laTYears[4]')
loFormSet.AddProperty('laHolDates[1,1]')
loFormSet.AddProperty('laCompany[1]')

loFormSet.laTStat[1,1] = "National"
loFormSet.laTStat[2,1] = "State"
loFormSet.laTStat[3,1] = "Company"
loFormSet.laTStat[4,1] = "Local"

loFormSet.laTYears[1] = "Previous"
loFormSet.laTYears[2] = "Current"
loFormSet.laTYears[3] = "Next"
loFormSet.laTYears[4] = "History"

loFormSet.AddProperty('lcDataDir'  , '' )      && Var to hold the company data path
loFormSet.AddProperty('lcComID'    , ''   )    && Var to accept company ID in Copy Dialog
loFormSet.AddProperty('lcCmFisYer' , ''   )    && Var to accept Fiscal year in Copy Dialog
loFormSet.AddProperty('lcOldComID' , ''   )    && Var to keep old ID before Copy from another comp.
loFormSet.AddProperty('lcComNam'   , ''   )    && Var to accept company name

loFormSet.AddProperty('lcOldStrt'  , {}   )    && Var to keep old value of Start date
loFormSet.AddProperty('lcOldEnd'   , {}   )    && Var to keep old value of End date

loFormSet.AddProperty('lcYer'      , ''   )    && Var to hold text in one of the messages
loFormSet.AddProperty('lcFldNm'    , ''   )    && Var to hold text in one of the messages

loFormSet.AddProperty('lcCursor'   , ''   )     
loFormSet.AddProperty('lc_TempHd'  , ''   )    && Var to hold name of Holidays temp. file
loFormSet.AddProperty('lc_TempPR'  , ''   )    && Var to hold name of Periods temp. file
loFormSet.AddProperty('lcIndExpHd' , ''   )    && Var to hold index expression of Holidays file
loFormSet.AddProperty('lcIndTagHd' , ''   )    && Var to hold tag name of Holidays file

loFormSet.AddProperty('llUpdHd'    , .F.  )    && Flag to know is there any modification in Holidays
loFormSet.AddProperty('llUpdPr'    , .F.  )    && Flag to know is there any modification in Periods

loFormSet.AddProperty('lnFcount'   , 0    )    && Var to keep no. of Holidays' fields 
loFormSet.AddProperty('lcCompFis'  , ''   )    && Var to hold Key fields for Periods file
loFormSet.AddProperty('lcOldHold'  , ''   )    && Var to hold Key fields for Holidays file
loFormSet.AddProperty('llSavFlg'   , .F.  )       
loFormSet.AddProperty('llOldFlg'   , .F.  )    && Flag to know that return cancel from lfAcptPrd

loFormSet.AddProperty('llNewPrds' , .F.  )     && Flag to be used if creat periods for 
                      && first time or when chang periods or dates.
loFormSet.AddProperty('llDefPrds' , .F.  )     && Flag to be used if creat default 
                      && periods is required.
loFormSet.AddProperty('llAcptPrd' , .F.  )     && Flag to tell if the periods of the year 
                      && is accepted before for the same record.

loFormSet.AddProperty('llEndScr'  , .F.  )     && Flag to know if finish & close this screen or not
loFormSet.AddProperty('llFrsPop'  , .T.  )         

loFormSet.AddProperty('lcPopText'  , " "  )    
loFormSet.AddProperty('puComp'     , "Select Company"     )
loFormSet.AddProperty('puComp_Id'  , " "  )   
loFormSet.AddProperty('lcComp'     , " "  )   
loFormSet.AddProperty('lcComp_Id'  , " "  )   

loFormSet.AddProperty('cbDay_1' , 0    )  
loFormSet.AddProperty('cbDay_2' , 0    )  
loFormSet.AddProperty('cbDay_3' , 0    )  
loFormSet.AddProperty('cbDay_4' , 0    )  
loFormSet.AddProperty('cbDay_5' , 0    )  
loFormSet.AddProperty('cbDay_6' , 0    )  
loFormSet.AddProperty('cbDay_7' , 0    )  

loFormSet.AddProperty('llComIdFlg')
 
*!**************************************************************************
*!
*!      Procedure: lpShow
*!
*!**************************************************************************
*
PROCEDURE lpShow
PARAMETERS loFormSet

IF TYPE('loFormSet.lcProgName')='U'
  RETURN 
ENDIF 

lcCursor = loFormSet.lcCursor
lc_TempHd = loFormSet.lc_TempHd
lc_TempPr = loFormSet.lc_TempPr
gcWorkDir = oAriaApplication.WorkDir

IF loFormSet.ActiveMode <> 'S'
  loFormSet.Ariaform1.puComp.Value = SYCCOMP.cComp_Id+" "+SYCCOMP.cCom_name
ENDIF 

*** If the program called from another program ***
IF loFormSet.llComIdFlg
  loFormSet.llComIdFlg = .F.
  loFormSet.llSavFlg   = .T.
  IF SEEK(loFormSet.lcComp_ID,"SYCCOMP")
    loFormSet.puComp = SYCCOMP.cComp_Id+" "+SYCCOMP.cCom_Name
    loFormSet.Ariaform1.Refresh()
    loFormSet.Ariaform1.puComp.Enabled = .F.
  ENDIF
  loFormSet.Ariaform1.lcFisDes.Value   = loFormSet.laTYears[2]
  loFormSet.Ariaform1.laData2.Enabled = .T.
ENDIF

*** Set the non working days for each company ***
FOR lnCount = 1 TO 7
  lcObjName  = "cbDay_"+ALLTRIM(STR(lnCount))
  loFormSet.&lcObjName = IIF(ALLTRIM(STR(lnCount)) $ loFormSet.laData[10],1,0)
ENDFOR

DO CASE
  *** Select Mode
  CASE loFormSet.ActiveMode = 'S'  
    loFormSet.lccompfis = " "
    IF EMPTY(loFormSet.lcComp_ID)
      STORE " " TO lcComp,loFormSet.puComp,loFormSet.lccomp_id
      loFormSet.Ariaform1.pucomp.DisplayValue = 'Select Company'
      
      loFormSet.Ariaform1.pucomp.Enabled = .T.
      loFormSet.laHolDates = {}
      loFormSet.Ariaform1.lcFisDes.Value   = ''
      loFormSet.llAcptPrd  = .F.
      loFormSet.llUpdHd    = .F.
      loFormSet.llUpdPr    = .F.
      loFormSet.Ariaform1.pbPeriod.Enabled = .F.
      loFormSet.Ariaform1.pbNotes.Enabled = .F.
      loFormSet.Ariaform1.pbHoliDay.Enabled = .F.
      
    ENDIF
  *** View Mode
  CASE loFormSet.ActiveMode = 'V' 
    SELECT FISHD
    lcScFields = loFormSet.lcScFields
    SCATTER FIELDS &lcScFields MEMO TO loFormSet.laData 

    loFormSet.laHolDates = {}
    loFormSet.lcOldHold  = loFormSet.laData[2]    
    lcIndExpHd = loFormSet.lcIndExpHd
    lcIndTagHd = loFormSet.lcIndTagHd
     SELECT dFshhdate ;
           FROM (loFormSet.lcDataDir+"fshld") ;
           WHERE &lcIndExpHd = loFormSet.laData[2];
           INTO ARRAY loFormSet.laHolDates

    SET ENGINEBEHAVIOR 70    
    *SELECT *,RECNO() AS 'nRecNo' , "S" AS 'cStatus' ;
           FROM (loFormSet.lcDataDir+"fshld"),&lcCursor ;
           INTO DBF (gcWorkDir+lc_TempHd);
           WHERE &lcIndExpHd = loFormSet.laData[2]	;
           DISTINCT
    SELECT *,RECNO() AS 'nRecNo' , "S" AS 'cStatus' ;
           FROM (loFormSet.lcDataDir+"fshld") ;
           INTO DBF (gcWorkDir+lc_TempHd);
           WHERE &lcIndExpHd = loFormSet.laData[2]	;
           DISTINCT
    SET ENGINEBEHAVIOR 90
    
    INDEX ON &lcIndExpHd.&lcIndTagHd   && Indexing the temp file so any modification
                                       && will get the right position

    loFormSet.lcCompFis = loFormSet.lcComp_ID+loFormSet.laData[2]
    SELECT *,RECNO() AS 'nRecNo', "S" AS 'cStatus';
           FROM (loFormSet.lcDataDir+"FSPRD") ;
           WHERE CFISFYEAR+CFSPPRDID = loFormSet.laData[2];
           INTO DBF (gcWorkDir+lc_TempPR)

    lnYer     = IIF(AT(loFormSet.laData[3],"PCNH") = 0 , 2 , AT(loFormSet.laData[3],"PCNH"))
    loFormSet.Ariaform1.lcFisDes.Value  = IIF(lnYer = 0 , ' ' , loFormSet.laTYears[lnYer] )
    loFormSet.llAcptPrd = .T.
    loFormSet.llUpdHd   = .F.
    loFormSet.llUpdPr   = .F.
    loFormSet.Ariaform1.pbPeriod.Enabled = .T.
    loFormSet.Ariaform1.pbNotes.Enabled = .T.
    loFormSet.Ariaform1.pbHoliDay.Enabled = .T.
    
    loFormSet.Ariaform1.Refresh()
    
    *** Set the non working days for each company ***
    FOR lnCount = 1 TO 7
      lcObjName  = "cbDay_"+ALLTRIM(STR(lnCount))
      loFormSet.&lcObjName = IIF(ALLTRIM(STR(lnCount)) $ loFormSet.laData[10],1,0)
    ENDFOR

  *** Edit Mode
  CASE loFormSet.ActiveMode = 'E'

    loFormSet.Ariaform1.pucomp.Enabled = .F.
    loFormSet.Ariaform1.laData2.Enabled = .F.
    IF FISHD.cFisystat <> 'C'
      loFormSet.Ariaform1.laData5.Enabled = .F.
    ENDIF
    loFormSet.Ariaform1.laData6.Enabled = .F.
    loFormSet.Ariaform1.laData7.Enabled = .F.
    
  *** Add Mode
  CASE loFormSet.ActiveMode = 'A' AND EMPTY(loFormSet.laData[5])
    loFormSet.Ariaform1.lcFisDes.Value  = loFormSet.laTYears[2]
    loFormSet.laData[3] = 'C'
    loFormSet.llAcptPrd = .F.
    loFormSet.llUpdHd   = .F.
    loFormSet.llUpdPr   = .F.
    loFormSet.Ariaform1.laData5.Enabled = .F.
    loFormSet.Ariaform1.pbPeriod.Enabled = .F.
    loFormSet.Ariaform1.pbHoliDay.Enabled = .F.
ENDCASE

IF loFormSet.ActiveMode = 'V' .AND. loFormSet.laData[3] <> 'H'
  loFormSet.oToolBar.cmdEdit.Enabled = .T.
  loFormSet.oToolBar.cmdEdit.ControlEnable = 1
ELSE
  loFormSet.oToolBar.cmdEdit.Enabled = .F.
  loFormSet.oToolBar.cmdEdit.ControlEnable = 0  
ENDIF 

loFormSet.lcBaseFile = 'FISHD'
loFormSet.Ariaform1.lcFisDes.Enabled = .F.
 
*!**************************************************************************
*!
*!      Function: lfvData_1
*!
*!**************************************************************************
*
FUNCTION lfvData_1
PARAMETERS loFormSet,loFld

lcCursor = loFormSet.lcCursor
lc_TempHd = loFormSet.lc_TempHd
lc_TempPr = loFormSet.lc_TempPr
gcWorkDir = oAriaApplication.WorkDir

IF loFld.DisplayValue == 'Select Company' OR loFld.Value == loFld.OldValue
  RETURN 
ENDIF  

IF !EMPTY(loFormSet.lcComp_Id)
 
  IF SEEK(loFormSet.lcComp_ID,"SYCCOMP")
    loFormSet.laData[2] = sycComp.cCurr_yer
  ELSE
    loFormSet.laData[2] = SPACE(4)
  ENDIF
  IF EMPTY(loFormSet.laData[2])
    STORE {} TO loFormSet.laData[6] , loFormSet.laData[7]
  ENDIF
  =lfOpenCmpData(loFormSet)
  
  DO CASE
    CASE !EMPTY(loFormSet.lcComp_ID) .AND. EMPTY(loFormSet.laData[2])
      *** The company fiscal years have not been set up yet. ***
      *** Setup or copy from another company?                ***
      *** <  Setup  > - <  Copy  > - <  Cancel  > ***
      lnOption = gfModalGen("QRM00082B00019","DIALOG")
      DO CASE
        CASE lnOption = 1                && Setup
          loFormSet.Ariaform1.puComp.Enabled = .F.         
          loFormSet.Ariaform1.lcFisDes.Value = loFormSet.laTYears[2]
          loFormSet.Ariaform1.lcFisDes.Enabled = .F.
          loFormSet.Ariaform1.laData2.Enabled = .T.
          loFormSet.Ariaform1.laData2.SetFocus()
        CASE lnOption = 2                && Copy
          loFormSet.lcOldComID = loFormSet.lcComp_ID
          loFormSet.lcCmFisYer = ''
          =lfCopyComp()                  && Function to copy data from another comp.
          
        CASE lnOption = 3                && Cancel
          loFormSet.Ariaform1.puComp.Enabled = .T.          
          RETURN .F.
      ENDCASE
    CASE !EMPTY(loFormSet.lcComp_ID) .AND. !EMPTY(loFormSet.laData[2]) 
      SEEK loFormSet.laData[2]
      lcScFields = loFormSet.lcScFields
      SCATTER FIELDS &lcScFields MEMO TO loFormSet.laData
      loFormSet.ChangeMode('V')  && = .T.       && Go to view mode
      
  ENDCASE
ELSE
  loFormSet.Ariaform1.puComp.Enabled = .T.
ENDIF
loFormSet.Ariaform1.Refresh()

*!**************************************************************************
*!
*!      Function: lfvData_2
*!
*!**************************************************************************
*
FUNCTION lfvData_2
PARAMETERS loFormSet, loFld

lcCursor = loFormSet.lcCursor
lc_TempHd = loFormSet.lc_TempHd
lc_TempPr = loFormSet.lc_TempPr
gcWorkDir = oAriaApplication.WorkDir
 
IF !EMPTY(loFormSet.laData[2])
  IF EMPTY(loFormSet.lcComp_ID)
    *** You have to select company first. ***
    *** <  Ok  > ***
    =gfModalGen("TRM00192B00000","DIALOG")
    loFormSet.laData[2] = SPACE(4)
    loFormSet.Ariaform1.puComp.SetFocus()
    RETURN 
  ENDIF
  
  IF !BETWEEN(VAL(loFormSet.laData[2]),1900,9998)
    loFormSet.laData[2] = SPACE(4)
    RETURN .F.
  ELSE
    IF SEEK(ALLTRIM(loFormSet.laData[2]))
      lcScFields = loFormSet.lcScFields
      SCATTER FIELDS &lcScFields MEMO TO loFormSet.laData
      loFormSet.ChangeMode('V')  && = .T.       && Go to view mode
      loFormSet.Ariaform1.Refresh()
    ELSE
      lcFisYears = ALLTRIM(STR(VAL(loFormSet.laData[2])-1))+','+ALLTRIM(loFormSet.laData[2]);
                   +','+ALLTRIM(STR(VAL(loFormSet.laData[2])+1))
      *** Fiscal years are ð.  Create default fiscal years? ***
      *** <  Yes  > - <  No  > - <  Reenter  > ***
      lnOption = gfModalGen("QRM00081B00020","DIALOG",lcFisYears)
      DO CASE
        CASE lnOption = 1               && set the default values
          loFormSet.laData[4]    = '12'
          loFormSet.laData[5]    = '01'
          lcSavDate    = SET("DATE")
          SET DATE American
          loFormSet.laData[6]    = CTOD('01/01/'+ALLTRIM(loFormSet.laData[2]))
          loFormSet.laData[7]    = CTOD('12/31/'+ALLTRIM(loFormSet.laData[2]))
          SET DATE &lcSavDate
          * Default current year short and long descriptions
          lcCen = SET('CENTURY')
          SET CENTURY ON
          loFormSet.laData[8] = 'Fis. year '+loFormSet.laData[2]
          loFormSet.laData[9] = 'Fiscal year '+loFormSet.laData[2]+ SPACE(1) + '(' + ;
                       DTOC(loFormSet.laData[6]) +'-' +DTOC(loFormSet.laData[7]) + ')'
          SET CENTURY &lcCen
          loFormSet.laData[10]   = '67'
          loFormSet.ChangeMode('A')  && = .T.
        CASE lnOption = 2
          loFormSet.ChangeMode('A')  && = .T.

        CASE lnOption = 3
          loFormSet.laData[2] = SPACE(4)
          loFormSet.Ariaform1.laData2.Enabled = .T.
          loFormSet.Ariaform1.Refresh()
          RETURN .F.
      ENDCASE
    ENDIF
  ENDIF  
ENDIF  

loFormSet.Ariaform1.Refresh()

*!**************************************************************************
*!
*!      Function: lfvData_4
*!
*!**************************************************************************
*
FUNCTION lfvData_4
PARAMETERS loFormSet,loFld

lcCursor = loFormSet.lcCursor
lc_TempHd = loFormSet.lc_TempHd
lc_TempPr = loFormSet.lc_TempPr
gcWorkDir = oAriaApplication.WorkDir

loFormSet.lcFldNm  = 'No. of periods'
lnNoPrd  = VAL(loFormSet.laData[4])
lnCurPrd = VAL(loFormSet.laData[5])

IF lnCurPrd <> 0 .AND. lnNoPrd  < lnCurPrd .AND. lnNoPrd <> 0
  *** No. of periods cannot be less than ***
  *** the value of current period...!    ***
  *** <  Ok  > ***
  = gfModalGen("QRM00062B00000","DIALOG")
  loFormSet.laData[4] = loFld.OldValue
  RETURN .F.
  
ELSE
  IF lnNoPrd <= 0 .OR. lnNoPrd > 13
    *** Period value must be between 1 and ð...! ***
    *** <  Ok  > ***
    = gfModalGen("QRM00063B00000","DIALOG",'13')
    loFormSet.laData[4] = loFld.OldValue
    RETURN .F.
  ELSE
    loFormSet.laData[4] = RIGHT("0"+ALLTRIM(loFormSet.laData[4]),2)  
    IF loFld.OldValue <> loFormSet.laData[4]
      =lfvAcptPrd()          && To know if accept to change periods or not.
      IF loFormSet.llOldFlg
        loFormSet.llOldFlg = .F.
        loFormSet.laData[4] = loFld.OldValue
      ENDIF
    ENDIF  
    loFormSet.Ariaform1.laData5.Enabled = .T.
  ENDIF
ENDIF

loFormSet.Ariaform1.laData4.Refresh()

*!**************************************************************************
*!
*!      Function: lfvData_5
*!
*!**************************************************************************
*
FUNCTION lfvData_5
PARAMETERS loFormSet,loFld

lcCursor = loFormSet.lcCursor
lc_TempHd = loFormSet.lc_TempHd
lc_TempPr = loFormSet.lc_TempPr
gcWorkDir = oAriaApplication.WorkDir


lnNoPrd  = VAL(loFormSet.laData[4])
lnCurPrd = VAL(loFormSet.laData[5])
llContinue = .T.
IF lnCurPrd <= 0 .OR. lnCurPrd > lnNoPrd
  *** Period value must be between 1 and ð...! ***
  *** <  Ok  > ***
  =gfModalGen("QRM00063B00000","DIALOG",loFormSet.laData[4])
  loFormSet.laData[5] = loFld.OldValue
  llContinue = .F.
ELSE 
  loFormSet.laData[5] = RIGHT("0"+ALLTRIM(loFormSet.laData[5]),2)
ENDIF

DO CASE
  CASE loFormSet.ActiveMode = 'E' .AND. loFormSet.llAcptPrd
    SELECT (lc_TempPR)
    GO TOP
    REPLACE ALL cStatus   WITH IIF(AT(cStatus,"SAM") > 0,;
                               SUBSTR("MAM",AT(cStatus,"SAM"),1),"S");
                lFspclsds WITH IIF(VAL(cFspprdid) < VAL(loFormSet.laData[5]),;
                               .T. , .F.)
    loFormSet.llUpdPr = .T.
    SELECT FISHD
  CASE loFormSet.ActiveMode = 'A' .AND. loFormSet.llAcptPrd
    SELECT (lc_TempPR)
    GO TOP
    SCAN
      REPLACE lFspclsds WITH IIF(VAL(cFspprdid) < VAL(loFormSet.laData[5]),.T.,.F.)
    ENDSCAN
    SELECT FISHD
ENDCASE
loFormSet.Ariaform1.laData5.Refresh()
RETURN llContinue
*!**************************************************************************
*!
*!      Function: lfvData_6
*!
*!**************************************************************************
*
FUNCTION lfvData_6
PARAMETERS loFormSet,loFld

lcCursor = loFormSet.lcCursor
lc_TempHd = loFormSet.lc_TempHd
lc_TempPr = loFormSet.lc_TempPr
gcWorkDir = oAriaApplication.WorkDir

loFormSet.lcFldNm = 'Start date'
loFormSet.lcYer   = ALLTRIM(loFormSet.laData[2]) + ' or ' + ALLTRIM(STR(VAL(loFormSet.laData[2])-1))

DO CASE
  CASE loFormSet.laData[6] >= loFormSet.laData[7] .AND. !EMPTY(loFormSet.laData[7])
    *** End date must be greater than start date. ***
    *** <  Ok  > ***
    = gfModalGen("QRM00069B00000","DIALOG")
    loFormSet.laData[6] = loFormSet.lcOldStrt
    RETURN .F.
  CASE GOMONTH(loFormSet.laData[6],12) < loFormSet.laData[7] .AND. !EMPTY(loFormSet.laData[7])
    *** Fiscal year cannot exceed twelve months. ***
    *** <  Ok  > ***
    = gfModalGen("QRM00070B00000","DIALOG")
    loFormSet.laData[6] = loFormSet.lcOldStrt
    RETURN .F.
  CASE YEAR(loFormSet.laData[6]) = VAL(loFormSet.laData[2]) .OR. ;
       YEAR(loFormSet.laData[6]) = VAL(loFormSet.laData[2]) - 1
    IF loFormSet.lcOldStrt <> loFormSet.laData[6]
      =lfvAcptPrd()         && To know if accept to change periods or not.
      IF loFormSet.llOldFlg
        loFormSet.llOldFlg = .F.
        loFormSet.laData[6] = loFormSet.lcOldStrt
      ENDIF
    ENDIF
    =lfvChcDate()             && To know if there is any holidays not
                              && within the start & end date..
    loFormSet.Ariaform1.laData6.Refresh()
  OTHERWISE
    *** Start date can only be in year ð. ***
    *** <  Ok  > ***
    =gfModalGen("QRM00067B00000","DIALOG",loFormSet.lcYer)
    loFormSet.laData[6] = loFormSet.lcOldStrt
    RETURN .F.
ENDCASE

*!**************************************************************************
*!
*!      Function: lfvData_7
*!
*!**************************************************************************
*
 FUNCTION lfvData_7
PARAMETERS loFormSet,loFld

lcCursor = loFormSet.lcCursor
lc_TempHd = loFormSet.lc_TempHd
lc_TempPr = loFormSet.lc_TempPr
gcWorkDir = oAriaApplication.WorkDir

loFormSet.lcFldNm = 'End date'

DO CASE
  CASE YEAR(loFormSet.laData[7]) <> VAL(loFormSet.laData[2])
    *** End date must be in ð. ***
    *** <  Ok  > ***
    = gfModalGen("QRM00068B00000","DIALOG",loFormSet.laData[2])
    loFormSet.laData[7] = loFormSet.lcOldEnd
    RETURN .F.
  CASE loFormSet.laData[7] <= loFormSet.laData[6]
    *** End date must be greater than start date. ***
    *** <  Ok  > ***
    = gfModalGen("QRM00069B00000","DIALOG")
    loFormSet.laData[7] = loFormSet.lcOldEnd
    RETURN .F.
  CASE GOMONTH(loFormSet.laData[6],12) < loFormSet.laData[7] .AND. !EMPTY(loFormSet.laData[6])
    *** Fiscal year cannot exceed twelve months. ***
    *** <  Ok  > ***
    = gfModalGen("QRM00070B00000","DIALOG")
    loFormSet.laData[7] = loFormSet.lcOldEnd
    RETURN .F.
  OTHERWISE
    IF loFormSet.lcOldEnd <> loFormSet.laData[7]
      =lfvAcptPrd()         && To know if accept to change periods or not
      IF loFormSet.llOldFlg
        loFormSet.llOldFlg = .F.
        loFormSet.laData[7] = loFormSet.lcOldEnd
      ENDIF
    ENDIF
    =lfvChcDate()             && To know if there is any holidays not
                              && within the start & end date..
ENDCASE

loFormSet.Ariaform1.laData7.Refresh()
loFormSet.Ariaform1.pbPeriod.Enabled = .T.
loFormSet.Ariaform1.pbHoliDay.Enabled = .T.

*!**************************************************************************
*!
*!      Function: lfvPeriod
*!
*!**************************************************************************
*
FUNCTION lfvPeriod
PARAMETERS loFormSet

DO (oAriaapplication.applicationhome+'SM\SMFSPRD.FXP') WITH loFormSet

*!**************************************************************************
*!
*!      Function: lfvAddDay
*!
*!**************************************************************************
*
FUNCTION lfvAddDay
PARAMETERS loFormSet,loFld
loFormSet.laData[10] = IIF(loFld.Value = 1,;
               ALLTRIM(loFormSet.laData[10])+RIGHT(loFld.Name,1),;
               STUFF(loFormSet.laData[10],AT(RIGHT(loFld.Name,1),loFormSet.laData[10]),1,''))

loFormSet.laData[10] = ALLTRIM(loFormSet.laData[10])

************************************************************
*! Name      : lfFormBeforeSave
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 02/13/2013
*! Purpose   : Before save validation function 
************************************************************
FUNCTION lfFormBeforeSave
PARAMETERS loFormSet

IF EMPTY(loFormSet.laData[4])
  *** You have to enter no. of periods for this fiscal year. ***
  *** <  Ok  > ***
  =gfModalGen("TRM00204B00000","DIALOG")
  loFormSet.Ariaform1.laData4.SetFocus()
  llCSave = .F.
  RETURN llCSave
ENDIF
IF EMPTY(loFormSet.laData[5])
  *** You have to enter current period for this fiscal year. ***
  *** <  Ok  > ***
  =gfModalGen("TRM00205B00000","DIALOG")
  loFormSet.Ariaform1.laData5.SetFocus()
  llCSave = .F.
  RETURN llCSave
ENDIF
IF EMPTY(loFormSet.laData[6]) .OR. EMPTY(loFormSet.laData[7])
  *** You cannot save this fiscal year without ***
  *** entering the start and the end date  for ***
  *** this year...
  *** <  Ok  > ***
  =gfModalGen("TRM00203B00000","DIALOG")
  lcI = IIF(EMPTY(loFormSet.laData[6]),'6','7')
  loFormSet.Ariaform1.laData&lcI..SetFocus()
  llCSave = .F.
  RETURN llCSave
ENDIF

IF loFormSet.ActiveMode = 'A'             && Add mode
  
  DO WHILE  !loFormSet.llAcptPrd
    *** You have to add periods for the fiscal year ***
    *** ð before saving or cancel the session. ***
    *** <  Ok  > - <  Cancel  > ***
    IF gfModalGen("TRM00084B00030","DIALOG",loFormSet.laData[2]) = 1
      DO (oAriaapplication.applicationhome+'SM\SMFSPRD.FXP') WITH loFormSet

    ELSE
      llCSave = .F.
      RETURN llCSave
    ENDIF
  ENDDO
  
ENDIF 

*- End of lfFormBeforeSave.

*!**************************************************************************
*!
*!      Procedure: lpSavScr
*!
*!**************************************************************************
* This procedure replaces the default Save procedure for the push button
* "SAVE"
FUNCTION lpSavScr
PARAMETERS loFormSet

LOCAL lcI
IF loFormSet.ActiveMode = 'A'             && Add mode
  

  SELECT FISHD
  
  =lfAddFsYer('loFormSet.laData[2]','loFormSet.laData[3]','loFormSet.laData[6]','loFormSet.laData[7]',;
              loFormSet.lc_TempPR,VAL(loFormSet.laData[5]),VAL(loFormSet.laData[4]))

  IF loFormSet.llUpdHd
    loFormSet.llUpdHd = .F.    
    =gfTmp2Mast('FSHLD',lc_TempHd,;
                "Save all the holidays for the current year...")
  ENDIF

ELSE

  *IF loFormSet.lcBaseFile = 'SYCFISHD'
    *lcScattFields = STRTRAN(lcScFields,'CCOMP_ID,')
    *=ACOPY(loFormSet.laData,laFsYear)
    *=ADEL(laFsYear,1)
    lcScFields = loFormSet.lcScFields
    GATHER FROM loFormSet.laData FIELDS &lcScFields MEMO
  *ELSE
  *  GATHER FROM loFormSet.laData FIELDS &lcScFields MEMO     
  *ENDIF
  =gfAdd_Info(loFormSet.lcBaseFile)
  
  IF loFormSet.llUpdHd
    loFormSet.llUpdHd = .F.
    =gfTmp2Mast('FSHLD',loFormSet.lc_TempHd,;
                "Save all the holidays for the current year...")
  ENDIF

  IF loFormSet.llUpdPr
    loFormSet.llUpdPr = .F.
    =gfTmp2Mast('FSPRD',loFormSet.lc_TempPr,;
                "Save periods for "+loFormSet.Ariaform1.lcFisDes.Value+" year..")
  ENDIF
ENDIF  

SELECT FISHD

IF loFormSet.llSavFlg
  llSavFis = .T.
  loFormSet.llEndScr = .T.
  CLEAR READ
ENDIF


*!**************************************************************************
*!
*!      Function: lfvAcptPrd
*!
*!**************************************************************************
*
FUNCTION lfvAcptPrd

IF loFormSet.llAcptPrd
  lnOption =gfModalGen("QRM00074B00018","ALERT",loFormSet.lcFldNm)
  loFormSet.llOldFlg    = .T.
  DO CASE
    CASE lnOption = 1               && Reenter Periods
      loFormSet.llAcptPrd   = .F.
      loFormSet.llNewPrds   = .T.
      loFormSet.llDefPrds   = .F.
      DO (oAriaapplication.applicationhome+'SM\SMFSPRD.FXP') WITH loFormSet

    CASE lnOption = 3               && Accept Default
      loFormSet.llAcptPrd   = .F.
      loFormSet.llNewPrds   = .T.
      loFormSet.llDefPrds   = .T.
      DO (oAriaapplication.applicationhome+'SM\SMFSPRD.FXP') WITH loFormSet

  ENDCASE
ENDIF

*!**************************************************************************
*!
*!      Function: lfCopyComp
*!
*!**************************************************************************
*
FUNCTION lfCopyComp

SELECT FISHD
lnSavRec = IIF(RECNO()>RECCOUNT(),0,RECNO())
GO TOP
llEof = EOF()
IF lnSavRec > 0
  GO lnSavRec
ENDIF

IF !llEof
  *DO (gcScrDir + gcWinAppl + '\SMCOPCM.SPR')
  lcScx = lfGetScx('SM\SMCOPCM.SCX')
  DO FORM (lcScx) WITH loFormSet
ELSE
  =gfModalGen("TRM00086B00000","DIALOG")
  loFormSet.Ariaform1.lcFisDes.Value = loFormSet.laTYears[2]
  loFormSet.Ariaform1.lcFisDes.Enabled = .F.
  loFormSet.Ariaform1.laData2.Enabled = .T.
ENDIF  

*!**************************************************************************
*!
*!      Function: lfvComID
*!
*!**************************************************************************
*
FUNCTION lfvComID
PARAMETERS loBranFormSet
LOCAL lnSlct,lcFlt
lnSlct = SELECT(0)
loFormSet = loBranFormSet.loFormSet
IF !SEEK(loBranFormSet.Ariaform1.lcComID.Value,'SYCCOMP') OR loBranFormSet.Ariaform1.lcComID.Value = loFormSet.lcComp_ID = loBranFormSet.Ariaform1.lcComID.Value
  SELECT syccomp
  SET FILTER TO CCOMP_ID <> loFormSet.lcComp_ID
  =gfBrows()
ENDIF
SELECT (lnSlct)

*!**************************************************************************
*!
*!      Function: lfvCmFisYr
*!
*!**************************************************************************
*
FUNCTION lfvCmFisYr
PARAMETERS loBranFormSet
loFormSet = loBranFormSet.loFormSet

IF !EMPTY(loBranFormSet.Ariaform1.lcComID.Value) .AND. !EMPTY(loFormSet.lcCmFisYer)
  IF SEEK(loBranFormSet.Ariaform1.lcComID.Value+loFormSet.lcCmFisYer)
    =lfColData()              && Collect data from another company.
  ELSE
    GO TOP
    lcBrFields  = "cComp_id :H='Company ID',cFisfyear :H='Fiscal Year',cFisystat :H='Fiscal State'"
    lcFile_ttl  = "Fiscal Year Information"

    loFormSet.lcComp_ID = ''
    =gfBrows()

    IF EMPTY(loFormSet.lcComp_ID)
      loBranFormSet.Ariaform1.lcComID.Value = '  '
      loFormSet.lcCmFisYer = '    '
      loBranFormSet.Ariaform1.lcComID.Enabled = .T.
      loBranFormSet.Ariaform1.lcComID.SetFocus()
    ELSE
      =lfColData()              && Collect data from another company
    ENDIF
  ENDIF
ENDIF

*!**************************************************************************
*!
*!      Function: lfCOlData
*!
*!**************************************************************************
*
FUNCTION lfColData

*** Collect data from another company ***
IF SEEK(loBranFormSet.Ariaform1.lcComID.Value,"SYCCOMP")
  *lcComp = SYCCOMP.cComp_Id+" "+SYCCOMP.cCom_name
  loFormSet.Ariaform1.puComp.Value = SYCCOMP.cComp_Id+" "+SYCCOMP.cCom_name
  
  *** Collect data from the fiscal header file ***
  lcScFields = loFormSet.lcScFields
  SCATTER FIELDS &lcScFields MEMO TO loFormSet.laData

  SELECT *,RECNO() AS 'nRecNo', "S" AS 'cStatus';
         FROM (loFormSet.lcDataDir+"FSPRD") ;
         WHERE CFISFYEAR+CFSPPRDID = loFormSet.laData[2];
         INTO DBF (gcWorkDir+lc_TempPR)

  SELECT FISHD
  loFormSet.lcComp_ID    = loFormSet.lcOldComID
  loFormSet.lcCompFis    = loFormSet.laData[2]  
  loFormSet.Ariaform1.lcFisDes.Value   = loFormSet.laTYears[2]
  loFormSet.llAcptPrd    = .T.
  loFormSet.llUpdPr      = .T.
  loFormSet.ChangeMode('A')  && = .T.       && Go to Add mode
  
  loBranFormSet.Release()
ENDIF

*!**************************************************************************
*!
*!      Function: lfvCancel
*!
*!**************************************************************************
*
FUNCTION lfvCancel

*loFormSet.lcComp_Id = loFormSet.lcOldComID
loFormSet.lcComp_ID = loFormSet.lcOldComID

*!**************************************************************************
*!
*!      Function: lfvChcDate
*!
*!**************************************************************************
*
FUNCTION lfvChcDate
*** To know if there is any holidays not ***
*** within the start & end date..        ***

IF !EMPTY(loFormSet.laHolDates)
  SELECT (lc_TempHd)
  FOR lnCnt = 1 TO ALEN(loFormSet.laHolDates,1)
    IF !BETWEEN(loFormSet.laHolDates[lnCnt],loFormSet.laData[6],loFormSet.laData[7])
      IF SEEK(loFormSet.lcComp_ID+loFormSet.laData[2]+DTOS(loFormSet.laHolDates[lnCnt]))
        lcStatus = SUBSTR('DDS',AT(cStatus,'SMA'),1)   && Delete
        REPLACE cStatus WITH lcStatus
        DELETE
        loFormSet.llUpdHd   = .T.
        loFormSet.laHolDates[lnCnt] = {}
      ENDIF
    ENDIF
  ENDFOR
ENDIF

*!**************************************************************************
*!
*!      Function: lfOpenCmpData
*!
*!**************************************************************************
* function to open all the data files from the company dir.
FUNCTION lfOpenCmpData
PARAMETERS loFormSet

IF !EMPTY(loFormSet.lcComp_ID)  
  loFormSet.lcDataDir = ALLT(LOOKUP(SYCCOMP.CCOM_DDIR,loFormSet.lcComp_ID,syccomp.ccomp_id,'Ccomp_id'))
  loFormSet.lcDataDir = gfGetDataDir(loFormSet.lcDataDir)
  IF USED('FISHD')
    USE IN FISHD    
  ENDIF
  *USE (loFormSet.lcDataDir+'FISHD') IN 0 AGAIN ALIAS SYCFISHD ORDER TAG COMPFYEAR
  USE (loFormSet.lcDataDir+'FISHD') IN 0 AGAIN ALIAS FISHD ORDER TAG COMPFYEAR
  SELECT FISHD
  CURSORSETPROP("Buffering",5)

    
  IF USED('FSHLD')
    USE IN FSHLD
  ENDIF
  USE (loFormSet.lcDataDir+'FSHLD') IN 0 AGAIN ALIAS FSHLD ORDER TAG COMFYRHDAT
  SELECT FSHLD
  CURSORSETPROP("Buffering",5)
  
  IF USED('FSPRD')
    USE IN FSPRD
  ENDIF
  USE (loFormSet.lcDataDir+'FSPRD') IN 0 AGAIN ALIAS FSPRD ORDER TAG Comfyrprdi
  SELECT FSPRD
  CURSORSETPROP("Buffering",5)
  
  SELECT FSHLD
  loFormSet.lcIndExpHd = lfGetIndExp()
  loFormSet.lcIndTagHd = lfGetIndTag()
  
  loFormSet.lnFcount = FCOUNT('FSHLD') + 3     && Var to know no. of holidays fields 
  DECLARE loFormSet.laBlanks[5,loFormSet.lnFcount]
  
  *** Build array data with blanks records ***
  FOR lnCnt = 1 TO 5
    loFormSet.laBlanks[lnCnt,1]  = CHR(255)
    loFormSet.laBlanks[lnCnt,2]  = CHR(255)
    loFormSet.laBlanks[lnCnt,3]  = {}
    loFormSet.laBlanks[lnCnt,4]  = SPACE(40)
    loFormSet.laBlanks[lnCnt,5]  = SPACE(12)
    loFormSet.laBlanks[lnCnt,loFormSet.lnFcount-1] = 'S'      && To skip these records during saving
  ENDFOR

  SELECT FISHD
ENDIF

*!**************************************************************************
*!
*!      Function: lfGetIndExp
*!
*!**************************************************************************
*
FUNCTION lfGetIndExp

lcExp = SYS(14,VAL(SYS(21)))
RETURN lcExp

*!**************************************************************************
*!
*!      Function: lfGetIndTag
*!
*!**************************************************************************
*
FUNCTION lfGetIndTag

lcTag = ' TAG '+SYS(22) + IIF('DESC' $ SET('ORDER'),' DESC','')
RETURN lcTag


*!********************************************************************
*!
*!              Function: lfAddFsYer
*!
*!********************************************************************
*
FUNCTION lfAddFsYer
PARAMETERS lcCurrYear,lcYearStat,lcBegDate,lcEndDate,;
           lcPrdTpFil,lnCurPriod,lnNo_Per

lcSavAlias = ALIAS() 
lcSaveYear = &lcCurrYear
lcSaveStat = &lcYearStat
ldSaveBeg  = &lcBegDate
ldSaveEnd  = &lcEndDate


llLastFeb  = .F.
lnYearCurr = INT(VAL(lcSaveYear))
lnYear     = INT(VAL(lcSaveYear))-2
lnYearDays = (ldSaveEnd - ldSaveBeg) + 1
ldBegin    = GOMONTH(ldSaveBeg ,-24)
ldEnd      = GOMONTH(ldSaveEnd ,-24)

*** We have to stop on the company record in the company file before start
*** the update becouse we are updateing the fields of the fisical year file
*** And the Currunt year and period in the company file at the same GATHER
*** command or you may have the error "End of file incountered" in the
*** company file not in the fisical year file.

SELECT SYCCOMP
SEEK loFormSet.lcComp_ID

SELECT (lcPrdTpFil)

llNotNor = .F.            &&Logical varible to know if the month Feb. is 29 days and ther is a priod with end date 02/28 
LOCATE FOR MONTH(dFsppendt) = 2 .AND. DAY(dFsppendt) = 28 
IF FOUND() AND MOD(YEAR(dFsppendt),4) = 0
  llNotNor = .T.
ENDIF       &&End of the IF Statmenyt 

GO TOP

REPLACE ALL cFisFYear  WITH ALLTRIM(STR(lnYear))   ;
            dFsppbgdt  WITH GOMONTH(dFsppbgdt , -24) ;
            dFsppendt  WITH GOMONTH(dFsppendt , -24)

DECLARE laYears[3]
laYears[1] = "previous"
laYears[2] = "current" 
laYears[3] = "next"

DECLARE laPerLock[lnNo_Per]
laPerLock = .F.
SCAN
  laPerLock[VAL(&lcPrdTpFil..cfspprdid)] = &lcPrdTpFil..lfsplocks
ENDSCAN
*- Save current year descriptions in two variables
lcShDesc = loFormSet.laData[8]
lcLngDes = loFormSet.laData[9]
FOR lnCount  = 1 TO 3
  SELECT FISHD
  lnYear      = lnYear  + 1
  ldBegin     = GOMONTH(ldBegin ,12)
  ldEnd       = GOmONTH(ldEnd   ,12)
  &lcCurrYear = ALLTRIM(STR(lnYear))
  &lcYearStat = SUBSTR('PCN',lnCount) 
  &lcBegDate  = ldBegin 
  &lcEndDate  = ldEnd
  lcCen = SET('CENTURY')
  SET CENTURY ON
  IF SUBSTR('PCN',lnCount) # 'C'
    loFormSet.laData[8] = 'Fis. year '+ ALLTRIM(STR(lnYear))
    loFormSet.laData[9] = 'Fiscal year '+ALLTRIM(STR(lnYear))+ SPACE(1) + '(' + ;
                DTOC(ldBegin) +'-' +DTOC(ldEnd) + ')'
  ELSE
    loFormSet.laData[8] = lcShDesc
    loFormSet.laData[9] = lcLngDes
   ENDIF
  SET CENTURY &lcCen
  
  APPEND BLANK
    lcScFields = loFormSet.lcScFields
    GATHER FROM loFormSet.laData FIELDS &lcScFields MEMO         
  =gfAdd_Info()
  SELECT (lcPrdTpFil)
  
  REPLACE ALL cFisFYear WITH ALLTRIM(STR(lnYear))   ;
              dFsppbgdt WITH GOMONTH(dFsppbgdt ,12) ;
              dFsppendt WITH GOMONTH(dFsppendt ,12) ;
              lFspclsds WITH IIF(lnCount=1,.T.,IIF(lnCount=3,.F.,;
                             IIF(VAL(cFspprdid)<lnCurPriod,.T.,.F.)));
              lfsplocks WITH IIF(lnCount=1 .OR. lnCount=3,.F.,;
                             laPerLock[VAL(&lcPrdTpFil..cfspprdid)]);
              cStatus   WITH 'A'


  LOCATE FOR MONTH(dFsppendt) = 2 .AND. DAY(dFsppendt) = 28 
  IF FOUND()    
    ldCrPrBeg = dFsppendt + 1        &&Varible to hold the next priod begin date
    IF MOD(YEAR(dFsppendt),4) = 0 .AND. !llNotNor
      REPLACE dFsppendt WITH (dFsppendt + 1)
    ENDIF      &&End of the IF Statmenyt

    IF llNotNor 
      SKIP 1
      IF !EOF()
        REPLACE dFsppbgdt WITH (ldCrPrBeg)
      ENDIF      &&End of the IF Statmenyt
    ENDIF      &&End of the IF Statmenyt
  ENDIF      &&End of the IF Statmenyt

  =gfTmp2Mast('FSPRD',lcPrdTpFil,;
              "Save periods for "+laYears[lnCount]+" year...")
ENDFOR

&lcCurrYear = lcSaveYear
&lcYearStat = lcSaveStat 

SELECT SYCCOMP
SEEK loFormSet.lcComp_ID
REPLACE cCurr_yer WITH loFormSet.laData[2]

SELECT (lcSavAlias)

************************************************************
*! Name      : lfShowMFISCOMNT
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 02/13/2013
*! Purpose   : Show notes field
************************************************************
FUNCTION lfShowMFISCOMNT
PARAMETERS loFormSet 

m.mStrRep = loFormSet.laData[11]
CREATE CURSOR TMPSTR (mStrRep M(10))
APPEND BLANK
REPLACE mStrRep WITH m.mStrRep  
llOk = .F.
DO FORM (oAriaApplication.ScreenHome + 'SY\SYZOOM') WITH loFormSet
IF llOk 
  loFormSet.laData[11] = TMPSTR.mStrRep
ENDIF 
USE IN TMPSTR


*- End of lfShowMFISCOMNT.

************************************************************
*! Name      : lfShowHolidays
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 02/13/2013
*! Purpose   : show holidays screen
************************************************************
FUNCTION lfShowHolidays
PARAMETERS loFormSet 
lcScx = lfGetScx('SM\SMHolid.scx')
DO FORM(lcScx) WITH loFormSet

*- End of lfShowHolidays.
************************************************************
*! Name      : lfFormInitHoliday
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 02/23/2013
*! Purpose   : Init method for the Holidays screen
************************************************************
FUNCTION lfFormInitHoliday
PARAMETERS loBranFormSet
loFormSet = loBranFormSet.loFormSet

WITH loBranFormSet.AriaForm1
  .Caption = 'Holidays'
  .cmdOK.Enabled = loFormSet.ActiveMode $ 'EA'
  .cmdNew.Enabled = loFormSet.ActiveMode $ 'EA'
  .cmdRemove.Enabled = loFormSet.ActiveMode $ 'EA'
  .puStat.RowSource = 'Thisformset.loFormSet.laTStat'
  .cFshhdesc.MaxLength = 35
ENDWITH

SELECT (loFormSet.lc_TempHd)
lst = loBranFormSet.AriaForm1.lsHoliday
SCAN 
  lst.AddItem(DTOC(dfshhdate))
  lst.AddListItem(SUBSTR(cfshhdesc,1,35),lst.ListCount,2)
  lst.AddListItem(cfshhstat,lst.ListCount,3)
ENDSCAN 
lst.SetFocus()
lst.ListIndex = 1
lst.InteractiveChange()




*DTOC(dfshhdate)+'|'+SUBSTR(cfshhdesc,1,35)+'|'+cfshhstat
*- End of lfFormInitHoliday.


************************************************************
*! Name      : lfFormActivate
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 02/13/2013
*! Purpose   : Activate screen
************************************************************
FUNCTION lfFormActivate
PARAMETERS loFormSet

*- End of lfFormActivate.


********************************************************************************************************************************************
********************************************************************************************************************************************
********************************************************************************************************************************************
*E303339,1 TMI 01/09/2013 [Start] this is just a version to allow the smcminf.prg to work, the smfsyer screen should be developed separately
IF .F.
  PARAMETERS pcComp_ID
  lcMsg = 'this is just a version to allow the smcminf.prg to work, the smfsyer screen should be developed separately'
  MESSAGEBOX(lcmsg)
  IF USED('FISHD')
    USE IN FISHD
  ENDIF 
  IF USED('fsprd')
    USE IN fsprd
  ENDIF   
  =gfOpenFile(ALLTRIM(loFormSet.laData[11])+'FISHD','COMPFYEAR','SH')   && CFISFYEAR
  =gfOpenFile(ALLTRIM(loFormSet.laData[11])+'FSPRD','COMFYRPRDI','SH')   && CFISFYEAR+CFSPPRDID
  
  DIMENSION laMonth[12]
  laMonth[1]  = 'January   '
  laMonth[2]  = 'February  '
  laMonth[3]  = 'March     '
  laMonth[4]  = 'April     '
  laMonth[5]  = 'May       '
  laMonth[6]  = 'June      '
  laMonth[7]  = 'July      '
  laMonth[8]  = 'August    '
  laMonth[9]  = 'September '
  laMonth[10] = 'October   '
  laMonth[11] = 'November  '
  laMonth[12] = 'December  '
  
  lcStat = 'PCN'
  FOR i=-1 TO 1
    SELECT FISHD
    APPEND BLANK
    REPLACE CFISFYEAR WITH STR(YEAR(DATE())+i,4) ,;
            CFISYSTAT WITH SUBSTR(lcStat,i+2,1) ,;
            CFISNOPRD WITH '12' ,;
            CFISLHEAD WITH  ' fiscal year' + STR(YEAR(DATE())+i,4),;
            CFISSHEAD WITH  CFISLHEAD ,;
            CFISNONWD WITH  '67     ',;
            DFISBGDAT WITH  DATE(YEAR(DATE())+i,1,1) ,;
            DFISENDAT WITH  DATE(YEAR(DATE())+i,12,31)
    
    FOR j=1 TO 12
      SELECT FSPRD
      APPEND BLANK
      REPLACE CFISFYEAR WITH FISHD.CFISFYEAR  ,;
              CFSPPRDID WITH padl(j,2,'0') ,;
              CFSPPDESC WITH laMonth[j] ,;
              DFSPPBGDT WITH DATE(YEAR(DATE())+i ,j,1);
              DFSPPENDT WITH gomonth(DFSPPBGDT,1)-1 ,;
              NFSPPARTN WITH CEILING(j/3)
    ENDFOR 
  ENDFOR 
  SELECT syccomp
  LOCATE FOR ccomp_id = pcComp_ID
  replace CCURR_YER WITH STR(YEAR(DATE()),4)       ,;
          CCURR_PRD WITH padl(MONTH(DATE()),2,'0')
  loFormSet.llSavFis = .T.
ENDIF 
*E303339,1 TMI 01/09/2013 [End  ] 