***********************************************************************
*:  Program file : ALPIKPK.PRG
*:  Program desc.: Custom Assignment User/Picking ticket program for GOU10
*:         System: Aria 4XP
*:      Developer: Mariam Mazhar[MMT]
*:           Date: 07/22/2014
*:      Reference: C201633.Exe,E303494[T20140616.0017]
*:************************************************************************
*: Modifications:
*B610880,1 MMT 10/13/2014 Enhance the performance of custom Picking/packing assignment program[T20140616.0017]
*:************************************************************************
=GFCALLFORM('ALPIKPK','AL')

*!*************************************************************
*! Name      : lfFormGoInit
*! Developer : Mariam Mazhar[MMT]
*! Date      : 07/22/2014
*! Purpose   : Form Init
*!*************************************************************
FUNCTION lfFormGoInit
PARAMETERS loFormSet
STORE "" TO lcProjectTemplate,lcDummyUserID
llCreateProject = .F.
loFormSet.lcAssignedAct = gfTempName()
loFormSet.lcUnAssignedAct = gfTempName()
loFormSet.lnmajorlen =LEN(gfitemmask("PM"))
=gfopenTable("ORDHDR","ORDHDR")
=gfopenTable("ORDLINE","ORDLINE")
=gfopenTable("CUSTOMER","CUSTOMER")
=gfopenTable("PIKTKT","PIKTKT")
=gfopentable('PMCALDT','PMCALDT','SH')
=gfopentable('PMCALHD','PMCALHD','SH')
=gfopentable('PMPRJRL','PMPRJRL','SH')
=gfopentable('PMPRJNTF','PMPRJNTF','SH')
=gfOpenTable('PMPRJDT','PMPRJDT')
=gfOpenTable('syschdul','Coprusr')
=gfOpenTable('PMPTHHD','PMPTHHD')
=gfopentable('SYUUSER','Cuser_id','SH')
=gfOpenTable('PIKTKT','PIKTKT')
=gfOpenTable('PMPRJHD','PMPRJHD','SH')    
=gfOpenTable('PMPTHDT','PMPTHDT')
IF !lfValidateSetups(loFormSet)
  RETURN .F.
ENDIF
lfCreateTemp(loFormSet)
*!*************************************************************
*! Name      : lfValidateSetups
*! Developer : Mariam Mazhar[MMT]
*! Date      : 07/22/2014
*! Purpose   : Validate setups
*!*************************************************************
FUNCTION lfValidateSetups
PARAMETERS loFormSet
lcPikAct = ''
lcPackAct =''
IF !FILE(oAriaApplication.DataDir+"GOU10Setup.XML")
  =GFCALLFORM('ALGOSETUPS','AL')
  IF !FILE(oAriaApplication.DataDir+"GOU10Setup.XML")
    RETURN .F.
  ENDIF
  lobjDOMDocument = CREATEOBJECT("MSXML2.DOMDocument")
  lobjDOMDocument.LOAD(oAriaApplication.DataDir+"GOU10Setup.XML")
  loRoot = lobjDOMDocument.childNodes(1)
  FOR lnIndex = 0 TO loRoot.childNodes(0).childNodes.Length-1
    IF (UPPER(loRoot.childNodes(0).childNodes(lnIndex).baseName) == UPPER("Create_Projects"))
      llCreateProject = IIF(ALLTRIM(loRoot.childNodes(0).childNodes(lnIndex).text)=='Y',.T.,.F.)
    ENDIF
    IF (UPPER(loRoot.childNodes(0).childNodes(lnIndex).baseName) == UPPER("Project_Template_ID"))
      lcProjectTemplate = ALLTRIM(loRoot.childNodes(0).childNodes(lnIndex).text)
    ENDIF
    IF (UPPER(loRoot.childNodes(0).childNodes(lnIndex).baseName) == UPPER("Default_User"))
      lcDummyUserID = ALLTRIM(loRoot.childNodes(0).childNodes(lnIndex).text)
    ENDIF
    IF (UPPER(loRoot.childNodes(0).childNodes(lnIndex).baseName) == UPPER("Pick_Activity"))
      lcPikAct = ALLTRIM(loRoot.childNodes(0).childNodes(lnIndex).text)
    ENDIF
    IF (UPPER(loRoot.childNodes(0).childNodes(lnIndex).baseName) == UPPER("Pack_Activity"))
      lcPackAct = ALLTRIM(loRoot.childNodes(0).childNodes(lnIndex).text)
    ENDIF

  ENDFOR    
ELSE
  lobjDOMDocument = CREATEOBJECT("MSXML2.DOMDocument")
  lobjDOMDocument.LOAD(oAriaApplication.DataDir+"GOU10Setup.XML")
  loRoot = lobjDOMDocument.childNodes(1)
  FOR lnIndex = 0 TO loRoot.childNodes(0).childNodes.Length-1
    IF (UPPER(loRoot.childNodes(0).childNodes(lnIndex).baseName) == UPPER("Create_Projects"))
      llCreateProject = IIF(ALLTRIM(loRoot.childNodes(0).childNodes(lnIndex).text)=='Y',.T.,.F.)
    ENDIF
    IF (UPPER(loRoot.childNodes(0).childNodes(lnIndex).baseName) == UPPER("Project_Template_ID"))
      lcProjectTemplate = ALLTRIM(loRoot.childNodes(0).childNodes(lnIndex).text)
    ENDIF
    IF (UPPER(loRoot.childNodes(0).childNodes(lnIndex).baseName) == UPPER("Default_User"))
      lcDummyUserID = ALLTRIM(loRoot.childNodes(0).childNodes(lnIndex).text)
    ENDIF
    IF (UPPER(loRoot.childNodes(0).childNodes(lnIndex).baseName) == UPPER("Pick_Activity"))
      lcPikAct = ALLTRIM(loRoot.childNodes(0).childNodes(lnIndex).text)
    ENDIF
    IF (UPPER(loRoot.childNodes(0).childNodes(lnIndex).baseName) == UPPER("Pack_Activity"))
      lcPackAct = ALLTRIM(loRoot.childNodes(0).childNodes(lnIndex).text)
    ENDIF
    
  ENDFOR     
ENDIF
loFormSet.llCreateProject = llCreateProject 

IF !gfSEEK(PADR(lcProjectTemplate,4),'PMPTHHD')
  lcProjectTemplate = ''
ENDIF

IF !gfSEEK(PADR(lcDummyUserID ,10),'SYUUSER')
  lcDummyUserID = ''
ENDIF

loFormset.lcPackAct = lcPackAct 
loFormSet.lcPikAct  = lcPikAct 
loFormSet.lcProjectTemplate  = lcProjectTemplate 
loFormSet.lcDummyUserID = lcDummyUserID 
IF EMPTY(ALLTRIM(lcDummyUserID)) OR EMPTY(ALLTRIM(lcProjectTemplate)) OR EMPTY(ALLTRIM(lcPackAct)) OR EMPTY(ALLTRIM(lcPikAct))
  =GFCALLFORM('ALGOSETUPS','AL')
  lobjDOMDocument = CREATEOBJECT("MSXML2.DOMDocument")
  lobjDOMDocument.LOAD(oAriaApplication.DataDir+"GOU10Setup.XML")
  loRoot = lobjDOMDocument.childNodes(1)
  FOR lnIndex = 0 TO loRoot.childNodes(0).childNodes.Length-1
    IF (UPPER(loRoot.childNodes(0).childNodes(lnIndex).baseName) == UPPER("Create_Projects"))
      llCreateProject = IIF(ALLTRIM(loRoot.childNodes(0).childNodes(lnIndex).text)=='Y',.T.,.F.)
    ENDIF
    IF (UPPER(loRoot.childNodes(0).childNodes(lnIndex).baseName) == UPPER("Project_Template_ID"))
      lcProjectTemplate = ALLTRIM(loRoot.childNodes(0).childNodes(lnIndex).text)
    ENDIF
    IF (UPPER(loRoot.childNodes(0).childNodes(lnIndex).baseName) == UPPER("Default_User"))
      lcDummyUserID = ALLTRIM(loRoot.childNodes(0).childNodes(lnIndex).text)
    ENDIF
    IF (UPPER(loRoot.childNodes(0).childNodes(lnIndex).baseName) == UPPER("Pick_Activity"))
      lcPikAct = ALLTRIM(loRoot.childNodes(0).childNodes(lnIndex).text)
    ENDIF
    IF (UPPER(loRoot.childNodes(0).childNodes(lnIndex).baseName) == UPPER("Pack_Activity"))
      lcPackAct = ALLTRIM(loRoot.childNodes(0).childNodes(lnIndex).text)
    ENDIF
  ENDFOR
ENDIF
IF  EMPTY(ALLTRIM(lcProjectTemplate))
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Template ID is invalid or empty, Cannot Proceed.')
  RETURN .F.  
ENDIF

IF EMPTY(ALLTRIM(lcDummyUserID)) 
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Default User ID is invalid or empty, Cannot Proceed.')
  RETURN .F.  
ENDIF

IF !gfSEEK(PADR(lcProjectTemplate,4),'PMPTHDT')
  lcProjectTemplate = ''
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Template ID is invalid or empty, Cannot Proceed.')
  RETURN .F.  
ENDIF

IF EMPTY(ALLTRIM(lcPikAct)) 
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Picking Activity is invalid or empty, Cannot Proceed.')
  RETURN .F.  
ENDIF

IF EMPTY(ALLTRIM(lcPackAct)) 
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Packing Activity is invalid or empty, Cannot Proceed.')
  RETURN .F.  
ENDIF
loFormset.lcPackAct = lcPackAct 
loFormSet.lcPikAct  = lcPikAct 
loFormSet.lcProjectTemplate  = lcProjectTemplate 
loFormSet.lcDummyUserID = lcDummyUserID 

SELECT PMPTHDT
lnLowerSeq = 1
DIMENSION laNoUserActivities[1,2]
laNoUserActivities = ''
SCAN 
IF VAL(COPRT_SEQ) < lnLowerSeq
  lnLowerSeq = VAL(COPRT_SEQ) 
ENDIF
IF PADR(ALLTRIM(coprt_res),10)==PADR(ALLTRIM(lcDummyUserID),10)
  IF EMPTY(ALLTRIM(laNoUserActivities[1,1]))
    laNoUserActivities[1,1] = coprt_dsc
    laNoUserActivities[1,2] = coprt_ctg+coprt_id
  ELSE
    DIMENSION laNoUserActivities[ALEN(laNoUserActivities,1)+1,2]
    laNoUserActivities[ALEN(laNoUserActivities,1),1] = coprt_dsc
    laNoUserActivities[ALEN(laNoUserActivities,1),2] = coprt_ctg+coprt_id
  ENDIF
ENDIF
ENDSCAN


DIMENSION LOFORMSET.laNoUserActivities [1,2]
LOFORMSET.laNoUserActivities [1,1] = "Select Activity"
LOFORMSET.laNoUserActivities [1,2] = 'NA'
IF !EMPTY(ALLTRIM(laNoUserActivities[1,1]))
   DIMENSION LOFORMSET.laNoUserActivities [ALEN(laNoUserActivities,1)+1,2]
   FOR lnCntAr = 1 TO ALEN(laNoUserActivities,1)
     LOFORMSET.laNoUserActivities [lnCntAr+1,1] = laNoUserActivities[lnCntAr,1]
     LOFORMSET.laNoUserActivities [lnCntAr+1,2] = laNoUserActivities[lnCntAr,2]
   ENDFOR
ENDIF
loFormSet.AriaForm1.cboActivity.Requery()
lcActToCompl = ""
SELECT PMPTHDT
LOCATE FOR  VAL(COPRT_SEQ) = lnLowerSeq
IF FOUND()
lcActToCompl = coprt_ctg+coprt_id
ENDIF
IF llCreateProject
  lfCreateProjects()
ENDIF
*!*************************************************************
*! Name      : lfCreateTemp
*! Developer : Mariam Mazhar[MMT]
*! Date      : 07/22/2014
*! Purpose   : Create Temp. Files
*!*************************************************************
FUNCTION lfCreateTemp
PARAMETERS loFormSet
DIMENSION laFileStructure[9,4]
laFileStructure[1,1] =  "TYPE"
laFileStructure[1,2] = "C"
laFileStructure[1,3] = 4
laFileStructure[1,4] = 0

laFileStructure[2,1] =  "PIKTKT"
laFileStructure[2,2] = "C"
laFileStructure[2,3] = 6
laFileStructure[2,4] = 0

laFileStructure[3,1] =  "ACCOUNT"
laFileStructure[3,2] = "C"
laFileStructure[3,3] = 5
laFileStructure[3,4] = 0

laFileStructure[4,1] =  "NAME"
laFileStructure[4,2] = "C"
laFileStructure[4,3] = 30
laFileStructure[4,4] = 0

laFileStructure[5,1] =  "TOTPIK"
laFileStructure[5,2] = "N"
laFileStructure[5,3] = 10
laFileStructure[5,4] = 0

laFileStructure[6,1] =  "Status"
laFileStructure[6,2] = "C"
laFileStructure[6,3] = 1
laFileStructure[6,4] = 0

laFileStructure[7,1] =  "Start"
laFileStructure[7,2] = "D"
laFileStructure[7,3] = 8
laFileStructure[7,4] = 0

laFileStructure[8,1] =  "Complete"
laFileStructure[8,2] = "D"
laFileStructure[8,3] = 8
laFileStructure[8,4] = 0

laFileStructure[9,1] =  "ccont_id"
laFileStructure[9,2] = "C"
laFileStructure[9,3] = 8
laFileStructure[9,4] = 0



=gfcrttmp(loFormSet.lcAssignedAct ,@laFileStructure,"PIKTKT",loFormSet.lcAssignedAct ,.T.)
=gfcrttmp(loFormSet.lcUnAssignedAct ,@laFileStructure,"PIKTKT",loFormSet.lcUnAssignedAct,.T.)
loFormSet.ChangeMode ('S')

*!*************************************************************
*! Name      : lfAssignGridSources 
*! Developer : Mariam Mazhar[MMT]
*! Date      : 07/22/2014
*! Purpose   : Assign grids control source
*!*************************************************************
FUNCTION lfAssignGridSources 
PARAMETERS loFormSet  
WITH loFormSet.AriaForm1.grdAssigned
  .RECORDSOURCE = ''
  .RecordSource = loFormSet.lcAssignedAct
  .Column1.ControlSource  =loFormSet.lcAssignedAct+'.Type'
  .Column2.ControlSource  =loFormSet.lcAssignedAct+'.PIKTKT'
  .Column4.ControlSource  =loFormSet.lcAssignedAct+'.ACCOUNT'
  .Column5.ControlSource  =loFormSet.lcAssignedAct+'.NAME'
  .Column6.ControlSource  =loFormSet.lcAssignedAct+'.TOTPIK'
  .Column7.ControlSource  = "ThisFormSet.mGetStatus()"&&loFormSet.lcAssignedAct+'.Status'
  .Column8.ControlSource  =loFormSet.lcAssignedAct+'.Start'
  .Column9.ControlSource  =loFormSet.lcAssignedAct+'.Complete'
  .Columns(3).CurrentControl = "cmdOpenPK"
  .Columns(10).CurrentControl = "cmdOpenPL"
  .Columns(10).visible =.f.
  .SETALL('READONLY',.T.,'COLUMN')
  .Enabled =.T.
ENDWITH 

WITH loFormSet.AriaForm1.grdUnAssigned
  .RECORDSOURCE = ''
  .RecordSource = loFormSet.lcUnAssignedAct 
  .Columns(1).ControlSource  =loFormSet.lcUnAssignedAct +'.Type'
  .Columns(2).ControlSource  =loFormSet.lcUnAssignedAct +'.PIKTKT'
  .Columns(4).ControlSource  =loFormSet.lcUnAssignedAct +'.ACCOUNT'
  .Columns(5).ControlSource  =loFormSet.lcUnAssignedAct +'.NAME'
  .Columns(6).ControlSource  =loFormSet.lcUnAssignedAct +'.TOTPIK'
  .Columns(7).ControlSource  = "ThisFormSet.mGetStatus()"&&loFormSet.lcUnAssignedAct +'.Status'
  .Columns(8).ControlSource  =loFormSet.lcUnAssignedAct +'.Start'
  .Columns(9).ControlSource  =loFormSet.lcUnAssignedAct +'.Complete'
  .SETALL('READONLY',.T.,'COLUMN')  
  .Columns(1).CurrentControl = "Text1"
  .Columns(2).CurrentControl = "Text1"
  .Columns(4).CurrentControl = "Text1"
  .Columns(5).CurrentControl = "Text1"
  .Columns(6).CurrentControl = "Text1"
  .Columns(7).CurrentControl = "Text1"
  .Columns(8).CurrentControl = "Text1"
  .Columns(9).CurrentControl = "Text1"
  .Columns(3).CurrentControl ="cmdOpenPK"
  .Enabled =.T.
ENDWITH 

*!*************************************************************
*! Name      : lfChangeMode
*! Developer : Mariam Mazhar[MMT]
*! Date      : 07/22/2014
*! Purpose   : Change Mode
*!*************************************************************
FUNCTION lfChangeMode
PARAMETERS loFormSet
IF loFormSet.ActiveMode ='S'
  loFormSet.AriaForm1.cboActivity.Enabled =.T.
  loFormSet.AriaForm1.cboActivity.Value = 'NA'
  loFormSet.AriaForm1.cmdRefresh.Enabled =.T.
ENDIF
lfAssignGridSources (loFormSet)
*!*************************************************************
*! Name      : lfCreateProjects  
*! Developer : Mariam Mazhar[MMT]
*! Date      : 07/22/2014
*! Purpose   : Create Projects
*!*************************************************************
FUNCTION lfCreateProjects  
*!*  _SCreen.Visible =.t.
*!*  SET STEP ON 
IF !USED('PMPRJHD_ALL')
  =gfOpenTable('PMPRJHD','PMPRJHD','SH','PMPRJHD_ALL')    
ENDIF  
SELECT PMPRJHD_ALL
*=gfSeek('K')
=gfSqlRun("Select Top 10000 * from PMPRJHD[INDEX = PMPRJHD] Where cPRJ_TYP ='K' ORDER by CPRJ_ID DESC",'PMPRJHD_ALL')
SELECT PIKTKT
=gfSeek('')
COUNT FOR Status $ "OHCX"  AND BETWEEN(Date,GOMONTH(oAriaApplication.SystemDate,-6),oAriaApplication.SystemDate) TO lnPikCount
LOCATE 
LOCATE FOR Status $ "OHCX" AND BETWEEN(Date,GOMONTH(oAriaApplication.SystemDate,-6),oAriaApplication.SystemDate)
lnCntPk = 0
*B610880,1 MMT 10/13/2014 Enhance the performance of custom Picking/packing assignment program[Start]
loFormProj = .F.
lnCrPrjCnt = 0
*B610880,1 MMT 10/13/2014 Enhance the performance of custom Picking/packing assignment program[End]
SCAN REST FOR Status $ "OHCX" AND BETWEEN(Date,GOMONTH(oAriaApplication.SystemDate,-6),oAriaApplication.SystemDate)
  WAIT WINDOW "Checking Picking Ticket # : "+PIKTKT.PIKTKT NOWAIT 
  *B610880,1 MMT 10/13/2014 Enhance the performance of custom Picking/packing assignment program[Start]
  lnCntPk = lnCntPk + 1 
  *B610880,1 MMT 10/13/2014 Enhance the performance of custom Picking/packing assignment program[End]
  DO CASE 
    CASE Status $ "OH" AND !Seek('K'+PIKTKT.PIKTKT,'PMPRJHD_ALL') AND !gfSeek('K'+PIKTKT.PIKTKT,'PMPRJHD')
      *B610880,1 MMT 10/13/2014 Enhance the performance of custom Picking/packing assignment program[Start]
*      lnCntPk = lnCntPk + 1 
      *B610880,1 MMT 10/13/2014 Enhance the performance of custom Picking/packing assignment program[End]
      IF TYPE('opross') <> 'O'
        opross = CREATEOBJECT('ariaprogressbar')
        oPross.TotalProgress = lnPikCount
        oPross.AutoCenter = .T.
        opross.Visible = .T.
      ENDIF

      oPross.CurrentProgress(lnCntPk)
      oPross.lblFirstLabel.Caption = "Creating Project for Picking Ticket#: "+PIKTKT.PIKTKT
      oPross.Show()
      *B610880,1 MMT 10/13/2014 Enhance the performance of custom Picking/packing assignment program[Start]  
      *DO (oAriaApplication.ApplicationHome+'MFPROJ.FXP') WITH 'K',PIKTKT.PIKTKT,REPLICATE('*',loFormSet.lnmajorlen),0,lcProjectTemplate
      lfCallProjectScreen(PIKTKT.PIKTKT)
      *DO (oAriaApplication.ApplicationHome+'MFPROJ.FXP') WITH 'K',PIKTKT.PIKTKT,REPLICATE('*',loFormSet.lnmajorlen),0,lcProjectTemplate
      *B610880,1 MMT 10/13/2014 Enhance the performance of custom Picking/packing assignment program[End]
     * MESSAGEBOX(SET("Procedure"))
      lfSchedul(PIKTKT.PIKTKT)
      lnCrPrjCnt = lnCrPrjCnt + 1
      IF MOD(lnCrPrjCnt ,50)=0 
        IF TYPE('loFormProj') = 'O' AND !ISNULL(loFormProj)
          loFormProj.ChangeMode("S")
          loFormProj.release()
          loFormProj = Null 
        ENDIF  
      ENDIF
      
    CASE PIKTKT.Status = 'C' AND Seek('K'+PIKTKT.PIKTKT,'PMPRJHD_ALL')  AND PMPRJHD_ALL.cprj_stts <> 'C'
     IF TYPE('opross') <> 'O'
        opross = CREATEOBJECT('ariaprogressbar')
        oPross.TotalProgress = lnPikCount
        oPross.AutoCenter = .T.
        opross.Visible = .T.
      ENDIF
      *B610880,1 MMT 10/13/2014 Enhance the performance of custom Picking/packing assignment program[Start]
*      lnCntPk = lnCntPk + 1 
      *B610880,1 MMT 10/13/2014 Enhance the performance of custom Picking/packing assignment program[End]
      oPross.CurrentProgress(lnCntPk)
      oPross.lblFirstLabel.Caption = "Completing the project of Picking Ticket#: "+PIKTKT.PIKTKT
      oPross.Show()
      lfCompleteProject(PIKTKT.PIKTKT)
    CASE PIKTKT.Status = 'X'  AND Seek('K'+PIKTKT.PIKTKT,'PMPRJHD_ALL') AND PMPRJHD_ALL.cprj_stts <> 'X'
      IF TYPE('opross') <> 'O'
        opross = CREATEOBJECT('ariaprogressbar')
        oPross.TotalProgress = lnPikCount
        oPross.AutoCenter = .T.
        opross.Visible = .T.
      ENDIF
      *B610880,1 MMT 10/13/2014 Enhance the performance of custom Picking/packing assignment program[Start]
      *lnCntPk = lnCntPk + 1 
      *B610880,1 MMT 10/13/2014 Enhance the performance of custom Picking/packing assignment program[End]
      oPross.CurrentProgress(lnCntPk)
      oPross.lblFirstLabel.Caption = "Cancelling the project of Picking Ticket#: "+PIKTKT.PIKTKT
      oPross.Show()
      lfCancelProject(PIKTKT.PIKTKT)
  ENDCASE
ENDSCAN
oPross = Null
*B610880,1 MMT 10/13/2014 Enhance the performance of custom Picking/packing assignment program[Start]
*TRY
IF TYPE('loFormProj') = 'O' AND !ISNULL(loFormProj)
  loFormProj.ChangeMode("S")
  loFormProj.release()
  loFormProj= Null
  RELEASE  loFormProj
ENDIF  
*CATCH 
*ENDTRY
*B610880,1 MMT 10/13/2014 Enhance the performance of custom Picking/packing assignment program[End]
*!*************************************************************
*! Name      : lfvChkCreatePrj
*! Developer : Mariam Mazhar[MMT]
*! Date      : 07/22/2014
*! Purpose   : Validdate Create Projects check box
*!*************************************************************
FUNCTION lfvChkCreatePrj
PARAMETERS loBFormSet
*!*  IF loBFormSet.AriaForm1.chkCreateProject.Value 
*!*    loBFormSet.AriaForm1.kbTmpl.Enabled = .T.
*!*    loBFormSet.AriaForm1.userkey.Enabled = .T.
*!*  ELSE
*!*    loBFormSet.AriaForm1.kbTmpl.Enabled = .F.
*!*    loBFormSet.AriaForm1.userkey.Enabled = .F.
*!*    loBFormSet.AriaForm1.kbTmpl.KeyTextBox.Value = ''
*!*    loBFormSet.AriaForm1.userkey.KeyTextBox.Value = ''
*!*  ENDIF 
*!*************************************************************
*! Name      : lfInitSetups
*! Developer : Mariam Mazhar[MMT]
*! Date      : 07/22/2014
*! Purpose   : Init of setups form
*!*************************************************************
FUNCTION  lfInitSetups
PARAMETERS loBFormSet
IF FILE(oAriaApplication.DataDir+"GOU10Setup.XML")
  lcXMLStr = FILETOSTR(oAriaApplication.DataDir+"GOU10Setup.XML")
  IF !EMPTY(lcXMLStr)
    lobjDOMDocument = CREATEOBJECT("MSXML2.DOMDocument")
    lobjDOMDocument.LOAD(oAriaApplication.DataDir+"GOU10Setup.XML")
    loRoot = lobjDOMDocument.childNodes(1)
    FOR lnIndex = 0 TO loRoot.childNodes(0).childNodes.Length-1
      IF (UPPER(loRoot.childNodes(0).childNodes(lnIndex).baseName) == UPPER("Create_Projects"))
        llCreateProject = IIF(ALLTRIM(loRoot.childNodes(0).childNodes(lnIndex).text)=='Y',.T.,.F.)
      ENDIF
      IF (UPPER(loRoot.childNodes(0).childNodes(lnIndex).baseName) == UPPER("Project_Template_ID"))
        lcProjectTemplate = ALLTRIM(loRoot.childNodes(0).childNodes(lnIndex).text)
      ENDIF
      IF (UPPER(loRoot.childNodes(0).childNodes(lnIndex).baseName) == UPPER("Default_User"))
        lcDummyUserID = ALLTRIM(loRoot.childNodes(0).childNodes(lnIndex).text)
      ENDIF
      IF (UPPER(loRoot.childNodes(0).childNodes(lnIndex).baseName) == UPPER("Pick_Activity"))
        lcPikAct = ALLTRIM(loRoot.childNodes(0).childNodes(lnIndex).text)
      ENDIF
      IF (UPPER(loRoot.childNodes(0).childNodes(lnIndex).baseName) == UPPER("Pack_Activity"))
        lcPackAct = ALLTRIM(loRoot.childNodes(0).childNodes(lnIndex).text)
      ENDIF
    ENDFOR
    
    
    loBFormSet.AriaForm1.chkCreateProject.Value = llCreateProject 
	*IF llCreateProject 
	  IF !USED('PMPTHHD')
	    =gfOpenTable('PMPTHHD','PMPTHHD')
	  ENDIF
	  IF !gfSEEK(PADR(lcProjectTemplate,4),'PMPTHHD')
	    lcProjectTemplate = ''
	  ENDIF
	  IF !USED('SYUUSER')
	    =gfopentable('SYUUSER','Cuser_id','SH')
	  ENDIF
	  IF !gfSEEK(PADR(lcDummyUserID ,10),'SYUUSER')
	    lcDummyUserID = ''
	  ENDIF
	  loBFormSet.AriaForm1.kbTmpl.KeyTextBox.Value = lcProjectTemplate 
	  loBFormSet.AriaForm1.userkey.KeyTextBox.Value = lcDummyUserID 
    IF EMPTY(lcProjectTemplate) OR EMPTY(lcDummyUserID)
      loBFormSet.AriaForm1.cboPackActivity.Enabled = .F.
  	  loBFormSet.AriaForm1.cboPikActivity.Enabled = .F.
    ELSE
      loBFormSet.AriaForm1.cboPackActivity.Enabled = .T.
      loBFormSet.AriaForm1.cboPikActivity.Enabled = .T.
      = gfSEEK(PADR(lcProjectTemplate,4),'PMPTHDT')
      SELECT PMPTHDT
      DIMENSION laNoUserActivities[1,2]
     laNoUserActivities = ''
     SCAN 
      IF PADR(ALLTRIM(coprt_res),10)==PADR(ALLTRIM(lcDummyUserID),10)
        IF EMPTY(ALLTRIM(laNoUserActivities[1,1]))
          laNoUserActivities[1,1] = coprt_dsc
          laNoUserActivities[1,2] = coprt_ctg+coprt_id
        ELSE
          DIMENSION laNoUserActivities[ALEN(laNoUserActivities,1)+1,2]
          laNoUserActivities[ALEN(laNoUserActivities,1),1] = coprt_dsc
          laNoUserActivities[ALEN(laNoUserActivities,1),2] = coprt_ctg+coprt_id
        ENDIF
      ENDIF  
    ENDSCAN
    DIMENSION loBFormSet.laNoUserActivities [1,2]
    loBFormSet.laNoUserActivities [1,1] = "Select Activity"
    loBFormSet.laNoUserActivities [1,2] = 'NA'
    IF !EMPTY(ALLTRIM(laNoUserActivities[1,1]))
       DIMENSION loBFormSet.laNoUserActivities [ALEN(laNoUserActivities,1)+1,2]
       FOR lnCntAr = 1 TO ALEN(laNoUserActivities,1)
         loBFormSet.laNoUserActivities [lnCntAr+1,1] = laNoUserActivities[lnCntAr,1]
         loBFormSet.laNoUserActivities [lnCntAr+1,2] = laNoUserActivities[lnCntAr,2]
       ENDFOR
     ENDIF
    loBFormSet.AriaForm1.cboPackActivity.Requery()
    loBFormSet.AriaForm1.cboPikActivity.Requery()
    loBFormSet.AriaForm1.cboPikActivity.Value = 'NA'
    loBFormSet.AriaForm1.cboPackActivity.Value = 'NA'
  ENDIF 
*!*  	ELSE
*!*  	  loBFormSet.AriaForm1.kbTmpl.Enabled = .F.
*!*  	  loBFormSet.AriaForm1.userkey.Enabled = .F.
*!*  	  loBFormSet.AriaForm1.kbTmpl.KeyTextBox.Value = ''
*!*  	  loBFormSet.AriaForm1.userkey.KeyTextBox.Value = ''
*!*  	ENDIF 
  ENDIF
ELSE
  loBFormSet.AriaForm1.cboPikActivity.Enabled = .F.
  loBFormSet.AriaForm1.cboPackActivity.Enabled = .F.
ENDIF
*!*************************************************************
*! Name      : lfSaveSetups
*! Developer : Mariam Mazhar[MMT]
*! Date      : 07/22/2014
*! Purpose   : Save setups
*!*************************************************************
FUNCTION lfSaveSetups
PARAMETERS loBFormSet
*IF loBFormSet.AriaForm1.chkCreateProject.Value 
IF EMPTY(ALLTRIM(loBFormSet.AriaForm1.kbTmpl.KeyTextBox.Value))
   =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Template ID is empty or invalid, Cannot Save.' )
  RETURN .F.  
ENDIF
IF EMPTY(ALLTRIM(loBFormSet.AriaForm1.userkey.KeyTextBox.Value))
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Default user ID is empty or invalid, Cannot Save.' )
  RETURN .F.
ENDIF  
IF loBFormSet.AriaForm1.cboPikActivity.Value = 'NA' OR EMPTY(ALLTRIM(loBFormSet.AriaForm1.cboPikActivity.Value))
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Picking Activity is empty or invalid, Cannot Save.' )
  RETURN .F.
ENDIF  

IF loBFormSet.AriaForm1.cboPackActivity.Value = 'NA' OR EMPTY(ALLTRIM(loBFormSet.AriaForm1.cboPackActivity.Value))
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Packing Activity is empty or invalid, Cannot Save.' )
  RETURN .F.
ENDIF  


lcXmlString = '<?xml version="1.0"?>'+CHR(13)+CHR(10)
lcXmlString = lcXmlString +  "  <xdoc>"+CHR(13)+CHR(10)
lcXmlString = lcXmlString +  "      <Setups>"+CHR(13)+CHR(10)
lcXmlString = lcXmlString +  "      <Create_Projects>"+IIF(loBFormSet.AriaForm1.chkCreateProject.Value ,'Y','N')+"</Create_Projects>"+CHR(13)+CHR(10)
lcXmlString = lcXmlString +  "      <Project_Template_ID>"+loBFormSet.AriaForm1.kbTmpl.KeyTextBox.Value+"</Project_Template_ID>"+CHR(13)+CHR(10)
lcXmlString = lcXmlString +  "      <Default_User>"+loBFormSet.AriaForm1.userkey.KeyTextBox.Value+"</Default_User>"+CHR(13)+CHR(10)
lcXmlString = lcXmlString +  "      <Pick_Activity>"+loBFormSet.AriaForm1.cboPikActivity.Value +"</Pick_Activity>"+CHR(13)+CHR(10)
lcXmlString = lcXmlString +  "      <Pack_Activity>"+loBFormSet.AriaForm1.cboPackActivity.Value +"</Pack_Activity>"+CHR(13)+CHR(10)
lcXmlString = lcXmlString +  "      </Setups>"+CHR(13)+CHR(10)
lcXmlString = lcXmlString +  "  </xdoc>"
STRTOFILE(lcXmlString ,oAriaApplication.DataDir+"GOU10Setup.XML",0)
loBFormSet.Release
*!*************************************************************
*! Name      : lfvPath_ID
*: Developer : Mariam Mazhar[MMT]
*: Date      : 07/22/2014
*! Purpose   : Valid function for get field cPath_ID
*!*************************************************************
*! Calls              :  lfVldKey()
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvPath_ID()
*!*************************************************************
FUNCTION lfvpath_id
PARAMETERS loBFormSet
PRIVATE lcoldsmode
IF !USED('PMPTHHD')
  =gfOpenTable('PMPTHHD','PMPTHHD')
ENDIF
#include r:\aria4xp\prgs\mfproj.h
WITH loBFormSet.ariaform1
  llbrowse  = .kbtmpl.selectedfrombrowse
  m.cpath_id = PADR(ALLTRIM(.kbtmpl.keytextbox.VALUE),4)
  lcoldval  = .kbtmpl.keytextbox.oldvalue
  IF llbrowse .OR. (!EMPTY(m.cpath_id) AND !gfseek(m.cpath_id,'PMPTHHD')) 
    SELECT pmpthhd
    =gfseek('')
    lcbrfields = "Cpath_id  :H='"+lang_mfproj_tmpid+"'," +;
    "Cpath_dsc :H='"+lang_mfproj_tmpname+"'"

    DIMENSION lafields[1]
    SELECT pmpthhd
    LOCATE
    llsel = gfbrows('','Cpath_id','laFields',lang_mfproj_tmplate)
    IF llsel
      .kbtmpl.keytextbox.VALUE = pmpthhd.cpath_id
    ELSE
     .kbtmpl.keytextbox.VALUE = ''
    ENDIF
    IF .kbtmpl.selectedfrombrowse
      .kbtmpl.selectedfrombrowse= .F.
    ENDIF
  ELSE
    .kbtmpl.selectedfrombrowse = .F.
  ENDIF
ENDWITH
IF !EMPTY(ALLTRIM(loBFormSet.ariaform1.kbtmpl.keytextbox.VALUE)) AND !EMPTY(ALLTRIM(loBFormSet.ariaform1.userkey.keytextbox.VALUE))
  lcProjectTemplate=loBFormSet.ariaform1.kbtmpl.keytextbox.VALUE
  lcDummyUserID = loBFormSet.ariaform1.userkey.keytextbox.VALUE
  = gfSEEK(PADR(lcProjectTemplate,4),'PMPTHDT')
  SELECT PMPTHDT
  DIMENSION laNoUserActivities[1,2]
  laNoUserActivities = ''
  SCAN 
    IF PADR(ALLTRIM(coprt_res),10)==PADR(ALLTRIM(lcDummyUserID),10)
      IF EMPTY(ALLTRIM(laNoUserActivities[1,1]))
        laNoUserActivities[1,1] = coprt_dsc
        laNoUserActivities[1,2] = coprt_ctg+coprt_id
      ELSE
        DIMENSION laNoUserActivities[ALEN(laNoUserActivities,1)+1,2]
        laNoUserActivities[ALEN(laNoUserActivities,1),1] = coprt_dsc
        laNoUserActivities[ALEN(laNoUserActivities,1),2] = coprt_ctg+coprt_id
      ENDIF
    ENDIF  
  ENDSCAN
  DIMENSION loBFormSet.laNoUserActivities [1,2]
  loBFormSet.laNoUserActivities [1,1] = "Select Activity"
  loBFormSet.laNoUserActivities [1,2] = 'NA'
  IF !EMPTY(ALLTRIM(laNoUserActivities[1,1]))
     DIMENSION loBFormSet.laNoUserActivities [ALEN(laNoUserActivities,1)+1,2]
     FOR lnCntAr = 1 TO ALEN(laNoUserActivities,1)
       loBFormSet.laNoUserActivities [lnCntAr+1,1] = laNoUserActivities[lnCntAr,1]
       loBFormSet.laNoUserActivities [lnCntAr+1,2] = laNoUserActivities[lnCntAr,2]
     ENDFOR
   ENDIF
  loBFormSet.AriaForm1.cboPackActivity.Requery()
  loBFormSet.AriaForm1.cboPikActivity.Requery()
  loBFormSet.AriaForm1.cboPikActivity.Value = 'NA'
  loBFormSet.AriaForm1.cboPackActivity.Value = 'NA'
  loBFormSet.AriaForm1.cboPikActivity.Enabled = .T.
  loBFormSet.AriaForm1.cboPackActivity.Enabled = .T.
ENDIF


*!*************************************************************
*! Name      : lfvUsrId
*! Developer : Mariam Mazhar
*! Date      : 07/22/2014
*! Purpose   : validate user id function of Notification screen
*!*************************************************************
FUNCTION lfvusrid
PARAMETERS lontfrmset

IF !USED('SYUUSER')
  =gfopentable('SYUUSER','Cuser_id','SH')
ENDIF

WITH lontfrmset.ariaform1.userkey
  lcoldval   = .keytextbox.oldvalue
  m.cuser_id = .keytextbox.VALUE
  llbrowse   = .selectedfrombrowse
ENDWITH

IF (PADR(m.cuser_id,10) = PADR(lcoldval,10)) .AND. !llbrowse
  RETURN .T.
ENDIF

PRIVATE lncuralias
lncuralias = SELECT(0)
SELECT syuuser
lcbrfields = [cUser_Id : H = ']+lang_mfproj_usrid  +;
             [', cUsr_Name : H = ']+lang_mfproj_usrname+[']

IF (!EMPTY(m.cuser_id) AND !gfseek(PADR(m.cuser_id,10),'SYUUSER')) .OR. llbrowse
  llbrowse = .F.
  DIMENSION latemp[1]
  IF ariabrow('',lang_mfproj_users,gnbrfsrow1, gnbrfscol1, gnbrfsrow2, gnbrfscol2,.F.,.F.,'cUser_Id','laTemp')
    m.cuser_id   = latemp[1]
  ELSE
    m.cuser_id   = lcoldval
  ENDIF
  lontfrmset.ariaform1.userkey.keytextbox.VALUE = m.cuser_id
  lcoldval = lontfrmset.ariaform1.userkey.keytextbox.oldvalue
ENDIF
loBFormSet = lontfrmset
IF !EMPTY(ALLTRIM(loBFormSet.ariaform1.kbtmpl.keytextbox.VALUE)) AND !EMPTY(ALLTRIM(lontfrmset.ariaform1.userkey.keytextbox.VALUE))
  lcProjectTemplate=loBFormSet.ariaform1.kbtmpl.keytextbox.VALUE
  lcDummyUserID = lontfrmset.ariaform1.userkey.keytextbox.VALUE
  = gfSEEK(PADR(lcProjectTemplate,4),'PMPTHDT')
  SELECT PMPTHDT
  DIMENSION laNoUserActivities[1,2]
  laNoUserActivities = ''
  SCAN 
    IF PADR(ALLTRIM(coprt_res),10)==PADR(ALLTRIM(lcDummyUserID),10)
      IF EMPTY(ALLTRIM(laNoUserActivities[1,1]))
        laNoUserActivities[1,1] = coprt_dsc
        laNoUserActivities[1,2] = coprt_ctg+coprt_id
      ELSE
        DIMENSION laNoUserActivities[ALEN(laNoUserActivities,1)+1,2]
        laNoUserActivities[ALEN(laNoUserActivities,1),1] = coprt_dsc
        laNoUserActivities[ALEN(laNoUserActivities,1),2] = coprt_ctg+coprt_id
      ENDIF
    ENDIF  
  ENDSCAN
  DIMENSION lontfrmset.laNoUserActivities [1,2]
  lontfrmset.laNoUserActivities [1,1] = "Select Activity"
  lontfrmset.laNoUserActivities [1,2] = 'NA'
  IF !EMPTY(ALLTRIM(laNoUserActivities[1,1]))
     DIMENSION lontfrmset.laNoUserActivities [ALEN(laNoUserActivities,1)+1,2]
     FOR lnCntAr = 1 TO ALEN(laNoUserActivities,1)
       lontfrmset.laNoUserActivities [lnCntAr+1,1] = laNoUserActivities[lnCntAr,1]
       lontfrmset.laNoUserActivities [lnCntAr+1,2] = laNoUserActivities[lnCntAr,2]
     ENDFOR
   ENDIF
  lontfrmset.AriaForm1.cboPackActivity.Requery()
  lontfrmset.AriaForm1.cboPikActivity.Requery()
  lontfrmset.AriaForm1.cboPikActivity.Value = 'NA'
  lontfrmset.AriaForm1.cboPackActivity.Value = 'NA'
    lontfrmset.AriaForm1.cboPikActivity.Enabled = .T.
  lontfrmset.AriaForm1.cboPackActivity.Enabled = .T.

ENDIF
SELECT (lncuralias)

*!*************************************************************
*! Name      : lfCompleteProject
*! Developer : Mariam Mazhar[MMT]
*! Date      : 07/22/2014
*! Purpose   : Complete Project
*!*************************************************************
FUNCTION lfCompleteProject
LPARAMETERS lcPickticket
IF !USED('syschdul')
  =gfOpenTable('syschdul','Coprusr')
ENDIF
IF !USED('PMPRJDT')
  =gfOpenTable('PMPRJDT','PMPRJDT')
ENDIF
SELECT PMPRJDT
=gfSeek('K'+lcPickticket)
IF !USED('PMPRJHD')
  =gfOpenTable('PMPRJHD','PMPRJHD')
ENDIF
SELECT PMPRJHD
=gfSeek('K'+lcPickticket)
=gfReplace("cprj_stts with 'C'")
ldtpactstrt = PMPRJHD.dclc_strt
ldtpactend  = PMPRJHD.dclc_Fnsh
=gfReplace("dact_strt WITH ldtpactstrt")
=gfReplace("dact_Fnsh WITH ldtpactend")
=gfTableUpdate()

SELECT pmprjdt
SCAN
  lcmcomps = 'Y'
  lcmcompf = 'Y'
  lcmcompd = 'Y'

  IF !EMPTY(pmprjdt.dact_strt)
    lcmcomps = 'N'
  ENDIF
  IF !EMPTY(pmprjdt.dact_fnsh)
    lcmcompf = 'N'
  eNDIF
  IF pmprjdt.nact_dur <> 0
    lcmcompd = 'N'
  ENDIF
  lcmcomplt = lcmcomps + lcmcompf + lcmcompd

  REPLACE dact_strt WITH IIF(lcmcomps = 'N',pmprjdt.dact_strt,pmprjdt.dclc_strt),;
        dact_fnsh WITH IIF(lcmcompf = 'N',pmprjdt.dact_fnsh,pmprjdt.dclc_fnsh),;
        nact_dur  WITH IIF(lcmcompd = 'N',pmprjdt.nact_dur,pmprjdt.nrem_dur)
  =gfReplace('')      
ENDSCAN
=gfTableUpdate()

SELECT syschdul
=gfsetorder('Coprusr')
IF gfseek("K"+lcPickticket)
  SELECT syschdul
  SCAN REST WHILE cconttype+cseqnumber+cstyle+STR(LINENO,6)+ccont_id+coperstat+cuser_id = 'K'+lcPickticket
    REPLACE ccompleted WITH 'Y',;
            coperstat  WITH 'C'
     =gfreplace('')
  ENDSCAN
ENDIF
=gfTableUpdate()

*!*************************************************************
*! Name      : lfCancelProject
*! Developer : Mariam Mazhar[MMT]
*! Date      : 07/22/2014
*! Purpose   : Cancel Project
*!*************************************************************
FUNCTION lfCancelProject
LPARAMETERS lcPickticket

IF !USED('syschdul')
  =gfOpenTable('syschdul','Coprusr')
ENDIF
IF !USED('PMPRJDT')
  =gfOpenTable('PMPRJDT','PMPRJDT')
ENDIF
SELECT PMPRJDT
=gfSeek('K'+lcPickticket)
IF !USED('PMPRJHD')
  =gfOpenTable('PMPRJHD','PMPRJHD')
ENDIF
SELECT PMPRJHD
=gfSeek('K'+lcPickticket)
=gfReplace("cprj_stts with 'X'")
ldtpactstrt = {}
ldtpactend  = {}
=gfReplace("dact_strt WITH ldtpactstrt")
=gfReplace("dact_Fnsh WITH ldtpactend")
=gfTableUpdate()

SELECT pmprjdt
=gfSeek('K'+lcPickticket)
SCAN
  lcmcomps = SUBSTR('YYY',1,1)
  lcmcompf = SUBSTR('YYY',2,1)
  lcmcompd = SUBSTR('YYY',3,1)

  REPLACE dact_strt WITH IIF(lcmcomps = 'N',pmprjdt.dact_strt,{}),;
    dact_fnsh WITH IIF(lcmcompf = 'N',pmprjdt.dact_fnsh,{}),;
    nact_dur  WITH IIF(lcmcompd = 'N',pmprjdt.nact_dur,0)
  =gfReplace('') 
ENDSCAN
=gfTableUpdate()

SELECT syschdul
=gfSeek('K'+lcPickticket)
SCAN REST WHILE cconttype+cseqnumber+cstyle+STR(LINENO,6)+ccont_id+coperstat+cuser_id = 'K'+lcPickticket
  REPLACE coperstat  WITH 'X'
  =gfadd_info('SYSCHDUL')
  =gfreplace("")
ENDSCAN
=gfTableUpdate()

*!*************************************************************
*! Name      : lfSchedul
*! Developer : Mariam Mazhar[MMT]
*! Date      : 07/22/2014
*! Purpose   : Schedule Project
*!*************************************************************
FUNCTION lfSchedul
PARAMETERS lcPickticket
llschedule = .T.
IF !USED('syschdul')
  =gfOpenTable('syschdul','Coprusr')
ENDIF
IF !USED('PMPRJDT')
  =gfOpenTable('PMPRJDT','PMPRJDT')
ENDIF
SELECT PMPRJDT
=gfSeek('K'+lcPickticket)

IF !USED('PMPRJHD')
  =gfOpenTable('PMPRJHD','PMPRJHD')
ENDIF

SELECT PMPRJHD
=gfSeek('K'+lcPickticket)

SELECT pmprjdt
=gfSeek('K'+lcPickticket)



lcprjhd = 'PMPRJHD'
lcprj_id = lcPickticket
lcprj_typ =  "K"
lcstyle= PMPRJHD.cstyle
lnlineno = 0
dsch_date =  oariaapplication.systemdate
lleditaddscr = .F.
IF llschedule
  lcoldalias = SELECT() 
  IF !USED('PMPRJRL_S')
    =gfopentable('PMPRJRL','PMPRJRL','SH','PMPRJRL_S')
  ENDIF
  SELECT 'PMPRJRL_S'
  =gfseek(SUBSTR(lcprj_typ,1,LEN(cprj_typ))+SUBSTR(lcprj_id,1,LEN(cprj_id))+SUBSTR(lcstyle,1,LEN(cstyle))+STR(0,6))
  IF !USED('PMPRJDT_S')
    =gfopentable('PMPRJDT','PMPRJDT','SH','PMPRJDT_S')
  ENDIF
  SELECT 'PMPRJDT_S'
  =gfseek(SUBSTR(lcprj_typ,1,LEN(cprj_typ))+SUBSTR(lcprj_id,1,LEN(cprj_id))+SUBSTR(lcstyle,1,LEN(cstyle))+STR(0,6))
  IF !USED('PMPRJHD_S')
    =gfopentable('PMPRJHD','PMPRJHD','SH','PMPRJHD_S')
  ENDIF
  SELECT 'PMPRJHD_S'
  =gfseek(SUBSTR(lcprj_typ,1,LEN(cprj_typ))+SUBSTR(lcprj_id,1,LEN(cprj_id))+SUBSTR(lcstyle,1,LEN(cstyle))+STR(0,6))
  lleditaddscr = .T.
  lleditadd = .T.
  lctmpprjrl = 'PMPRJRL_S'
  lctmpprjdt = 'PMPRJDT_S'
  lcprjhder = 'PMPRJHD_S'
  SELECT 'PMPRJHD_S'
  =gfreplace("llok_stat WITH .T.")
  =gftableupdate()

  DO lfschedule IN (oariaapplication.applicationhome+'SM\PMPRJSCH.Fxp') WITH;
    lcprj_typ,lcprj_id ,lcstyle  ,lnlineno ,dsch_date

  DIMENSION  latmpval[1]
  latmpval = ''
  SELECT(lctmpprjdt)
  SCAN
    =gfreplace('')
  ENDSCAN
  =gftableupdate()
  SELECT syschdul
  =gftableupdate()
  SELECT(lcprjhder)
  =gfreplace('')
  SELECT MIN(dclc_strt), MAX(dclc_fnsh) ;
    FROM (lctmpprjdt) ;
    WHERE cprj_typ +  cprj_id +  cstyle +STR(LINENO,6)+ coprt_ctg + coprt_id = ;
    SUBSTR(lcprj_typ,1,LEN(&lctmpprjdt..cprj_typ)) + SUBSTR(lcprj_id,1,LEN(&lctmpprjdt..cprj_id)) +;
    SUBSTR(lcstyle,1,LEN(&lctmpprjdt..cstyle)) +STR(lnlineno,6);
    .AND. !lvoid;
    INTO ARRAY latmpval
  IF !EMPTY(latmpval[1])
    m.dclc_strt = latmpval[1]
    m.dclc_fnsh = latmpval[2]
    gfreplace("dclc_strt WITH m.dclc_strt,"+;
      "dclc_fnsh WITH m.dclc_fnsh")
  ENDIF
  SELECT(lcprjhder)
  =gfreplace("llok_stat WITH .F.")
  =gfreplace('')
  =gftableupdate()
  SELECT(lcoldalias)
  SELECT pmprjdt
  lctmpdtfl = 'PMPRJDT'
  =gfsetorder('PMPRJDT')
  =gfseek(SUBSTR(lcprj_typ,1,LEN(cprj_typ))+SUBSTR(lcprj_id,1,LEN(cprj_id))+SUBSTR(lcstyle,1,LEN(cstyle))+STR(0,6))
  SCAN REST WHILE cprj_typ+cprj_id+cstyle+STR(LINENO,6)+coprt_ctg+coprt_id = SUBSTR(lcprj_typ,1,LEN(cprj_typ))+;
      SUBSTR(lcprj_id,1,LEN(cprj_id)) + SUBSTR(lcstyle,1,LEN(cstyle))+STR(0,6);
      FOR lschadtoad
    SELECT pmprjdt
    gfreplace("lSchAdToAd WITH .F.")
  ENDSCAN
ENDIF
SELECT pmprjdt
=gftableupdate()
IF !EMPTY(lcActToCompl)
  SELECT pmprjdt
  =gfSeek('K'+lcPickticket)
  LOCATE REST WHILE cprj_typ+cprj_id+cstyle+STR(LINENO,6)+coprt_ctg+coprt_id ='K'+lcPickticket FOR coprt_ctg+coprt_id = lcActToCompl AND (EMPTY(dact_strt) OR EMPTY(dact_fnsh))
  IF FOUND()
    = gfreplace("dact_strt WITH dclc_strt,"+;
      "dact_fnsh WITH dclc_fnsh")
  ENDIF 
  =gfTableUpdate()
  SELECT syschdul
  =gfsetorder('Coprusr')
  IF gfseek("K"+lcPickticket)
    SELECT syschdul
    SCAN REST WHILE cconttype+cseqnumber+cstyle+STR(LINENO,6)+ccont_id+coperstat+cuser_id = 'K'+lcPickticket FOR ccont_id = lcActToCompl
      REPLACE ccompleted WITH 'Y',;
              coperstat  WITH 'C'
       =gfreplace('')
    ENDSCAN
  ENDIF
  =gfTableUpdate()
  SELECT pmprjRL
  lcOrdOrder = ORDER()
  =gfSetOrder('PMPRJRLP')
  IF gfSeek("K"+lcPickticket+lcstyle+STR(0,6)+lcActToCompl)
    SELECT syschdul
    =gfsetorder('Coprusr')
    IF gfseek("K"+lcPickticket)
      SELECT syschdul
      SCAN REST WHILE cconttype+cseqnumber+cstyle+STR(LINENO,6)+ccont_id+coperstat+cuser_id = 'K'+lcPickticket FOR ccont_id = pmprjRL.coprt_ctg+pmprjRL.coprt_id
        REPLACE lpredcomp with .T.,;
                coperstat WITH 'O'
         =gfreplace('')
      ENDSCAN
    ENDIF
    =gfTableUpdate()
  ENDIF
  SELECT pmprjRL
  gfSetOrder(lcOrdOrder)
ENDIF
*!*************************************************************
*! Name      : lfVUScanserID
*! Developer : Mariam Mazhar[MMT]
*! Date      : 07/22/2014
*! Purpose   : Validate Scanned user ID
*!*************************************************************
FUNCTION lfVUScanserID
PARAMETERS loFormSet
lcUserScanId = loFormSet.AriaForm1.txtUserID.Value
SELECT (loFormSet.lcAssignedAct)
DELETE ALL
IF EMPTY(ALLTRIM(lcUserScanId ))
  RETURN .t.
ENDIF
IF PADR(ALLTRIM(loFormSet.lcDummyUserID),10)==PADR(ALLTRIM(lcUserScanId),10)
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,"Invalid User ID. Please select a valid user.")
  loFormSet.AriaForm1.txtUserID.Value = ''
  loFormSet.AriaForm1.txtUserName.Value = ''
  RETURN .F.
ENDIF

IF !USED('SYUUSER')
  =gfopentable('SYUUSER','Cuser_id','SH')
ENDIF
IF !gfSeek(PADR(ALLTRIM(lcUserScanId),10),'SYUUSER','Cuser_id')
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,"Invalid User ID. Please select a valid user.")
  loFormSet.AriaForm1.txtUserID.Value = ''
  loFormSet.AriaForm1.txtUserName.Value = ''
  RETURN .F.
ENDIF 
loFormSet.AriaForm1.txtUserName.Value = syuuser.cusr_name
= gfSeek('K','PMPRJHD','PMPRJHD') 
SELECT syschdul
=gfsetorder('Coprusr')
IF gfseek("K")
  SELECT syschdul
  SCAN REST WHILE cconttype+cseqnumber+cstyle+STR(LINENO,6)+ccont_id+coperstat+cuser_id = 'K' ;
      FOR CCONT_ID = loFormSet.AriaForm1.cboActivity.Value AND CUSER_ID = PADR(ALLTRIM(lcUserScanId),10) AND !coperstat $ 'CX' AND lpredcomp
    IF Seek('K'+SYSCHDUL.cseqnumber,'PMPRJHD','PMPRJHD') 
      IF PMPRJHD.cprj_stts $ 'CX'
        LOOP
      ELSE
        =gfSeek(SYSCHDUL.cseqnumber,'PIKTKT','PIKTKT')
        =gfSeek('S'+PIKTKT.Account,'CUSTOMER','CUSTOMER')
        =gfSeek('O'+PIKTKT.Order,'Ordline','Ordline')
        =gfSeek('O'+PIKTKT.Order,'Ordhdr','Ordhdr')
        SELECT ORDLINE
        SUM TOTPIK TO lnPick REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = 'O'+PIKTKT.Order FOR PIKTKT = PIKTKT.PIKTKT
        INSERT INTO (loFormSet.lcAssignedAct) VALUES ("P.T.",SYSCHDUL.cseqnumber,PIKTKT.Account,Customer.StName,lnPick,PIKTKT.Status,Ordhdr.Start,Ordhdr.Complete,syschdul.ccont_id)
      ENDIF
    ELSE
      LOOP 
    ENDIF
  ENDSCAN     
ENDIF
SELECT (loFormSet.lcAssignedAct) 
LOCATE 
*lfAssignGridSources (loFormSet)
loFormset.ariaform1.grdAssigned.Refresh ()
loFormset.ariaform1.grdAssigned.AfterRowColChange ()

*!*************************************************************
*! Name      : lfvActivity
*! Developer : Mariam Mazhar[MMT]
*! Date      : 07/22/2014
*! Purpose   : Validate Activity
*!*************************************************************
FUNCTION lfvActivity
PARAMETERS loFormSet
IF loFormSet.AriaForm1.cboActivity.Value <> 'NA'
  loFormSet.AriaForm1.txtUserID.Enabled =.T.
  loFormSet.AriaForm1.txtPickTkt.Enabled =.T.
ELSE  
  loFormSet.AriaForm1.txtUserID.Enabled =.F.
  loFormSet.AriaForm1.txtPickTkt.Enabled =.F.
ENDIF
loFormSet.AriaForm1.txtUserID.Value =''
loFormSet.AriaForm1.txtPickTkt.Value =''
loFormSet.AriaForm1.txtUserName.Value = ''
SELECT (loFormSet.lcAssignedAct)
DELETE ALL
loFormset.ariaform1.grdAssigned.Refresh ()
SELECT (loFormSet.lcUnAssignedAct)
DELETE ALL
*B610880,1 MMT 10/13/2014 Enhance the performance of custom Picking/packing assignment program[Start]
*IF !USED('PMPRJHD_ALL')
*  =gfOpenTable('PMPRJHD','PMPRJHD','SH','PMPRJHD_ALL')    
*ENDIF  
*SELECT PMPRJHD_ALL
*=gfSqlRun("Select * from PMPRJHD[INDEX = PMPRJHD] Where cPRJ_TYP ='K' AND cprj_stts Not in ('C','X') ORDER by CPRJ_ID",'PMPRJHD_ALL')
*B610880,1 MMT 10/13/2014 Enhance the performance of custom Picking/packing assignment program[End]
SELECT syschdul
*B610880,1 MMT 10/13/2014 Enhance the performance of custom Picking/packing assignment program[Start]
*=gfsetorder('Coprusr')
*IF gfseek("K")
lcSyOrder = ORDER()
=gfsetorder('SCHACCT')
IF gfseek("K"+loFormSet.AriaForm1.cboActivity.Value)
*B610880,1 MMT 10/13/2014 Enhance the performance of custom Picking/packing assignment program[End]
  SELECT syschdul
  *B610880,1 MMT 10/13/2014 Enhance the performance of custom Picking/packing assignment program[Start]
*!*    SCAN REST WHILE cconttype+cseqnumber+cstyle+STR(LINENO,6)+ccont_id+coperstat+cuser_id = 'K' ;
*!*        FOR CCONT_ID = loFormSet.AriaForm1.cboActivity.Value AND CUSER_ID = loFormSet.lcDummyUserID AND !coperstat $ 'CX' AND lpredcomp
  SCAN REST WHILE CCONTTYPE+CCONT_ID+STORE+CCOMPLETED+CUSER_ID+DTOS(DTRANDATE)+CTRANTIME+CSEQNUMBER = 'K'+loFormSet.AriaForm1.cboActivity.Value ;
      FOR CUSER_ID = loFormSet.lcDummyUserID AND !coperstat $ 'CX' AND lpredcomp
       
  *B610880,1 MMT 10/13/2014 Enhance the performance of custom Picking/packing assignment program[End]    
    *IF Seek('K'+SYSCHDUL.cseqnumber,'PMPRJHD_ALL') 
      *IF PMPRJHD.cprj_stts $ 'CX'
      *  LOOP
      *ELSE
        =gfSeek(SYSCHDUL.cseqnumber,'PIKTKT','PIKTKT')
        *B610880,1 MMT 10/13/2014 Enhance the performance of custom Picking/packing assignment program[Start]
        IF PIKTKT.Status $ 'CX'
          LOOP 
        ENDIF
        WAIT WINDOW "Checking Picking ticket#: "+SYSCHDUL.cseqnumber NOWAIT 
        *B610880,1 MMT 10/13/2014 Enhance the performance of custom Picking/packing assignment program[End]
        =gfSeek('S'+PIKTKT.Account,'CUSTOMER','CUSTOMER')
        =gfSeek('O'+PIKTKT.Order,'Ordline','Ordline')
        =gfSeek('O'+PIKTKT.Order,'Ordhdr','Ordhdr')
        SELECT ORDLINE
        SUM TOTPIK TO lnPick REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = 'O'+PIKTKT.Order FOR PIKTKT = PIKTKT.PIKTKT
        INSERT INTO (loFormSet.lcUnAssignedAct) VALUES ("P.T.",SYSCHDUL.cseqnumber,PIKTKT.Account,Customer.StName,lnPick,PIKTKT.Status,Ordhdr.Start,Ordhdr.Complete,syschdul.ccont_id)
      *ENDIF
   * ELSE
    *  LOOP 
   * ENDIF
  ENDSCAN     
ENDIF
*B610880,1 MMT 10/13/2014 Enhance the performance of custom Picking/packing assignment program[Start]
WAIT CLEAR  
IF !EMPTY(lcSyOrder)
  SELECT syschdul
  =gfSetOrder(lcSyOrder)
ENDIF  
*B610880,1 MMT 10/13/2014 Enhance the performance of custom Picking/packing assignment program[End]
SELECT (loFormSet.lcUnAssignedAct) 
LOCATE 
*lfAssignGridSources (loFormSet)
loFormset.ariaform1.grdUnAssigned.Refresh ()
loFormset.ariaform1.grdUnAssigned.AfterRowColChange ()
IF loFormSet.AriaForm1.cboActivity.Value <> 'NA'
  loFormSet.AriaForm1.txtUserID.SetFocus()
ENDIF
*B610880,1 MMT 10/13/2014 Enhance the performance of custom Picking/packing assignment program[Start]
IF UPPER(ALLTRIM(loFormSet.AriaForm1.cboActivity.Value)) = ALLTRIM(UPPER(loFormset.lcPackAct))
  loFormset.ariaform1.grdAssigned.Column10.Visible =.f.
ELSE
  loFormset.ariaform1.grdAssigned.Column10.Visible =.f.  
ENDIF
*B610880,1 MMT 10/13/2014 Enhance the performance of custom Picking/packing assignment program[end]
*!*************************************************************
*! Name      : lfVPikTkt
*! Developer : Mariam Mazhar[MMT]
*! Date      : 07/22/2014
*! Purpose   : Validate Picking ticket
*!*************************************************************
FUNCTION lfVPikTkt
PARAMETERS loFormSet
lcPiktkt = loFormSet.AriaForm1.txtPickTkt.Value
IF EMPTY(ALLTRIM(lcPiktkt))
  RETURN .T.
ENDIF 

IF EMPTY(ALLTRIM(loFormSet.AriaForm1.txtUserID.Value))
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,"Please select a valid User ID.")  
  loFormSet.AriaForm1.txtPickTkt.Value =''
  RETURN .T.
ENDIF
IF !USED('PIKTKT')
  =gfopentable('PIKTKT','PIKTKT','SH')
ENDIF
IF !gfSeek(lcPiktkt ,'PIKTKT','PIKTKT') OR (gfSeek(lcPiktkt ,'PIKTKT','PIKTKT') AND PIKTKT.Status <> 'O')
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,"Invalid Picking ticket# . Please select a valid Picking ticket#.")  
  loFormSet.AriaForm1.txtPickTkt.Value =''
  RETURN .F.
ENDIF

IF !gfSeek('K'+lcPiktkt ,'PMPRJHD','PMPRJHD')  OR (gfSeek('K'+lcPiktkt ,'PMPRJHD','PMPRJHD') AND PMPRJHD.cprj_stts $ 'CX')
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,"Invalid Picking ticket# . Please select a valid Picking ticket#.")  
  loFormSet.AriaForm1.txtPickTkt.Value =''
  RETURN .F.
ENDIF 

IF gfSeek('K'+lcPiktkt+ loFormSet.AriaForm1.cboActivity.Value,'PMPRJDT','PMPRJUSR') and  !EMPTY(PMPRJDT.dact_Fnsh)
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,"Activity is complete, Cannot assign.")  
  loFormSet.AriaForm1.txtPickTkt.Value =''
  RETURN .F.
ENDIF
llcomplete =.F.
IF gfSeek('K'+lcPiktkt+ loFormSet.AriaForm1.cboActivity.Value,'PMPRJDT','PMPRJUSR') 
  IF PADR(PMPRJDT.COPRT_RES ,10)<> PADR(loFormSet.AriaForm1.txtUserID.Value,10)
    SELECT PMPRJDT
    =gfReplace("COPRT_RES with '"+loFormSet.AriaForm1.txtUserID.Value+"'")
    =gfTableUpdate()
    SELECT syschdul
    =gfsetorder('Coprusr')
    IF gfseek("K"+lcPiktkt)
      SELECT syschdul
      LOCATE REST WHILE cconttype+cseqnumber+cstyle+STR(LINENO,6)+ccont_id+coperstat+cuser_id = 'K'+lcPiktkt FOR ccont_id = loFormSet.AriaForm1.cboActivity.Value
      IF FOUND()
        REPLACE cuser_id WITH loFormSet.AriaForm1.txtUserID.Value
        =gfreplace('')
        =gfTableUpdate()
      ENDIF  
    ENDIF
    =gfSeek(SYSCHDUL.cseqnumber,'PIKTKT','PIKTKT')
    =gfSeek('S'+PIKTKT.Account,'CUSTOMER','CUSTOMER')
    =gfSeek('O'+PIKTKT.Order,'Ordline','Ordline')
    =gfSeek('O'+PIKTKT.Order,'Ordhdr','Ordhdr')
    SELECT ORDLINE
    SUM TOTPIK TO lnPick REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = 'O'+PIKTKT.Order FOR PIKTKT = PIKTKT.PIKTKT
    INSERT INTO (loFormSet.lcAssignedAct) VALUES ("P.T.",SYSCHDUL.cseqnumber,PIKTKT.Account,Customer.StName,lnPick,PIKTKT.Status,Ordhdr.Start,Ordhdr.Complete,syschdul.ccont_id)
    SELECT (loFormSet.lcUnAssignedAct) 
    =SEEK(lcPiktkt) 
    DELETE 
  ELSE && if assigned to the user , complete it 
    llcomplete =.T.
    ****************
    lcActToCompl = loFormSet.AriaForm1.cboActivity.Value
    lcstyle = REPLICATE('*',loFormSet.lnmajorlen)
    SELECT pmprjdt
    lcPickticket =lcPiktkt
    =gfSeek('K'+lcPickticket)
    LOCATE REST WHILE cprj_typ+cprj_id+cstyle+STR(LINENO,6)+coprt_ctg+coprt_id ='K'+lcPickticket FOR coprt_ctg+coprt_id = lcActToCompl AND (EMPTY(dact_strt) OR EMPTY(dact_fnsh))
    IF FOUND()
      = gfreplace("dact_strt WITH oAriaApplication.SystemDate,"+;
        "dact_fnsh WITH oAriaApplication.SystemDate")
    ENDIF 
    =gfTableUpdate()
    SELECT syschdul
    =gfsetorder('Coprusr')
    IF gfseek("K"+lcPickticket)
      SELECT syschdul
      SCAN REST WHILE cconttype+cseqnumber+cstyle+STR(LINENO,6)+ccont_id+coperstat+cuser_id = 'K'+lcPickticket FOR ccont_id = lcActToCompl
        REPLACE ccompleted WITH 'Y',;
                coperstat  WITH 'C'
         =gfreplace('')
      ENDSCAN
    ENDIF
    =gfTableUpdate()
    SELECT pmprjRL
    lcOrdOrder = ORDER()
    =gfSetOrder('PMPRJRLP')
    IF gfSeek("K"+lcPickticket)
      SELECT pmprjRL
      LOCATE FOR CPRD_CTG+CPRD_ID= lcActToCompl
      IF FOUND()
        SELECT syschdul
        =gfsetorder('Coprusr')
        IF gfseek("K"+lcPickticket)
          SELECT syschdul
          SCAN REST WHILE cconttype+cseqnumber+cstyle+STR(LINENO,6)+ccont_id+coperstat+cuser_id = 'K'+lcPickticket FOR ccont_id = pmprjRL.coprt_ctg+pmprjRL.coprt_id
            REPLACE lpredcomp with .T.,;
                  coperstat WITH 'O'
            =gfreplace('')
          ENDSCAN
        ENDIF
        =gfTableUpdate()
      ENDIF  
    ENDIF
    SELECT pmprjRL
    gfSetOrder(lcOrdOrder)
    SELECT (loFormSet.lcAssignedAct) 
    =SEEK(lcPiktkt) 
    DELETE 
    ****************
  ENDIF  
ENDIF
*B610880,1 MMT 10/13/2014 Enhance the performance of custom Picking/packing assignment program[Start]
IF !llcomplete 
  WAIT WINDOW "Picking Ticket#:"+ALLTRIM(loFormSet.AriaForm1.txtPickTkt.Value) +" has been assigned to "+ALLTRIM(loFormSet.AriaForm1.txtUserID.Value) TIMEOUT  3
ENDIF  
*B610880,1 MMT 10/13/2014 Enhance the performance of custom Picking/packing assignment program[End]
loFormSet.AriaForm1.txtPickTkt.Value = ''
IF UPPER(ALLTRIM(loFormSet.AriaForm1.cboActivity.Value)) <> ALLTRIM(UPPER(loFormset.lcPackAct))
  loFormSet.AriaForm1.txtUserID.Value = ''
  loFormSet.AriaForm1.txtUserName.Value = ''
  SELECT (loFormSet.lcAssignedAct) 
  DELETE ALL
  loFormset.ariaform1.grdAssigned.Refresh ()
  loFormset.ariaform1.grdUnAssigned.Refresh ()
  RETURN .T.
ELSE
  loFormset.ariaform1.grdAssigned.Refresh ()
  loFormset.ariaform1.grdUnAssigned.Refresh ()
  RETURN .F.
ENDIF  

*!*************************************************************
*! Name      : lfOpenPiktkt
*! Developer : Mariam Mazhar[MMT]
*! Date      : 07/22/2014
*! Purpose   : Open Picking ticket screen
*!*************************************************************
FUNCTION lfOpenPLScreen
PARAMETERS lcRecordSource,loFormSet
*B610880,1 MMT 10/13/2014 Enhance the performance of custom Picking/packing assignment program[Start]
IF !USED('Pack_HDR')
  =gfOpenTable('Pack_HDR','Pack_HDR')
ENDIF
llPackFound = .F.
IF gfSeek(EVALUATE(lcRecordSource+".PIKTKT"),'Pack_HDR','Pack_HDR')
  llPackFound = .T.
ENDIF
IF UPPER(ALLTRIM(loFormSet.AriaForm1.cboActivity.Value)) = ALLTRIM(UPPER(loFormset.lcPackAct))
  IF !llPackFound 
    =oAriaApplication.DoProgram('AWRALPLIST',"",.F.,'AL')
    TRY 
      _SCreen.ActiveForm.Parent.Activate()
      _SCreen.ActiveForm.Parent.AriaForm1.Activate()
      _SCreen.ActiveForm.Parent.ChangeMode('A')
      _SCreen.ActiveForm.Parent.AriaForm1.kbPkTktNo.KeyTextBox.Value = EVALUATE(lcRecordSource+".PIKTKT")
      _SCreen.ActiveForm.Parent.AriaForm1.kbPkTktNo.KeyTextBox.Valid
    CATCH
    ENDTRY   
  ELSE
    =oAriaApplication.DoProgram('AWRALPLIST','"'+EVALUATE(lcRecordSource+".PIKTKT")+'"',.F.,'AL')
  ENDIF  
ELSE 
*B610880,1 MMT 10/13/2014 Enhance the performance of custom Picking/packing assignment program[END]
  =oAriaApplication.DoProgram('AWRALPKTKT','"'+EVALUATE(lcRecordSource+".PIKTKT")+'"',.F.,'AL')
*B610880,1 MMT 10/13/2014 Enhance the performance of custom Picking/packing assignment program[Start]
ENDIF
FUNCTION lfOpenPiktkt
PARAMETERS lcRecordSource
=oAriaApplication.DoProgram('AWRALPKTKT','"'+EVALUATE(lcRecordSource+".PIKTKT")+'"',.F.,'AL')
*B610880,1 MMT 10/13/2014 Enhance the performance of custom Picking/packing assignment program[End]
*!*************************************************************
*! Name      : lfStatusDesc
*! Developer : Mariam Mazhar[MMT]
*! Date      : 07/22/2014
*! Purpose   : Get Picking ticket status description
*!*************************************************************
FUNCTION lfStatusDesc
PARAMETERS loFormSet
IF TYPE('Status') ='U'
  RETURN ''
ENDIF
IF Status ='O'
  RETURN 'Open'
ENDIF
IF Status ='H'
  RETURN 'Hold'
ENDIF
IF Status ='P'
  RETURN 'Pulled'
ENDIF
*!*************************************************************
*! Name      : lfRefresh
*! Developer : Mariam Mazhar[MMT]
*! Date      : 07/22/2014
*! Purpose   : Refresh button 
*!*************************************************************
FUNCTION lfRefresh
PARAMETERS loFormSet
lfValidateSetups(loFormSet)
loFormSet.AriaForm1.cboActivity.Value ='NA'
loFormSet.AriaForm1.txtUserID.Value =''
loFormSet.AriaForm1.txtUserName.Value =''
loFormSet.AriaForm1.txtPickTkt.Value =''
SELECT (loFormSet.lcassignedact)
DELETE ALL
SELECT (loFormSet.lcunassignedact)
DELETE ALL
loFormSet.AriaForm1.grdAssigned.Refresh ()
loFormSet.AriaForm1.grdunAssigned.Refresh ()

FUNCTION lfvPackActivity
PARAMETERS loBrFormSet
IF loBrFormSet.AriaForm1.cboPackActivity.Value <> 'NA' AND loBrFormSet.AriaForm1.cboPackActivity.Value == loBrFormSet.AriaForm1.cboPikActivity.Value 
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Picking and Packing Activities are the same, cannot proceed.')
  loBrFormSet.AriaForm1.cboPackActivity.Value = loBrFormSet.AriaForm1.cboPackActivity.OldValue
ENDIF


FUNCTION lfvPickActivity
PARAMETERS loBrFormSet
IF loBrFormSet.AriaForm1.cboPikActivity.Value <> 'NA' AND loBrFormSet.AriaForm1.cboPackActivity.Value == loBrFormSet.AriaForm1.cboPikActivity.Value 
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Picking and Packing Activities are the same, cannot proceed.')
  loBrFormSet.AriaForm1.cboPikActivity.Value = loBrFormSet.AriaForm1.cboPikActivity.OldValue
ENDIF
*B610880,1 MMT 10/13/2014 Enhance the performance of custom Picking/packing assignment program[Start]
FUNCTION gfaudtrl
LPARAMETERS loformset,  lcprog ,  lckey  ,lcapobjnam,  lcevent, lcinform
FUNCTION lfCallProjectScreen
PARAMETERS lcPktktNum
*'K',PIKTKT.PIKTKT,REPLICATE('*',loFormSet.lnmajorlen),0,lcProjectTemplate

IF !USED('PIKTKT')
  =gfopentable('PIKTKT','PIKTKT','SH')
ENDIF
IF !USED('ORDLINE')
  =gfopentable('ORDLINE','ORDLINE','SH')
ENDIF
IF TYPE('loFormProj') <> 'O' OR ISNULL(loFormProj)
  SET PROCEDURE TO (oAriaApplication.ApplicationHome+'MFPROJ.FXP') ADDITIVE 
  DO FORM (oariaapplication.screenhome+ 'MFPROJ.SCX') WITH 'K',lcPktktNum,REPLICATE('*',loFormSet.lnmajorlen),0,lcProjectTemplate,.F.,.F.,.F.  NOSHOW NAME loFormProj
  IF TYPE('loFormProj') = 'O' AND !ISNULL(loFormProj)
    loFormProj.PreferenceName =''
  ENDIF  
ELSE
  TRY 
    loFormProj.ChangeMode("S")
    lfCreateNewProject(loFormProj,'K',lcPktktNum,REPLICATE('*',loFormSet.lnmajorlen),0,lcProjectTemplate,.F.,.F.,.F.)
  CATCH
    SET PROCEDURE TO (oAriaApplication.ApplicationHome+'MFPROJ.FXP') ADDITIVE 
    DO FORM (oariaapplication.screenhome+ 'MFPROJ.SCX') WITH 'K',lcPktktNum,REPLICATE('*',loFormSet.lnmajorlen),0,lcProjectTemplate,.F.,.F.,.F.  NOSHOW NAME loFormProj
    IF TYPE('loFormProj') = 'O' AND !ISNULL(loFormProj)
      loFormProj.PreferenceName =''
    ENDIF  
  ENDTRY  
  
  *loFormProj.INIT('K',lcPktktNum,REPLICATE('*',loFormSet.lnmajorlen),0,lcProjectTemplate,.F.,.F.,.F.)  
  *
ENDIF  

FUNCTION lfCreateNewProject
PARAMETERS loFormSetPj,lcTrPrjType,lcTRPrjID,lcTRPrjSty,lnTrLineNo,lcTempID,llastStr,ldSchedlDate,llShowscreen


loFormSetPj.lctrprjid = lcTRPrjID
loFormSetPj.lctrprjsty = lcTRPrjSty
loFormSetPj.lcTrPrjType = lcTrPrjType
loFormSetPj.lcTempID =lcTempID
loFormSetPj.llshowscreen = llShowscreen

IF TYPE('lnTrLineNo') <> 'N'
  lnTrLineNo = 0
ENDIF  
loFormSetPj.nlinenum = lnTrLineNo
loFormSetPj.trnlinenum =lnTrLineNo 

 
  WITH loFormSetPj.ariaform1
    .cboTranType.Value  = lcTrPrjType
    .cboTranType.InteractiveChange 
    .kbTranNo.keytextbox.Value = PADR(lcTRPrjID,6)
    loFormSetPj.lcStyle = lcTRPrjSty
    loFormSetPj.nlinenum =  loFormSetPj.trnlinenum 
  
    .kbTranNo.sharedvalidation ()
    IF !(loFormSetPj.activemode $ 'AV') 
      IF lnTrLineNo = 0
        .kbColor.keytextbox.Value = STRTRAN(loFormSetPj.lcNonMajPic,'X','*')
      ELSE
        .kbColor.keytextbox.Value = RIGHT(lcTRPrjSty,LEN(loFormSetPj.lcnonmajpic))  
      ENDIF 
    ENDIF  
  
    IF !(loFormSetPj.activemode $ 'AV') 
      .kbstyle.keytextbox.Value = lcTRPrjSty
    ENDIF 
  
    IF !(loFormSetPj.activemode $ 'AV') AND lnTrLineNo <> 0
      .kbstyle.keytextbox.Value = SUBSTR(lcTRPrjSty,1,loFormSetPj.lnmajorlen)
    ENDIF 
    IF !(lcTrPrjType $ 'CDAH') AND  !(loFormSetPj.activemode $ 'AV') 
      .kbstyle.sharedvalidation ()
    ENDIF   
    IF !(loFormSetPj.activemode = 'V') AND lcTrPrjType <> "H" AND !EMPTY(lcTempID)
      .kbTmpl.keytextbox.Value =lcTempID
      .kbTmpl.sharedvalidation ()
    ENDIF 
    IF !(loFormSetPj.activemode = 'V') 
      .chkLstStrt.Value = llastStr
      .chkLstStrt.Valid
    ENDIF
  ENDWITH 
  IF loFormSetPj.activemode = 'A' AND !llShowscreen
    loFormSetPj.ariaform1.cmdTask.Click 
    loFormSetPj.beforesave ()
    
  ENDIF
*ENDIF 
*B610880,1 MMT 10/13/2014 Enhance the performance of custom Picking/packing assignment program[End]