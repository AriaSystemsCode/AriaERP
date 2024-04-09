*!**********************************************************************************************
*! Program file  : MFPJMON.PRG
*! Program desc. : Scheduling Report
*! Screen        : MFPJMON.SCX
*! System        : Aria4
*! Module        : SO, PO, MF
*! Developer     : Ahmad Shoukry Mohammed (ASM)
*! Date          : 08/30/2005
*! Reference     : N039625
*!**********************************************************************************************
*! Parameters    : None
*!*************************************************************
*! Modifications :
*! B131422,1 ASM 03/12/2006 Grid not Maximized and Report Template not working [Start]
*! B607907,1 SSH 12/25/2006 Error if Template code is Empty in OG (T20061123.0001)
*! B608373,1 WAM 12/05/2007 Fix errors in the project monitor screen when click the Task History and Schedule buttons
*! N000632,1 MMT 06/01/2009 Modify Project Monitor Screen As per Ticket[T20080429.0012]
*! B608925,1 MMT 07/27/2009 Increase Number of tasks to be 85 instead of 25 [T20080918.0002]
*! B609028,1 MMT 10/07/2009 Fix bug of not exporting Task notes to excel[T20080429.0012]
*! B609028,2 MMT 10/13/2009 Fix bug of Wrong Order of exporting Task notes to excel[T20080429.0012]
*! B609091,1 MMT 11/18/2009 Add Order Status Field in Browse Fields[T20080429.0012]
*  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [T20091118.0003]
*  E302650,2 MMT 12/24/2009 make project monitor work based on report templated selected [T20091118.0003]
*  E302650,3 MMT 12/29/2009 make project monitor work based on report templated selected [T20091118.0003]
*  B609143,1 MMT 02/11/2010 Error 'Too Many VAriables' in project monitor screen[T20091118.0003]
*  B609143,2 MMT 03/03/2010 Fix problem of PO Screen Toolbar is no working when called from project Monitor[T20091118.0003]
*  B609220,1 MMT 04/26/2010 PO and SO screens toolbar is not working when called from project Monitor[T20080918.0002]
*  E302694,1 MMT 05/16/2010 Add Shortcut to Project Monitor to complete tasks[T20100324.0007]
*  B609286,1 MMT 06/07/2010 SO - project monitor screen not working for style filter [T20100602.0001]
*  B609386,1 MMT 08/25/2010 When user select Notify user from OG it got not record to display[T20100817.0020]
*  B609386,1 MMT 08/25/2010 Error while export to Excel when same color selected twice[T20100823.0021]
*  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[T20101104.0001]
*  B609555,1 MMT 03/27/2011 Task List screen gives error while completing task
*  B609592,1 MMT 06/01/2011 Error while Opening OG and Apvendor is not found in DBF folder{Media}
*  B609631,1 MMT 06/26/2011 Project Monitor should display 1 line for style color in Extended Size Scale companies[T20110620.0022]
*  B609778,1 MMT 12/25/2011 fix bug of not Displaying the Cancelled projects[T20111207.0046]
*! E303030,1 MAB 12/28/2011 Extend File/Field/Index to NN length instead of 8,10,10
*************************************************************************
#INCLUDE R:\Aria4XP\prgS\mfpjmon.h
PRIVATE lcAllVar
lcAllVar = 'llCallScop,llFrstTime,llNoShow,lcWindTitle,lcTempFile,lcTempFld,lnSetUps,'
lcAllVar = lcAllVar + 'lcMajPic, lcPrjTitle, lcRpExp, lcRpNotfy, lcRpOprStt, lcRpOprtyp, lcRpPrjStt, lcRpPrjTp,lcRpTaskNom,llRpShWDy, '+;
  'lcRpRprt, lcRpRptly, lcRpSrtBy, lcRpTemplt, llRpShLate, lcSortBy,lcVarPrj,lcOldValue,LNRPDYCMP,lcVarChr,laTmpltOpr,lcTmpTasks,laFxFltCpy,'
lcAllVar = lcAllVar +  'Sydfield, Style, ORDHDR, ORDLINE, APVENDOR, CUSTOMER, SYUUSER, '+;
  'CUTPICK, POSHDR, POSLN, PMPRJHD, PMPRJDT, PMRPRTM, PMCTGHD,PMPRJNTF,PMCTGDT'

*! B608925,1 MMT 07/27/2009 Increase Number of tasks to be 85 instead of 25 [Start]
lcAllVar = lcAllVar +  ',lcTempFile2,lcTempFile3'
*! B608925,1 MMT 07/27/2009 Increase Number of tasks to be 85 instead of 25 [End]

*  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [Start]
lcAllVar = lcAllVar +  ',LCRPRPTTM,lcLastConts'
*  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [End]

DIMENSION laAllVar[1,1]
STORE '' TO laAllVar
=gfSubStr(lcAllVar,@laAllVar,',')

LOCAL lnI
FOR lnI = 1 TO ALEN(laAllVar,1)
  IF LOWER(LEFT(laAllVar[lnI,1],2)) <> 'la'
    PRIVATE &laAllVar[lnI,1].
  ENDIF
ENDFOR

loFormSet = CREATEOBJECT("Project_Monitor")
lfAddPro(loFormSet)
loFormSet.lcTempFile = gfTempName()
loFormSet.lcTempFld = gfTempName()

*! B608925,1 MMT 07/27/2009 Increase Number of tasks to be 85 instead of 25 [Start]
loFormSet.lcTempFile2= gfTempName()
loFormSet.lcTempFile3= gfTempName()
*! B608925,1 MMT 07/27/2009 Increase Number of tasks to be 85 instead of 25 [End]
*  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [Start]
loFormSet.LCRPRPTTM = 'N/A'
loFormSet.lcLastConts = ''
*  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [End]

SET MULTILOCKS ON
IF !lfOGFiles(@loFormSet)
  RETURN .F.
ENDIF
lfvScope(loFormSet)
*!*  *-- Preparing Screen Variables
*!*  STORE .F. TO llCallScop , llContinue
*!*  lcScrMode = 'V'


*-- Run the Screen
*DO FORM (oAriaApplication.ScreenHome+"\MFPJMON.SCX")

*!*  RETURN
*-- End of Program

*! N000632,1 MMT 06/01/2009 Modify Project Monitor Screen [Start]
*!*  *!*************************************************************
*!*  *! Name      : lfFormInit
*!*  *! Developer : Ahmad Shoukry Mohammed (ASM)
*!*  *! Date      : 08/30/2005
*!*  *! Purpose   : Called from the Init Event of the FormSet
*!*  *!*************************************************************
*!*  *! Parameters: loFormSet
*!*  *!*************************************************************
*!*  *! Returns   : True/False
*!*  *!*************************************************************
*!*  FUNCTION lfFormInit
*!*  LPARAMETERS loFormSet
*!*  LOCAL lnCol, lcCap, lnCount, lcCount, lcTask

*!*  loFormSet.lcCallProg='MFPJMON.FXP'
*!*  =lfAddPro(loFormSet)

*!*  SET MULTILOCKS ON
*!*  IF !lfOGFiles(@loFormSet)
*!*    RETURN .F.
*!*  ENDIF

*!*  loFormSet.llNoShow   = .F.       && Flag to make the screen call the PROCEDURE lpShow every time it runs
*!*  loFormSet.llFrstTime = .T.       && Flag to know if we are going to call lpShow for the first time


*!*  loFormSet.lcWindTitle = 'Project Monitoring'
*!*  lcCap = loFormSet.AriaForm1.Caption
*!*  loFormSet.AriaForm1.Caption = loFormSet.lcWindTitle + SUBSTR(lcCap,AT('/',lcCap)-1)

*!*  loFormSet.llCallScop = .F.

*!*  loFormSet.lc_PrjAudt = gfTempName()

*!*  =gfOpenTable(oAriaApplication.DataDir+'PMPRJDT','PMPRJDTS')

*!*  SELECT PMPRJDT
*!*  =AFIELDS(laFileStru)
*!*  lnFileStru = ALEN(laFileStru, 1)
*!*  DIMENSION laFileStru(lnFileStru + 2, 18)
*!*  laFileStru[lnFileStru +1 ,1] = 'cStatus'
*!*  laFileStru[lnFileStru +1 ,2] = 'C'
*!*  laFileStru[lnFileStru +1 ,3] = 1
*!*  laFileStru[lnFileStru +1 ,4] = 0

*!*  laFileStru[lnFileStru +2,1] = 'nRecNo'
*!*  laFileStru[lnFileStru +2 ,2] = 'N'
*!*  laFileStru[lnFileStru +2 ,3] = 10
*!*  laFileStru[lnFileStru +2 ,4] = 0
*!*  FOR lnCount = lnFileStru+1 TO lnFileStru+2
*!*    STORE .F. TO laFileStru[lnCount,5],laFileStru[lnCount,6]
*!*    STORE "" TO laFileStru[lnCount,7],laFileStru[lnCount,8],laFileStru[lnCount,9],laFileStru[lnCount,10],laFileStru[lnCount,11],laFileStru[lnCount,12],;
*!*                laFileStru[lnCount,13],laFileStru[lnCount,14],laFileStru[lnCount,15],laFileStru[lnCount,16]
*!*    STORE 0 TO laFileStru[lnCount,17],laFileStru[lnCount,18]
*!*  ENDFOR

*!*  CREATE CURSOR (loFormSet.lc_PrjAudt) FROM ARRAY laFileStru
*!*  INDEX ON cOprt_Ctg + cOprt_ID TAG PMPRJDT OF (loFormSet.lc_PrjAudt)

*!*  loFormSet.lcTempFile = gfTempName()
*!*  loFormSet.lcTempFld = gfTempName()
*!*  =lpCreatTmp(loFormSet)
*!*  SELECT (loFormSet.lcTempFile)

*!*  SET CLASSLIB TO (oAriaApplication.ClassDir+"mfpjmon.vcx") ADDITIVE

*!*  WITH loFormSet.AriaForm1.grdProjects
*!*    .RecordSourceType = 1
*!*    .RecordSource = loFormSet.lcTempFile
*!*    .ColumnCount = 0

*!*    .AddColumn(1)
*!*    .Column1.ControlSource = loFormSet.lcTempFile+'.cprj_typ'
*!*    .Column1.AddObject('cntProject','Project')
*!*    .Column1.CurrentControl = 'cntProject'
*!*    .RowHeight = .Column1.cntProject.Height
*!*    .Column1.Width = .Column1.cntProject.Width
*!*    .Column1.Header1.Caption = ''
*!*    .Column1.Sparse = .F.

*!*    FOR lnCount = 2 TO 10
*!*      .AddColumn(lnCount)
*!*      lcTask = ALLTRIM(STR(lnCount-1))
*!*      lcCount = ALLTRIM(STR(lnCount))
*!*      .Column&lcCount..ControlSource = loFormSet.lcTempFile+'.cOprtDsc'+lcTask
*!*      .Column&lcCount..AddObject('cntTask','Task',lcTask)
*!*      .Column&lcCount..CurrentControl = 'cntTask'
*!*      .Column&lcCount..Width = .Column&lcCount..cntTask.Width
*!*      .Column&lcCount..Header1.Caption = ''
*!*      .Column&lcCount..Sparse = .F.
*!*    NEXT
*!*    *.LockColumns = 1
*!*    .mReBuildToolbar()
*!*  ENDWITH
*!*  *ASM, Format the grid [End]
*!*  IF TYPE("loFormSet.AriaForm1.grdProjectsToolBar")="O"
*!*    DECLARE loFormSet.AriaForm1.grdProjectsToolBar.afilefields[2,4]
*!*    loFormSet.AriaForm1.grdProjectsToolBar.afilefields[1,1] = "Porject Status"
*!*    loFormSet.AriaForm1.grdProjectsToolBar.afilefields[1,2] = loFormSet.lcTempFile+".cProjStat"
*!*    loFormSet.AriaForm1.grdProjectsToolBar.afilefields[1,3] = "C"
*!*    loFormSet.AriaForm1.grdProjectsToolBar.afilefields[1,4] = "F"
*!*    loFormSet.AriaForm1.grdProjectsToolBar.afilefields[2,1] = "Task Status"
*!*    loFormSet.AriaForm1.grdProjectsToolBar.afilefields[2,2] = loFormSet.lcTempFile+".cTskSt"
*!*    loFormSet.AriaForm1.grdProjectsToolBar.afilefields[2,3] = "C"
*!*    loFormSet.AriaForm1.grdProjectsToolBar.afilefields[2,4] = "F"
*!*    loFormSet.AriaForm1.grdProjectsToolBar.ncustomcolumncount = 0
*!*  ENDIF

*!*  loFormSet.ChangeMode("V")

*!*  RETURN
*!*  *--end of lfFormInit


*!*************************************************************
*! Name      : lfAddPro
*! Developer : Ahmad Shoukry Mohammed (ASM)
*! Date      : 08/30/2005
*! Purpose   : function to Add properties to the FormSet.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfAddPro
  LPARAMETERS loFormSet


  LOCAL lnI, lnRow, lnCol, lcACopy
  FOR lnI = 1 TO ALEN(laAllVar,1)
    IF LOWER(LEFT(laAllVar[lnI,1],2)) = 'la'
      IF TYPE(laAllVar[lnI,1])='U'
        loFormSet.ADDPROPERTY(laAllVar[lnI,1]+'[1]')
        LOOP
      ENDIF
      lnRow = ALEN(laAllVar[lnI,1],1)
      lnCol = ALEN(laAllVar[lnI,1],2)
      IF lnCol>0
        loFormSet.ADDPROPERTY(laAllVar[lnI,1]+'['+ALLTRIM(STR(lnRow))+;
          ','+ALLTRIM(STR(lnCol))+']')
      ELSE
        loFormSet.ADDPROPERTY(laAllVar[lnI,1]+'['+ALLTRIM(STR(lnRow))+']')
      ENDIF
      lcACopy = '=ACOPY(' + laAllVar[lnI,1] + ',loFormSet.' + laAllVar[lnI,1] + ')'
      &lcACopy.
    ELSE
      loFormSet.ADDPROPERTY(laAllVar[lnI,1],IIF(TYPE(laAllVar[lnI,1])<>'U',EVALUATE(laAllVar[lnI,1]),.F.))
    ENDIF
  ENDFOR

  RETURN
  *--end of lfAddPro


  *!*************************************************************
  *! Name      : lfOGFiles
  *! Developer : Ahmad Shoukry Mohammed (ASM)
  *! Date      : 08/30/2005
  *! Purpose   : To Open files in the Option Grid
  *!*************************************************************
  *! Parameters: loFormSet
  *!*************************************************************
  *! Returns   : True / False
  *!*************************************************************
FUNCTION lfOGFiles
  LPARAMETERS loFormSet

  *gfOpenTable('Sydfield','CFLD_NAME') AND
  *! B609028,1 MMT 10/07/2009 Fix bug of not exporting Task notes to excel[Start]
  *!*  IF !(gfOpenTable('Style','Style') AND ;
  *!*    gfOpenTable('ORDHDR','ORDHDR') AND gfOpenTable('ORDLINE','ORDLINE') AND ;
  *!*    gfOpenTable('APVENDOR','VENCODE') AND gfOpenTable('CUSTOMER','CUSTOMER') AND ;
  *!*    gfOpenTable('SYUUSER','CUSER_ID') AND gfOpenTable('CUTPICK','CUTPICK') AND ;
  *!*    gfOpenTable('POSHDR','POSHDR') AND gfOpenTable('POSLN','POSLN') AND ;
  *!*    gfOpenTable('PMPRJHD','PMPRJHD') AND gfOpenTable('PMPRJDT','PMPRJDTS') AND ;
  *!*    gfOpenTable('PMRPRTM','PMRPRTM') AND gfOpenTable('PMCTGHD','PMCTGHD') AND;
  *!*    gfOpenTable('PMPRJNTF','PMPRJNTF') AND gfOpenTable('PROFVALU','PROFILE') ;
  *!*    AND gfOpenTable('CODES','CCODE_NO'))
  *  B609592,1 MMT 06/01/2011 Error while Opening OG and Apvendor is not found in DBF folder{Start}
  *IF !(gfOpenTable('Style','Style') AND gfOpenTable('PMPTHDT','PMPTHDT') AND gfOpenTable('PMPTHHD','PMPTHHD') AND ;
      gfOpenTable('ORDHDR','ORDHDR') AND gfOpenTable('ORDLINE','ORDLINE') AND ;
      gfOpenTable('APVENDOR','VENCODE') AND gfOpenTable('CUSTOMER','CUSTOMER') AND ;
      gfOpenTable('SYUUSER','CUSER_ID') AND gfOpenTable('CUTPICK','CUTPICK') AND ;
      gfOpenTable('POSHDR','POSHDR') AND gfOpenTable('POSLN','POSLN') AND ;
      gfOpenTable('PMPRJHD','PMPRJHD') AND gfOpenTable('PMPRJDT','PMPRJDTS') AND ;
      gfOpenTable('PMRPRTM','PMRPRTM') AND gfOpenTable('PMCTGHD','PMCTGHD') AND;
      gfOpenTable('PMPRJNTF','PMPRJNTF') AND gfOpenTable('PROFVALU','PROFILE') ;
      AND gfOpenTable('CODES','CCODE_NO'))
  IF !(gfOpenTable('Style','Style') AND gfOpenTable('PMPTHDT','PMPTHDT') AND gfOpenTable('PMPTHHD','PMPTHHD') AND ;
      IIF('SO' $ oAriaApplication.CompanyInstalledModules,gfOpenTable('ORDHDR','ORDHDR') AND gfOpenTable('ORDLINE','ORDLINE'),.T.) AND ;
      IIF('PO' $ oAriaApplication.CompanyInstalledModules OR 'AP' $ oAriaApplication.CompanyInstalledModules OR ;
      'MA' $ oAriaApplication.CompanyInstalledModules OR 'MF' $ oAriaApplication.CompanyInstalledModules,gfOpenTable('APVENDOR','VENCODE'),.T.);
      AND IIF('SO' $ oAriaApplication.CompanyInstalledModules OR 'AL' $ oAriaApplication.CompanyInstalledModules OR;
      'AR' $ oAriaApplication.CompanyInstalledModules,gfOpenTable('CUSTOMER','CUSTOMER'),.T.) AND ;
      gfOpenTable('SYUUSER','CUSER_ID') AND IIF('PO' $ oAriaApplication.CompanyInstalledModules OR 'MF' $ oAriaApplication.CompanyInstalledModules  ,;
      gfOpenTable('CUTPICK','CUTPICK') AND ;
      gfOpenTable('POSHDR','POSHDR') AND gfOpenTable('POSLN','POSLN'),.T.) AND ;
      gfOpenTable('PMPRJHD','PMPRJHD') AND gfOpenTable('PMPRJDT','PMPRJDTS') AND ;
      gfOpenTable('PMRPRTM','PMRPRTM') AND gfOpenTable('PMCTGHD','PMCTGHD') AND;
      gfOpenTable('PMPRJNTF','PMPRJNTF') AND gfOpenTable('PROFVALU','PROFILE') ;
      AND gfOpenTable('CODES','CCODE_NO'))
  *  B609592,1 MMT 06/01/2011 Error while Opening OG and Apvendor is not found in DBF folder{End}      
    *! B609028,1 MMT 10/07/2009 Fix bug of not exporting Task notes to excel[End]
    RETURN .F.
  ENDIF

  *  B609778,1 MMT 12/25/2011 fix bug of not Displaying the Cancelled projects[Start]
  DECLARE laRpSource[4],laRpTarget[3]
  STORE 'Planning'      TO laRpSource[1],laRpTarget[1]  
  STORE 'In progress'     TO laRpSource[2],laRpTarget[2]
  STORE 'Complete'     TO laRpSource[3],laRpTarget[3]
  STORE 'Cancelled' TO laRpSource[4]
  lcRpPrjStt = 'PIC'
  *  B609778,1 MMT 12/25/2011 fix bug of not Displaying the Cancelled projects[End]
  
  *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [Start]
  IF !USED('NOTEPAD')
    =gfOpenTable ('NOTEPAD','NOTEPAD')
  ENDIF
  *Sql SyDField
  lnFielDResult = oAriaApplication.remotesystemdata.execute;
    ("SELECT sydfIEld.cfld_head,sydfIEld.cfld_name,sydfIEld.cdata_typ   ,sydfIEld.nfld_dec ,sydfield.nfld_wdth  FROM SYDFIELD",'','TmpSQLFIELD',"",oAriaApplication.cAria4Sysfiles,3,;
    "",SET("Datasession"))
  IF lnFielDResult > 0
    SELECT TmpSQLFIELD
    =CURSORSETPROP("Buffering" ,3)
    INDEX ON cfld_name TAG 'SQLFIELD'
    FOR lnC = 1 TO 8
      APPEND BLANK
      *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
*!*        REPLACE cfld_name  WITH 'CLCOPN'+STR(lnC,1),;
*!*          cfld_head  WITH 'Open PO Line Qty #'+STR(lnC,1),;
*!*          nfld_dec   WITH  0,;
*!*          nfld_wdth  WITH  11,;
*!*          cdata_typ  WITH  'N'
      REPLACE cfld_name  WITH 'CLCOPN'+STR(lnC,1),;
              cfld_head  WITH LANG_MFPROJMON_OPENLINEQTY+STR(lnC,1),;
              nfld_dec   WITH  0,;
              nfld_wdth  WITH  11,;
              cdata_typ  WITH  'N'
      *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
      APPEND BLANK
      *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
*!*        REPLACE cfld_name  WITH 'CLCREC'+STR(lnC,1),;
*!*          cfld_head  WITH 'Received PO Line Qty #'+STR(lnC,1),;
*!*          nfld_dec   WITH  0,;
*!*          nfld_wdth  WITH  11,;
*!*          cdata_typ  WITH  'N'
      REPLACE cfld_name  WITH 'CLCREC'+STR(lnC,1),;
              cfld_head  WITH LANG_MFPROJMON_RCVLINEQTY+STR(lnC,1),;
              nfld_dec   WITH  0,;
              nfld_wdth  WITH  11,;
              cdata_typ  WITH  'N'
      *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
      APPEND BLANK
      *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
*!*        REPLACE cfld_name  WITH 'CLCINT'+STR(lnC,1),;
*!*          cfld_head  WITH 'In Transit PO Line Qty #'+STR(lnC,1)            ,;
*!*          nfld_dec   WITH  0,;
*!*          nfld_wdth  WITH  11,;
*!*          cdata_typ  WITH  'N'
      REPLACE cfld_name  WITH 'CLCINT'+STR(lnC,1),;
        cfld_head  WITH LANG_MFPROJMON_INTLINEQTY+STR(lnC,1)            ,;
        nfld_dec   WITH  0,;
        nfld_wdth  WITH  11,;
        cdata_typ  WITH  'N'
      *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
    ENDFOR
    APPEND BLANK
    *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
*!*      REPLACE cfld_name  WITH 'CPOSUM',;
*!*        cfld_head  WITH 'Purchase Order Summary',;
*!*        nfld_dec   WITH  0,;
*!*        nfld_wdth  WITH  10,;
*!*        cdata_typ  WITH  'M'
    REPLACE cfld_name  WITH 'CPOSUM',;
            cfld_head  WITH LANG_MFPROJMON_POCTSUMMARY ,;
            nfld_dec   WITH  0,;
            nfld_wdth  WITH  10,;
            cdata_typ  WITH  'M'
      *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]


    APPEND BLANK
      *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
*!*      REPLACE cfld_name  WITH 'CLCTOTOPN',;
*!*        cfld_head  WITH 'Open PO Line TotQty',;
*!*        nfld_dec   WITH  0,;
*!*        nfld_wdth  WITH  11,;
*!*        cdata_typ  WITH  'N'
    REPLACE cfld_name  WITH 'CLCTOTOPN',;
            cfld_head  WITH LANG_MFPROJMON_OPENLINETOT,;
            nfld_dec   WITH  0,;
            nfld_wdth  WITH  11,;
            cdata_typ  WITH  'N'
      *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]

    APPEND BLANK
   *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
*!*      REPLACE cfld_name  WITH 'CLCTOTINT',;
*!*        cfld_head  WITH 'In Tran. PO Line TotQty',;
*!*        nfld_dec   WITH  0,;
*!*        nfld_wdth  WITH  11,;
*!*        cdata_typ  WITH  'N'
    REPLACE cfld_name  WITH 'CLCTOTINT',;
            cfld_head  WITH LANG_MFPROJMON_INTLINETOT,;
            nfld_dec   WITH  0,;
            nfld_wdth  WITH  11,;
            cdata_typ  WITH  'N'
   *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]

    APPEND BLANK
    *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
*!*      REPLACE cfld_name  WITH 'CLCTOTREC',;
*!*        cfld_head  WITH 'Total Rec. PO Line Qty'   ,;
*!*        nfld_dec   WITH  0,;
*!*        nfld_wdth  WITH  11,;
*!*        cdata_typ  WITH  'N'
    REPLACE cfld_name  WITH 'CLCTOTREC',;
            cfld_head  WITH LANG_MFPROJMON_RCVLINETO  ,;
            nfld_dec   WITH  0,;
            nfld_wdth  WITH  11,;
            cdata_typ  WITH  'N'
    APPEND BLANK 
    REPLACE cfld_name  WITH 'DPORCDAT',;
            cfld_head  WITH LANG_MFPROJMON_RCVDATE,;
            nfld_dec   WITH  0,;
            nfld_wdth  WITH  8,;
            cdata_typ  WITH  'D'            
    *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
    APPEND BLANK
    *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
*!*      REPLACE cfld_name  WITH 'MUSRCOMMPO',;
*!*        cfld_head  WITH 'Purchase Order Notes'   ,;
*!*        nfld_dec   WITH  0,;
*!*        nfld_wdth  WITH  10,;
*!*        cdata_typ  WITH  'M'
    REPLACE cfld_name  WITH 'MUSRCOMMPO',;
            cfld_head  WITH LANG_MFPROJMON_POCTNOTES   ,;
            nfld_dec   WITH  0,;
            nfld_wdth  WITH  10,;
            cdata_typ  WITH  'M'
      *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]

  ENDIF

  *Fox
  lnFielDResult = oAriaApplication.remotesystemdata.execute;
    ("SELECT sydfIEld.cfld_head,sydfIEld.cfld_name,sydfIEld.nfld_dec ,sydfIEld.cdata_typ ,sydfield.nfld_wdth FROM SYDFIELD",'','TmpFoxFIELD',"",oAriaApplication.SystemConnectionString,3,;
    "",SET("Datasession"))
  IF lnFielDResult > 0
    SELECT TmpFoxFIELD
    =CURSORSETPROP("Buffering" ,3)
    INDEX ON cfld_name TAG 'FoxFIELD'
    APPEND BLANK
    *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
*!*      REPLACE cfld_name  WITH 'CSOSUM',;
*!*        cfld_head  WITH 'Sales Order Summary',;
*!*        nfld_dec   WITH  0,;
*!*        nfld_wdth  WITH  10,;
*!*        cdata_typ  WITH  'M'
    REPLACE cfld_name  WITH 'CSOSUM',;
      cfld_head  WITH LANG_MFPROJMON_SOSUMMARY,;
      nfld_dec   WITH  0,;
      nfld_wdth  WITH  10,;
      cdata_typ  WITH  'M'
    *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
    APPEND BLANK
    *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]    
*!*      REPLACE cfld_name  WITH 'MUSRCOMMSO',;
*!*        cfld_head  WITH 'Sales Order Notes'                ,;
*!*        nfld_dec   WITH  0,;
*!*        nfld_wdth  WITH  10,;
*!*        cdata_typ  WITH  'M'
    REPLACE cfld_name  WITH 'MUSRCOMMSO',;
            cfld_head  WITH LANG_MFPROJMON_SONOTES               ,;
            nfld_dec   WITH  0,;
            nfld_wdth  WITH  10,;
            cdata_typ  WITH  'M'
    *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]


  ENDIF




  DIMENSION laTmpVal[1],laTmpDes[1]
  STORE '' TO laTmpVal,laTmpDes
  laTmpVal[1] = 'N/A'
  laTmpDes[1] = 'N/A'

  DIMENSION laPrjDesc[2],laPrjVals[2]
  STORE '' TO laPrjDesc,laPrjVals
  laPrjDesc[1] = LANG_MFPROJMON_STYLE
  laPrjVals[1] = 'S'
  laPrjDesc[2] = LANG_MFPROJMON_OTHER
  laPrjVals[2] = 'H'
  lnValCnt = 3
  IF 'MF' $ oAriaApplication.CompanyInstalledModules
    DIMENSION laPrjDesc[ALEN(laPrjDesc)+3],laPrjVals[ALEN(laPrjVals)+3]
    laPrjDesc[lnValCnt] = LANG_MFPROJMON_CTKTKT
    laPrjVals[lnValCnt] = 'C'
    lnValCnt = lnValCnt + 1
    laPrjDesc[lnValCnt] = LANG_MFPROJMON_ADORORDER
    laPrjVals[lnValCnt] = 'A'
    lnValCnt = lnValCnt + 1
    laPrjDesc[lnValCnt] = LANG_MFPROJMON_DYEORDER
    laPrjVals[lnValCnt] = 'D'
    lnValCnt = lnValCnt + 1
  ENDIF

  IF 'PO' $ oAriaApplication.CompanyInstalledModules
    DIMENSION laPrjDesc[ALEN(laPrjDesc)+3],laPrjVals[ALEN(laPrjVals)+3]
    laPrjDesc[lnValCnt] = LANG_MFPROJMON_PO
    laPrjVals[lnValCnt] = 'P'
    lnValCnt = lnValCnt + 1
    laPrjDesc[lnValCnt] = LANG_MFPROJMON_INTRPO
    laPrjVals[lnValCnt] = 'N'
    lnValCnt = lnValCnt + 1
    laPrjDesc[lnValCnt] = LANG_MFPROJMON_RETPO
    laPrjVals[lnValCnt] = 'R'
    lnValCnt = lnValCnt + 1
  ENDIF

  IF 'SO' $ oAriaApplication.CompanyInstalledModules
    DIMENSION laPrjDesc[ALEN(laPrjDesc)+2],laPrjVals[ALEN(laPrjVals)+2]
    laPrjDesc[lnValCnt] = LANG_MFPROJMON_SALORD
    laPrjVals[lnValCnt] = 'O'
    lnValCnt = lnValCnt + 1
    laPrjDesc[lnValCnt] =  LANG_MFPROJMON_TMPEDI
    laPrjVals[lnValCnt] = 'T'
    lnValCnt = lnValCnt + 1
  ENDIF


  IF 'MA' $ oAriaApplication.CompanyInstalledModules
    DIMENSION laPrjDesc[ALEN(laPrjDesc)+1],laPrjVals[ALEN(laPrjVals)+1]
    laPrjDesc[lnValCnt] = LANG_MFPROJMON_MATERIAL
    laPrjVals[lnValCnt] = 'M'
  ENDIF
  IF !USED('PMRPRTM')
    =gfOpenTable('PMRPRTM','PMRPRTM','SH')  && Contains Templates
  ENDIF
  IF !USED('PMRPTMDT')
    =gfOpenTable("PMRPTMDT","PMRPTMDS", "SH")  && Contains Templates Details
  ENDIF
  IF !USED('POSLN_PO')
    gfOpenTable('POSLN','POSLN','SH','POSLN_PO') 
  ENDIF   
  *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [End]

  IF TYPE('loFormSet') <> "O"
    gfOpenTable('PMCTGDT','PMCTGDT','SH',lcTmpTasks)
    SELECT (lcTmpTasks)
    gfSeek('')
  ENDIF

  
  IF TYPE('lcRpPrjTp') = 'C' AND TYPE('loogscroll') = 'O'
    lfvPrjTyp(.T.)
  ENDIF
  RETURN
  *-- End of lfOGFiles


  *!*************************************************************
  *! Name      : lfOpenFile
  *! Developer : Ahmad Shoukry Mohammed (ASM)
  *! Date      : 08/30/2004
  *! Purpose   : To Open files and store name of files opened
  *!*************************************************************
  *! Parameters: loFormSet, lcFile, lcTag
  *!*************************************************************
  *! Returns   : True / False
  *!*************************************************************
FUNCTION lfOpenFile
  PARAMETERS loFormSet, lcFile, lcTag
  LOCAL lcDir, lcProp, llRetVal

  IF TYPE('loFormSet')<>'O'
    lcDir = IIF(UPPER(LEFT(lcFile,2))=='SY',oAriaApplication.SysPath,oAriaApplication.DataDir)
    llRetVal = gfOpenTable(lcDir+lcFile,lcTag)
  ELSE
    lcProp = lcFile
    IF TYPE('loFormSet.'+lcProp)<>'O'
      loFormSet.&lcProp = CREATEOBJECT("RemoteTable",lcFile,lcTag,lcFile,loFormSet.DATASESSIONID)
    ENDIF
    llRetVal = TYPE('loFormSet.'+lcProp)='O'
  ENDIF


  RETURN llRetVal
  *-- End of lfOpenFile


  *!*  *!***************************************************************************
  *!*  *! Name      : lfAddGridCol
  *!*  *! Developer : Ahmad Shoukry Mohammed (ASM)
  *!*  *! Date      : 12/19/2001
  *!*  *! Purpose   : Function to Add a Column to the Grid.
  *!*  *!***************************************************************************
  *!*  *! Parameters: Grd, lnCol, lcControlSource, lcCaption, lnHdrAligment, lnWidth, lcInputMask
  *!*  *!***************************************************************************
  *!*  FUNCTION lfAddGridCol
  *!*  LPARAMETERS grd, lnCol, lcControlSource, lcCaption, lnHdrAligment, lnWidth, lcInputMask
  *!*  LOCAL lcCol

  *!*  lnCol = lnCol +1
  *!*  lcCol = ALLTRIM(STR(lnCol))
  *!*  WITH grd
  *!*    .column&lcCol..ControlSource  = lcControlSource
  *!*    .column&lcCol..Header1.Caption = lcCaption
  *!*    .column&lcCol..Header1.Alignment = IIF(TYPE('lnHdrAligment')='N',lnHdrAligment,0)
  *!*    .column&lcCol..Width = IIF(TYPE('lnWidth')='N',lnWidth,75)
  *!*    IF TYPE('lcInputMask')='C'
  *!*      .column&lcCol..InputMask = lcInputMask
  *!*    ENDIF
  *!*  ENDWITH

  *!*  RETURN
  *!*  *-- End of lfAddGridCol


  *!*  *!*************************************************************
  *!*  *! Name      : lfFormDestroy
  *!*  *! Developer : Ahmad Shoukry Mohammed (ASM)
  *!*  *! Date      : 08/30/2005
  *!*  *! Purpose   : Called from the Destroy Event of the FormSet
  *!*  *!*************************************************************
  *!*  *! Parameters: loFormSet
  *!*  *!*************************************************************
  *!*  *! Returns   : True/False
  *!*  *!*************************************************************
  *!*  FUNCTION lfFormDestroy
  *!*  LPARAMETERS loFormSet

  *!*  RETURN
  *--end of lfFormDestroy


  *!***************************************************************************
  *! Name      : lpShow
  *! Developer : Ahmad Shoukry Mohammed (ASM)
  *! Date      : 08/30/2005
  *! Purpose   : Called from the Refresh event of the FormSet
  *!***************************************************************************
  *! Parameters: loFormSet
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
  *!*  PROCEDURE lpShow
  *!*  LPARAMETERS loFormSet


  *!*  IF loFormSet.llCallScop
  *!*    IF !loFormSet.llFrstTime
  *!*      =lfvScope(loFormSet)
  *!*      loFormSet.llCallScop = .F.
  *!*    ENDIF
  *!*  ENDIF

  *!*  loFormSet.llFrstTime = .F.

  *!*  loFormSet.AriaForm1.grdProjects.Refresh()
  *!*  WITH loFormset.AriaForm1
  *!*    STORE .T. TO .cmdSearch.Enabled, .cmdRefresh.Enabled, .cmdHistory.Enabled, .cmdReschedule.Enabled
  *!*  ENDWITH

  *!*  RETURN
  *!*  *-- End of lpShow

  *!***************************************************************************
  *! Name      : lpCreatTmp
  *! Developer : Ahmad Shoukry Mohammed (ASM)
  *! Date      : 08/30/2005
  *! Purpose   : Function To Create the temp file used in the Grid.
  *!***************************************************************************
  *! Parameters: loFormSet
  *!***************************************************************************
  *! Returns   : None
  *!***************************************************************************
  *!*  PROCEDURE lpCreatTmp
  *!*  LPARAMETERS loFormSet
  *!*  LOCAL lNFileStru, lnCount, lcNum
  *!*  LOCAL ARRAY laFileStru[1,6]

  *!*  #DEFINE lnMaxTasks 20
  *!*  IF EMPTY(loFormSet.AriaForm1.grdProjects.RecordSource)

  *!*    =AFIELDS(laFileStru,'PMPRJHD')
  *!*    lfAddFldtoArray(@laFileStru,'Partner','C',8,0)
  *!*    lfAddFldtoArray(@laFileStru,'cProjStat','C',8,0)
  *!*    lfAddFldtoArray(@laFileStru,'cProjType','C',20,0)
  *!*    lfAddFldtoArray(@laFileStru,'cTskSt','C',20,0)
  *!*    FOR lnCount=1 TO lnMaxTasks
  *!*      lcNum = ALLTRIM(STR(lnCount))
  *!*      lfAddFldtoArray(@laFileStru,'cOprtDsc' + lcNum,'C',40,0)
  *!*      lfAddFldtoArray(@laFileStru,'dEstStrt' + lcNum,'D',8,0)
  *!*      lfAddFldtoArray(@laFileStru,'dEstFnsh' + lcNum,'D',8,0)
  *!*      lfAddFldtoArray(@laFileStru,'dClcStrt' + lcNum,'D',8,0)
  *!*      lfAddFldtoArray(@laFileStru,'dClFnh' + lcNum,'D',8,0)
  *!*      lfAddFldtoArray(@laFileStru,'dActStrt' + lcNum,'D',8,0)
  *!*      lfAddFldtoArray(@laFileStru,'dAcFnh' + lcNum,'D',8,0)
  *!*    NEXT

  *!*  =gfCrtTmp(loFormSet.lcTempFile,@laFileStru)





  *!*  SELECT(loFormSet.lcTempFile)

  *!*  *!*    DO CASE
  *!*  *!*      CASE loFormSet.lcRpSrtBy $ 'OC'
  *!*  *!*        lcSortBy = 'CPRJ_TYP+cPrj_Id'
  *!*  *!*      CASE loFormSet.lcRpSrtBy = 'R'
  *!*  *!*        lcSortBy = 'CPRJ_TYP+DTOS(dReq_Fnsh)+cPrj_Id'
  *!*  *!*      CASE loFormSet.lcRpSrtBy $ 'A'
  *!*  *!*        lcSortBy = 'CPRJ_TYP+ACCOUNT+cPrj_Id'
  *!*  *!*      CASE loFormSet.lcRpSrtBy $ 'V'
  *!*  *!*        lcSortBy = 'CPRJ_TYP+VENDOR+cPrj_Id'
  *!*  *!*      CASE loFormSet.lcRpSrtBy $ 'SM'
  *!*  *!*       IF lcRpPrjTp = 'S'
  *!*  *!*         lcSortBy = 'CPRJ_TYP+cStyle'
  *!*  *!*       ELSE
  *!*  *!*         lcSortBy = 'CPRJ_TYP+cPrj_Id'
  *!*  *!*       ENDIF
  *!*  *!*    ENDCASE
  *!*  *!*     INDEX on &lcSortBy TAG  (loFormSet.lcTempFile)
  *!*  *!*    CURSORSETPROP("Buffering" ,5)
  *!*  *!*

  *!*  ENDIF

  *!*  SELECT (loFormSet.lcTempFile)
  *!*  loFormSet.nworkarea = ALIAS()
  *!*  loFormSet.DataEnvironment.InitialSelectedAlias = ALIAS()


  *!*  RETURN
  *!*  *-- End of lpCreatTmp


  *!***************************************************************************
  *! Name      : lfAddFldtoArray
  *! Developer : Ahmad Shoukry Mohammed (ASM)
  *! Date      : 08/30/2005
  *! Purpose   : Function To Add Fld Data to the Structure's Array
  *!***************************************************************************
  *! Parameters: laArray, lcFldName, lcFldType, lnFldLen, lnFldDec
  *!***************************************************************************
  *! Returns   : None
  *!***************************************************************************
PROCEDURE lfAddFldtoArray
  LPARAMETERS laArray, lcFldName, lcFldType, lnFldLen, lnFldDec
  LOCAL lnIndex

  lnIndex = ALEN(laArray,1)+1
  DIMENSION laArray[lnIndex,18]

  laArray[lnIndex,1] = lcFldName
  laArray[lnIndex,2] = lcFldType
  laArray[lnIndex,3] = lnFldLen
  laArray[lnIndex,4] = lnFldDec

  STORE '' TO laArray[lnIndex ,7],laArray[lnIndex,8],;
    laArray[lnIndex,9] ,laArray[lnIndex ,10],;
    laArray[lnIndex,11],laArray[lnIndex,12],;
    laArray[lnIndex,13],laArray[lnIndex,14],;
    laArray[lnIndex,15],laArray[lnIndex,16]

  STORE 0 TO  laArray[lnIndex,17],laArray[lnIndex,18]

  RETURN
  *-- End of lfAddFldtoArray



  *!***************************************************************************
  *! Name      : lfvScope
  *! Developer : Ahmad Shoukry Mohammed (ASM)
  *! Date      : 08/30/2005
  *! Purpose   : Calls the Option Grid and acts according to its returned expression
  *!***************************************************************************
  *! Parameters: loFormSet
  *!***************************************************************************
  *! Returns   : None
  *!***************************************************************************
FUNCTION lfvScope
  PARAMETERS loFormSet
  LOCAL lcTemp
  PRIVATE loParentForm
  loParentForm = loFormSet

  WITH loFormSet

    lcPrjTitle = .lcPrjTitle
    lcMajPic   = .lcMajPic
    lcRpNotfy  = .lcRpNotfy
    lcRpOprStt = .lcRpOprStt
    lcRpOprtyp = .lcRpOprtyp
    lcRpPrjStt = .lcRpPrjStt
    lcRpPrjTp  = .lcRpPrjTp
    lcRpRprt   = .lcRpRprt
    lcRpRptly  = .lcRpRptly
    lcRpSrtBy  = .lcRpSrtBy
    lcRpTemplt = .lcRpTemplt
    llRpShLate = .llRpShLate
    lcSortBy   = .lcSortBy
    lcOldValue = .lcOldValue
    LNRPDYCMP = .LNRPDYCMP
    lcVarChr = .lcVarChr
    laTmpltOpr= .laTmpltOpr
    lcTmpTasks = .lcTmpTasks
    lcRpTaskNom = .lcRpTaskNom
    llRpShWDy =.llRpShWDy
    lnSetUps = .lnSetUps
    lcVarPrj  = .lcVarPrj
    DIMENSION loFormSet.laFxFltCpy[1]
    DIMENSION laSortDesc[IIF(oAriaApplication.ActiveModuleID= "MF" , 2 , 3) ,1]
    DIMENSION laSortVal[IIF(oAriaApplication.ActiveModuleID= "MF" , 2 , 3),1]
    *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [Start]
    LCRPRPTTM = 'N/A'
    DIMENSION laTmpVal[1],laTmpDes[1]
    STORE '' TO laTmpVal,laTmpDes
    laTmpVal[1] = 'N/A'
    laTmpDes[1] = 'N/A'
    
    *  B609778,1 MMT 12/25/2011 fix bug of not Displaying the Cancelled projects[Start]
    lnRpStatus = 0
    DIMENSION LARPTARGET[4],laRpSource[4]
    STORE '' TO LARPTARGET,laRpSource
    *  B609778,1 MMT 12/25/2011 fix bug of not Displaying the Cancelled projects[END]    
    
    DIMENSION laPrjDesc[2],laPrjVals[2]
    STORE '' TO laPrjDesc,laPrjVals
    *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [End]
    .lcRpExp   = gfOpGrid('MFPJMON', .T., .F., .F., .T.)
    *.lcRpExp   = gfOpGrid('MFPJMON', .T.)
    IF UPPER(.lcRpExp)=='.F.'
      RETURN
    ENDIF
    *! B609028,2 MMT 10/13/2009 Fix bug of Wrong Order of exporting Task notes to excel[Start]
    *B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
    *WAIT WINDOW 'Collecting Data...' NOWAIT    
    WAIT WINDOW LANG_MFPROJMON_COLLECTDATA  NOWAIT
    *B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
    *! B609028,2 MMT 10/13/2009 Fix bug of Wrong Order of exporting Task notes to excel[End]

    lcTaskRef = gfTempName()
    CREATE CURSOR (lcTaskRef) (COPRT_CTG C(3),COPRT_ID C(5),nNum N(5))
    SELECT (lcTaskRef)
    INDEX ON  COPRT_CTG +COPRT_ID TAG (lcTaskRef)
    lnTaskCnt = 0

    .lcSortBy = lcSortBy
    .lcRpSrtBy = lcRpSrtBy

    *! B608925,1 MMT 07/27/2009 Increase Number of tasks to be 85 instead of 25 [Start]
    *Looking for the first 3 SO profiles and First PO profiles
    lcSOProf1 = ''
    lcSOProf2 = ''
    lcSOProf3 = ''
    lcSOProf1DESC = ''
    lcSOProf2DESC = ''
    lcSOProf3DESC = ''

    lcPOProf1 = ''
    lcPOProf2 = ''
    lcPOProf3 = ''
    lcPOProf1DESC = ''
    lcPOProf2DESC = ''
    lcPOProf3DESC = ''
    SELECT Codes
    =gfSeek('N'+'CPRO_CODE')
    SCAN REST WHILE CDEFCODE+cfld_name+CCODE_NO+CDISCREP+CRLTD_NAM = 'N'+'CPRO_CODE' FOR;
        ALLTRIM(Codes.CRLTD_NAM) ='CPRO_TYPE' AND  ALLTRIM(Codes.crltd_vlu)= 'ST'

      IF !EMPTY(lcPOProf1) AND !EMPTY(lcPOProf2) AND !EMPTY(lcPOProf3) AND;
          !EMPTY(lcSOProf1) AND !EMPTY(lcSOProf2) AND !EMPTY(lcSOProf3)
        EXIT
      ENDIF

      lnCodeRec = RECNO()
      lcCodeNo = Codes.CCODE_NO

      IF (EMPTY(lcPOProf1) OR EMPTY(lcPOProf2) OR EMPTY(lcPOProf3))
        =gfSeek('N'+'CPRO_CODE '+lcCodeNo)
        SCAN REST WHILE CDEFCODE+cfld_name+CCODE_NO+CDISCREP+CRLTD_NAM = 'N'+'CPRO_CODE '+lcCodeNo  FOR;
            ALLTRIM(Codes.CRLTD_NAM) ='CPRO_SCR' AND  ALLTRIM(Codes.crltd_vlu)= 'PO'


          DO CASE
            CASE EMPTY(lcPOProf1)
              lcPOProf1 = Codes.CCODE_NO
              =gfSeek('N'+'CPRO_CODE '+lcCodeNo)
              LOCATE REST WHILE CDEFCODE+cfld_name+CCODE_NO+CDISCREP+CRLTD_NAM = 'N'+'CPRO_CODE '+lcCodeNo  FOR Codes.crltfield = 'N'
              IF FOUND()
                lcPOProf1DESC  = Codes.CDISCREP
              ENDIF
              EXIT

            CASE EMPTY(lcPOProf2)
              lcPOProf2 = Codes.CCODE_NO
              =gfSeek('N'+'CPRO_CODE '+lcCodeNo)
              LOCATE REST WHILE CDEFCODE+cfld_name+CCODE_NO+CDISCREP+CRLTD_NAM = 'N'+'CPRO_CODE '+lcCodeNo  FOR Codes.crltfield = 'N'
              IF FOUND()
                lcPOProf2DESC  = Codes.CDISCREP
              ENDIF
              EXIT

            CASE EMPTY(lcPOProf3)
              lcPOProf3 = Codes.CCODE_NO
              =gfSeek('N'+'CPRO_CODE '+lcCodeNo)
              LOCATE REST WHILE CDEFCODE+cfld_name+CCODE_NO+CDISCREP+CRLTD_NAM = 'N'+'CPRO_CODE '+lcCodeNo  FOR Codes.crltfield = 'N'
              IF FOUND()
                lcPOProf3DESC  = Codes.CDISCREP
              ENDIF
              EXIT

          ENDCASE
        ENDSCAN
      ENDIF

      IF BETWEEN(lnCodeRec,1,RECCOUNT())
        GO RECORD lnCodeRec
      ENDIF

      IF (EMPTY(lcSOProf1) OR EMPTY(lcSOProf2) OR EMPTY(lcSOProf3))
        =gfSeek('N'+'CPRO_CODE '+lcCodeNo)
        SCAN REST WHILE CDEFCODE+cfld_name+CCODE_NO+CDISCREP+CRLTD_NAM = 'N'+'CPRO_CODE '+lcCodeNo  FOR;
            ALLTRIM(Codes.CRLTD_NAM) ='CPRO_SCR' AND  ALLTRIM(Codes.crltd_vlu)= 'SO'

          DO CASE
            CASE EMPTY(lcSOProf1)
              lcSOProf1 = Codes.CCODE_NO
              =gfSeek('N'+'CPRO_CODE '+lcCodeNo)
              LOCATE REST WHILE CDEFCODE+cfld_name+CCODE_NO+CDISCREP+CRLTD_NAM = 'N'+'CPRO_CODE '+lcCodeNo  FOR Codes.crltfield = 'N'
              IF FOUND()
                lcSOProf1DESC  = Codes.CDISCREP
              ENDIF
              EXIT

            CASE EMPTY(lcSOProf2)
              lcSOProf2 = Codes.CCODE_NO
              =gfSeek('N'+'CPRO_CODE '+lcCodeNo)
              LOCATE REST WHILE CDEFCODE+cfld_name+CCODE_NO+CDISCREP+CRLTD_NAM = 'N'+'CPRO_CODE '+lcCodeNo  FOR Codes.crltfield = 'N'
              IF FOUND()
                lcSOProf2DESC  = Codes.CDISCREP
              ENDIF
              EXIT

            CASE EMPTY(lcSOProf3)
              lcSOProf3 = Codes.CCODE_NO
              =gfSeek('N'+'CPRO_CODE '+lcCodeNo)
              LOCATE REST WHILE CDEFCODE+cfld_name+CCODE_NO+CDISCREP+CRLTD_NAM = 'N'+'CPRO_CODE '+lcCodeNo  FOR Codes.crltfield = 'N'
              IF FOUND()
                lcSOProf3DESC  = Codes.CDISCREP
              ENDIF
              EXIT

          ENDCASE
        ENDSCAN
      ENDIF
      IF BETWEEN(lnCodeRec,1,RECCOUNT())
        GO RECORD lnCodeRec
      ENDIF
    ENDSCAN
    *! B608925,1 MMT 07/27/2009 Increase Number of tasks to be 85 instead of 25 [End]

    *! B609028,1 MMT 10/07/2009 Fix bug of not exporting Task notes to excel[Start]
    DIMENSION laPoCustFld[1,2]
    STORE '' TO laPoCustFld
    lcNomDesc = ''
    *! B609028,1 MMT 10/07/2009 Fix bug of not exporting Task notes to excel[End]

    *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [Start]
    lcBrFields = ''
    *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [End]

    IF !lfCreateTemp(loFormSet)
      RETURN .F.
    ENDIF
    .lcPrjTitle = lcPrjTitle
    .lcMajPic = lcMajPic
    .lcRpNotfy = lcRpNotfy
    .lcRpOprStt = lcRpOprStt
    .lcRpOprtyp = lcRpOprtyp
    .lcRpPrjStt = lcRpPrjStt
    .lcRpPrjTp = lcRpPrjTp
    .lcRpRprt = lcRpRprt
    .lcRpRptly = lcRpRptly
    .lcRpSrtBy = lcRpSrtBy
    .lcRpTemplt = lcRpTemplt
    .llRpShLate = llRpShLate
    .lcSortBy = lcSortBy
    .lcOldValue = lcOldValue
    .lnSetUps = lnSetUps
    .LNRPDYCMP  = LNRPDYCMP
    .lcVarChr     = lcVarChr
    .laTmpltOpr = laTmpltOpr
    .lcTmpTasks = lcTmpTasks
    .lcRpTaskNom = lcRpTaskNom
    .llRpShWDy = llRpShWDy
    .lcVarPrj  = lcVarPrj
    *MAriam Data Collection
    =gfOpenTable('SYSCHDUL','Coprusr')
    =gfOpenTable('SCALE','SCALE')
    lcFltExp = ''

    * IF lcRpPrjTp <> 'S' && Project Id Option will not be displayed in case of Type Style
    lnPrjPos = ASCAN(.laFxFltCpy,'PMPRJHD.CPRJ_ID')
    lcSelPrj =''
    IF lnPrjPos  <> 0
      lnPrjPos = ASUBSCRIPT(.laFxFltCpy,lnPrjPos ,1)
      lcSelPrj  =.laFxFltCpy[lnPrjPos ,6]
      IF !EMPTY(lcSelPrj) AND USED(lcSelPrj)
        SELECT (lcSelPrj)
        LOCATE
        IF !EOF()
          *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [Start]
          *lcFltExp = "Seek(PMPRJHD.CPRJ_ID+'-'+PMPRJHD.CSTYLE,'&lcSelPrj')"
          lcFltExp = "Seek(PMPRJHD.CPRJ_ID+'+'+PMPRJHD.CSTYLE+'+'+STR(PMPRJHD.LINENO,6),'&lcSelPrj')"
          *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [End]
        ENDIF
      ENDIF
    ENDIF
    *ENDIF


    *Filters On project Header Table{Start}
    *Style
    lnStyPos = ASCAN(.laFxFltCpy,'PMPRJHD.CSTYLE')
    lcSelSty =''
    *B609286,1 MMT 06/07/2010 SO - project monitor screen not working for style filter [Start]
    llStyleSelect = .F.
    *B609286,1 MMT 06/07/2010 SO - project monitor screen not working for style filter [End]
    IF lnStyPos <> 0
      lnStyPos = ASUBSCRIPT(.laFxFltCpy,lnStyPos ,1)
      lcSelSty =.laFxFltCpy[lnStyPos ,6]
      IF !EMPTY(lcSelSty ) AND USED(lcSelSty )
        SELECT (lcSelSty )
        LOCATE
        IF !EOF()
          *B609286,1 MMT 06/07/2010 SO - project monitor screen not working for style filter [Start]
          *lcFltExp = lcFltExp + IIF(EMPTY(lcFltExp),'',' AND ')+ "Seek(PMPRJHD.CSTYLE,'&lcSelSty')"
          llStyleSelect = .T.          
          *B609286,1 MMT 06/07/2010 SO - project monitor screen not working for style filter [End]          
        ENDIF
      ENDIF
    ENDIF

    *Show Late Projects
    IF llRpShLate
      lcFltExp = lcFltExp + IIF(EMPTY(lcFltExp),'',' AND ')+ " PmPrjHd.dAct_Fnsh > PmPrjHd.dEst_Fnsh"
    ENDIF
    
    *Project Status
    *  B609778,1 MMT 12/25/2011 fix bug of not Displaying the Cancelled projects[Start]
*!*      IF lcRpPrjStt = "A"
*!*        lcFltExp = lcFltExp + IIF(EMPTY(lcFltExp),'',' AND ')+" PmPrjHd.cPrj_Stts <> 'X' "
*!*      ELSE
*!*        lcFltExp = lcFltExp + IIF(EMPTY(lcFltExp),'',' AND ')+" PmPrjHd.cPrj_Stts = '"+lcRpPrjStt+"' "
*!*      ENDIF
    IF EMPTY(lcRpPrjStt)
      lcFltExp = lcFltExp + IIF(EMPTY(lcFltExp),'',' AND ')+" PmPrjHd.cPrj_Stts $ 'PICX' "
    ELSE
      lcFltExp = lcFltExp + IIF(EMPTY(lcFltExp),'',' AND ')+" PmPrjHd.cPrj_Stts $ '"+lcRpPrjStt+"' "
    ENDIF
    *  B609778,1 MMT 12/25/2011 fix bug of not Displaying the Cancelled projects[END]

    *Required Project Date
    lnPrjDPos = ASCAN(.laFxFltCpy,'PMPRJHD.DREQ_FNSH')
    lcSelDate =''
    IF lnPrjDPos <> 0
      lnPrjDPos = ASUBSCRIPT(.laFxFltCpy,lnPrjDPos,1)
      lcSelDate =.laFxFltCpy[lnPrjDPos,6]
      IF !EMPTY(lcSelDate)
        lcStartDate = SUBSTR(lcSelDate,1,10)
        lcEndDate   = SUBSTR(lcSelDate,12,21)
        IF ATC("|",lcSelDate) = 1
          lcFltExp = lcFltExp + IIF(EMPTY(lcFltExp),'',' AND ')+ "PMPRJHD.DREQ_FNSH =< CTOD('"+SUBSTR(lcSelDate,2)+"')"
        ELSE
          lcFltExp = lcFltExp + IIF(EMPTY(lcFltExp),'',' AND ')+ "BETWEEN(PMPRJHD.DREQ_FNSH,CTOD('&lcStartDate'),CTOD('&lcEndDate'))"
        ENDIF
      ENDIF
    ENDIF
    *Filters On project Header Table{End}

    lnmajorlen =LEN(gfItemMask("PM"))

    *  E302650,3 MMT 12/29/2009 make project monitor work based on report templated selected [Start]
    lcStyleMask = gfItemMask("M")
    *  E302650,3 MMT 12/29/2009 make project monitor work based on report templated selected [End]


    *Sales Order Filters[Start]
    IF lcRpPrjTp $ 'TO'

      *Customer
      lnAccPos = ASCAN(.laFxFltCpy,'ORDHDR.ACCOUNT')
      lcSelAcc =''
      llAccSelected = .F.
      IF lnAccPos <> 0
        lnAccPos = ASUBSCRIPT(.laFxFltCpy,lnAccPos ,1)
        lcSelAcc =.laFxFltCpy[lnAccPos ,6]
        IF !EMPTY (lcSelAcc) AND USED(lcSelAcc)
          SELECT (lcSelAcc)
          LOCATE
          IF !EOF()
            llAccSelected = .T.
          ENDIF
        ENDIF
      ENDIF

      *Customer Delivery Date
      lnCmpDPos = ASCAN(.laFxFltCpy,'ORDHDR.COMPLETE')
      lcCmpDate =''
      IF lnCmpDPos <> 0
        lnCmpDPos = ASUBSCRIPT(.laFxFltCpy,lnCmpDPos ,1)
        lcCmpDate =.laFxFltCpy[lnCmpDPos ,6]
      ENDIF

    ENDIF
    *Sales Order Filters[End]


    IF lcRpPrjTp $ 'PANDR'
      *VENDOR
      lnVenPos = ASCAN(.laFxFltCpy,'POSHDR.VENDOR')
      lcSelVend =''
      llVendSelected = .F.
      IF lnVenPos<> 0
        lnVenPos= ASUBSCRIPT(.laFxFltCpy,lnVenPos,1)
        lcSelVend=.laFxFltCpy[lnVenPos,6]
        IF !EMPTY (lcSelVend) AND USED(lcSelVend)
          SELECT (lcSelVend)
          LOCATE
          IF !EOF()
            llVendSelected = .T.
          ENDIF
        ENDIF
      ENDIF
    ENDIF

    *Notify User
    lnNtfPos = ASCAN(.laFxFltCpy,'PMPRJNTF.CUSER_ID')
    lcNTFSel =''
    llNtfySelected = .F.
    IF lnNtfPos <> 0
      lnNtfPos = ASUBSCRIPT(.laFxFltCpy,lnNtfPos ,1)
      lcNTFSel =.laFxFltCpy[lnNtfPos ,6]
      IF !EMPTY (lcNTFSel ) AND USED(lcNTFSel)
        SELECT (lcNTFSel )
        LOCATE
        IF !EOF()
          llNtfySelected = .T.
        ENDIF
      ENDIF
    ENDIF


    *Task
    lnTskPos = ASCAN(.laFxFltCpy,'PMPRJDT.COPRT_ID')
    lcTskSel =''
    llTskSelected = .F.
    IF lnTskPos <> 0
      lnTskPos = ASUBSCRIPT(.laFxFltCpy,lnTskPos ,1)
      lcTskSel =.laFxFltCpy[lnTskPos ,6]
      IF !EMPTY (lcTskSel) AND USED(lcTskSel)
        SELECT (lcTskSel)
        LOCATE
        IF !EOF()
          llTskSelected = .T.
        ENDIF
      ENDIF
    ENDIF

    IF !llTskSelected AND !EMPTY(lcRpTemplt)

      *! B609028,1 MMT 10/07/2009 Fix bug of not exporting Task notes to excel[Start]
      *IF gfSeek(lcRpTemplt,'PmRprTm')
      IF gfSeek(lcRpTemplt,'PMPTHDT')
        *! B609028,1 MMT 10/07/2009 Fix bug of not exporting Task notes to excel[End]
        lcFilTasks = gfTempName()
        CREATE CURSOR (lcFilTasks) (KeyExp C(9))
        SELECT (lcFilTasks)
        INDEX ON KeyExp TAG (lcFilTasks)
        *! B609028,1 MMT 10/07/2009 Fix bug of not exporting Task notes to excel[Start]
        *!*        lcTmp_Memo = PmRprTm.mTmp_Oprt
        *!*        lnStart=1
        *!*        lnEnd=AT('|',lcTmp_Memo)
        *!*        DO WHILE lnEnd <> 0
        *!*          SELECT(lcFilTasks)
        *!*          APPEND BLANK
        *!*          lcCurrVal = SUBSTR(lcTmp_Memo,lnStart,lnEnd-1)
        *!*          REPLACE KeyExp   WITH SUBSTR(lcCurrVal ,1,3)+"-"+SUBSTR(lcCurrVal ,4,5)
        *!*          lcTmp_Memo = STUFF(lcTmp_Memo ,lnStart,lnEnd,"")
        *!*          lnEnd=AT('|',lcTmp_Memo)
        *!*        ENDDO
        *!*        IF lnEnd = 0
        *!*          SELECT(lcFilTasks)
        *!*          APPEND BLANK
        *!*          REPLACE KeyExp  WITH SUBSTR(lcTmp_Memo,1,3)+"-"+SUBSTR(lcTmp_Memo,4,5)
        *!*        ENDIF
        SELECT PMPTHDT
        SCAN REST WHILE CPATH_ID+COPRT_CTG+COPRT_ID = PADR(lcRpTemplt,4)
          SELECT(lcFilTasks)
          APPEND BLANK
          REPLACE KeyExp   WITH PMPTHDT.COPRT_CTG +"-"+PMPTHDT.COPRT_ID
        ENDSCAN
        *! B609028,1 MMT 10/07/2009 Fix bug of not exporting Task notes to excel[End]
        SELECT (lcFilTasks)
        LOCATE
        IF !EOF()
          .laFxFltCpy[lnTskPos ,6]=  lcFilTasks
          llTskSelected = .T.
          lcTskSel =lcFilTasks
        ENDIF
      ENDIF
    ENDIF


    IF lcRpPrjTp $ 'PO'
      *EX Factory Date
      lnPCmpDPos = ASCAN(.laFxFltCpy,'POSHDR.COMPLETE')
      lcPCmpDate =''
      IF lnPCmpDPos <> 0
        lnPCmpDPos = ASUBSCRIPT(.laFxFltCpy,lnPCmpDPos ,1)
        lcPCmpDate =.laFxFltCpy[lnPCmpDPos ,6]
      ENDIF
    ENDIF



    IF lcRpPrjTp = 'O'
      SELECT CUTPICK
      =gfSetOrder('CUTORD')
    ELSE
      SELECT CUTPICK
      =gfSetOrder('CUTPICK')
    ENDIF

    SELECT PMPRJDT
    =gfSetOrder('PMPRJDT')




    FOR lnI =1 TO lnTaskCnt
      lcI = ALLTRIM(STR(lnI))
      m.cOprID&lcI.= ''
      m.cOpDsc&lcI.= ''
      m.COPCTG&lcI.= ''
      m.cTskSt&lcI.= ''
      m.dAcFnh&lcI.= {}
      m.DClFnh&lcI.= {}
      m.mOPCOM&lcI. = ""
      m.nTaskStat = 0
    ENDFOR



    lnTaskNum = 1

  	*  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
  	opross = CREATEOBJECT('ariaprogressbar')  
  	oPross.lblFirstLabel.Caption = LANG_MFPROJMON_COLLECTDATA 
    *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]

    SELECT PmPrjHd
    =gfSeek(lcRpPrjTp)

    *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [Start]
    * SCAN REST WHILE PmPrjHd.cprj_typ+PmPrjHd.cprj_id+PmPrjHd.cstyle= lcRpPrjTp FOR &lcFltExp
    
    IF !EMPTY(LCRPRPTTM) AND LCRPRPTTM <> 'N/A'
    *  E302650,2 MMT 12/24/2009 make project monitor work based on report templated selected [Start]
    IF !EMPTY(loFormSet.LATABLEUSE[1])
    *  E302650,2 MMT 12/24/2009 make project monitor work based on report templated selected [End]
      FOR lnA = 1 TO ALEN(loFormSet.LATABLEUSE,1)
        IF !USED(loFormSet.LATABLEUSE[lnA])
          DO CASE
            CASE ALLTRIM(loFormSet.LATABLEUSE[lnA]) = 'CUTPICK'
              =gfOpenTable(loFormSet.LATABLEUSE[lnA],'CUTPKORD')
            CASE ALLTRIM(loFormSet.LATABLEUSE[lnA]) = "ITEM"
              =gfOpenTable(loFormSet.LATABLEUSE[lnA],'STYLE')
            CASE ALLTRIM(loFormSet.LATABLEUSE[lnA]) = "APVENDOR"
              =gfOpenTable(loFormSet.LATABLEUSE[lnA],'VENCODE')
            OTHERWISE
              =gfOpenTable(loFormSet.LATABLEUSE[lnA],loFormSet.LATABLEUSE[lnA])
          ENDCASE
        ENDIF
      ENDFOR
      *  E302650,2 MMT 12/24/2009 make project monitor work based on report templated selected [Start]
      ENDIF
      *  E302650,2 MMT 12/24/2009 make project monitor work based on report templated selected [End]
      
      *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
      COUNT REST WHILE PmPrjHd.cprj_typ+PmPrjHd.cprj_id+PmPrjHd.cstyle+STR(PmPrjHd.LINENO,6) = lcRpPrjTp FOR &lcFltExp TO lnTotal
    	oPross.TotalProgress = lnTotal
    	oPross.AutoCenter = .T.
      IF lnTotal > 0
      	oPross.CurrentProgress(0)
      ENDIF  
    	oPross.Show()
      lnPrepRec = 0		
      =gfSeek(lcRpPrjTp)
      *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
      
      SCAN REST WHILE PmPrjHd.cprj_typ+PmPrjHd.cprj_id+PmPrjHd.cstyle+STR(PmPrjHd.LINENO,6) = lcRpPrjTp FOR &lcFltExp
      
        *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
        lnPrepRec = lnPrepRec + 1
        oPross.CurrentProgress(lnPrepRec)
        oPross.lblSecondLabel.Caption =  LANG_MFPROJMON_PROJECTNUMBER +PmPrjHd.cprj_id
        *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
      
      
        FOR lnU = 1 TO ALEN(loFormSet.LATEMPFL,1)
          IF !EMPTY(loFormSet.LATEMPFL[lnU,1]) AND USED(loFormSet.LATEMPFL[lnU,1])
            SELECT (loFormSet.LATEMPFL[lnU,1])
            SCATTER MEMO MEMVAR BLANK
          ENDIF
        ENDFOR

        m.cprj_typ = PmPrjHd.cprj_typ
        m.cprj_id = PmPrjHd.cprj_id
        m.DREQ_FNSH = PmPrjHd.DREQ_FNSH
        m.ACCOUNT = ''
        m.VENDOR = ''
        m.cstyle = PmPrjHd.cstyle
        m.LINENO = PmPrjHd.LINENO
        IF ASCAN(loFormSet.LATABLEFLD,'PMPRJHD',1,0,1)> 0
          lnPOs = ASUBSCRIPT(loFormSet.LATABLEFLD,ASCAN(loFormSet.LATABLEFLD,'PMPRJHD',1,0,1),1)
          lcFldScat = loFormSet.LATABLEFLD[lnPOs ,2]
          SELECT PmPrjHd
          *  E302650,2 MMT 12/24/2009 make project monitor work based on report templated selected [Start]
          *SCATTER FIELDS &lcFldScat. MEMO MEMVAR
          SCATTER FIELDS LIKE   &lcFldScat. MEMO MEMVAR
          *  E302650,2 MMT 12/24/2009 make project monitor work based on report templated selected [End]
          IF ASCAN(loFormSet.LAFLDMAP,'PMPRJHD',1,0,2) > 0
            FOR lnE = 1 TO ALEN(loFormSet.LAFLDMAP,1)
              IF 'PMPRJHD' $ loFormSet.LAFLDMAP[lnE,2]
                lcMemFld = 'm.'+loFormSet.LAFLDMAP[lnE,1]
                &lcMemFld  = EVALUATE(loFormSet.LAFLDMAP[lnE,2])
              ELSE
                LOOP 
              ENDIF   
            ENDFOR  
          ENDIF 
        ENDIF
        IF !(lcRpPrjTp $ 'SHM')
          IF !lfFiltrTnTmp()
            LOOP
          ENDIF
        ELSE
          llTaskFnd = .F.
          FOR lnI =1 TO ALEN(loFormSet.LATEMPACT,1)
            lcI = ALLTRIM(STR(lnI))
            m.cOprID&lcI.= ''
            m.cOpDsc&lcI.= ''
            m.COPCTG&lcI.= ''
            m.cTskSt&lcI.= ''
            m.dAcFnh&lcI.= {}
            m.DClFnh&lcI.= {}
            m.mOPCOM&lcI. = ""
            m.nTaskStat = 0
          ENDFOR
          IF lcRpPrjTp = 'S' AND gfSeek(ALLTRIM(PmPrjHd.cstyle),'STYLE')
            IF ASCAN(loFormSet.LATABLEFLD,'STYLE',1,0,1)> 0
              lnPOs = ASUBSCRIPT(loFormSet.LATABLEFLD,ASCAN(loFormSet.LATABLEFLD,'STYLE',1,0,1),1)
              lcFldScat = loFormSet.LATABLEFLD[lnPOs ,2]
              SELECT STYLE
              *  E302650,2 MMT 12/24/2009 make project monitor work based on report templated selected [Start]
              *SCATTER FIELDS &lcFldScat. MEMO MEMVAR
              SCATTER FIELDS LIKE  &lcFldScat. MEMO MEMVAR
              *  E302650,2 MMT 12/24/2009 make project monitor work based on report templated selected [End]
              IF ASCAN(loFormSet.LAFLDMAP,'STYLE',1,0,2) > 0
                FOR lnE = 1 TO ALEN(loFormSet.LAFLDMAP,1)
                  IF 'STYLE' $ loFormSet.LAFLDMAP[lnE,2]
                    lcMemFld = 'm.'+loFormSet.LAFLDMAP[lnE,1]
                    &lcMemFld  = EVALUATE(loFormSet.LAFLDMAP[lnE,2])
                  ELSE
                    LOOP 
                  ENDIF   
                ENDFOR  
              ENDIF 
              *  E302650,2 MMT 12/24/2009 make project monitor work based on report templated selected [Start]
              IF !EMPTY(loFormSet.LATABLEPROF[1,1])
                FOR lnT = 1 TO ALEN(loFormSet.LATABLEPROF,1)
                  IF loFormSet.LATABLEPROF[lnT,2] = 'ST' && GET PROFILE VALUE FOR SALES Order Header
                    IF gfSeek('ST'+PADR(ALLTRIM(PmPrjHd.cstyle),130)+loFormSet.LATABLEPROF[lnT,1],"PROFVALU")
                      lcPrFFld = 'P'+loFormSet.LATABLEPROF[lnT,1]
                      m.&lcPrFFld. = PROFVALU.cpro_value
                    *  E302650,3 MMT 12/29/2009 make project monitor work based on report templated selected [Start]
                    ELSE
                      lcStyleClr = STRTRAN(lcStyleMask ,'X','*')
                      lcStyleClr = STRTRAN(lcStyleClr ,SUBSTR(lcStyleClr ,1 ,lnmajorlen ),SUBSTR(PmPrjHd.cstyle,1,lnmajorlen ))
                      IF gfSeek('ST'+PADR(lcStyleClr ,130)+loFormSet.LATABLEPROF[lnT,1],"PROFVALU")
                        lcPrFFld = 'P'+loFormSet.LATABLEPROF[lnT,1]
                        m.&lcPrFFld. = PROFVALU.cpro_value
                      ENDIF 
                    *  E302650,3 MMT 12/29/2009 make project monitor work based on report templated selected [End]  
                    ENDIF
                  ENDIF 
                ENDFOR 
              ENDIF 
              *  E302650,2 MMT 12/24/2009 make project monitor work based on report templated selected [End]
            ENDIF
            =gfSeek('S'+STYLE.SCALE,'SCALE')
            m.ScaleDsc  = SCALE.cscl_desc
            m.Desc1     = STYLE.Desc1
          ELSE
            m.ScaleDsc  = ''
            m.Desc1     = ''
          ENDIF

          lfTaskList()
          *!*          IF llTaskFnd
          *!*            INSERT INTO (.lcTempFile) FROM MEMVAR
          *!*            *! B608925,1 MMT 07/27/2009 Increase Number of tasks to be 85 instead of 25 [Start]
          *!*            INSERT INTO (.lcTempFile2) FROM MEMVAR
          *!*            INSERT INTO (.lcTempFile3) FROM MEMVAR
          *!*            *! B608925,1 MMT 07/27/2009 Increase Number of tasks to be 85 instead of 25 [End]
          *!*          ENDIF
          
          *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
          IF !EMPTY(loFormSet.LATEMPACT[1])
            llAllEmpty = .T.
            FOR lnD =1 TO ALEN(loFormSet.LATEMPACT,1)
              lcD = ALLTRIM(STR(lnD))
              IF !EMPTY(m.cTskSt&lcD.)
                llAllEmpty = .F.
                EXIT 
              ENDIF
            ENDFOR 
            IF llAllEmpty
              LOOP 
            ENDIF
          ENDIF  
          *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
          
          FOR lnR = 1 TO ALEN(loFormSet.LATEMPFL,1)
            IF !EMPTY(loFormSet.LATEMPFL[lnR,1]) AND USED(loFormSet.LATEMPFL[lnR,1])
              INSERT INTO (loFormSet.LATEMPFL[lnR,1]) FROM MEMVAR
            ENDIF
          ENDFOR
        ENDIF
      ENDSCAN
      *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [End]
    ELSE
    
      *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
      COUNT REST WHILE PmPrjHd.cprj_typ+PmPrjHd.cprj_id+PmPrjHd.cstyle+STR(PmPrjHd.LINENO,6) = lcRpPrjTp FOR &lcFltExp TO lnTotal
      oPross.TotalProgress = lnTotal
      oPross.AutoCenter = .T.
      IF lnTotal > 0
        oPross.CurrentProgress(0)
      ENDIF
      oPross.Show()
      lnPrepRec = 0    
      =gfSeek(lcRpPrjTp)
      *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
   
      SCAN REST WHILE PmPrjHd.cprj_typ+PmPrjHd.cprj_id+PmPrjHd.cstyle+STR(PmPrjHd.LINENO,6) = lcRpPrjTp FOR &lcFltExp
      
        *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
        lnPrepRec = lnPrepRec + 1
        oPross.CurrentProgress(lnPrepRec)
        oPross.lblSecondLabel.Caption =  LANG_MFPROJMON_PROJECTNUMBER +PmPrjHd.cprj_id
        *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]

        SELECT (.lcTempFile)
        SCATTER MEMO MEMVAR BLANK
        SELECT (.lcTempFile2)
        SCATTER MEMO MEMVAR BLANK
        SELECT (.lcTempFile3)
        SCATTER MEMO MEMVAR BLANK
        
        SELECT PmPrjHd
        SCATTER MEMO MEMVAR
        m.cProjStat = IIF(m.dAct_Fnsh > m.dEst_Fnsh, 'LATE', 'ON TIME')
        m.cProjType = lfProjType(m.cprj_typ)

        *! B609028,1 MMT 10/07/2009 Fix bug of not exporting Task notes to excel[Start]
        IF !EMPTY(laPoCustFld[1,1])
          FOR lnT = 1 TO ALEN(laPoCustFld,1)
            lcFldName = laPoCustFld[lnT,1]
            DO CASE
              CASE laPoCustFld[lnT,2] = 'C' OR  laPoCustFld[lnT,2] = 'M'
                m.&lcFldName. = ''
              CASE laPoCustFld[lnT,2] = 'N'
                m.&lcFldName. = 0
              CASE laPoCustFld[lnT,2] = 'L'
                m.&lcFldName. = .F.
            ENDCASE
          ENDFOR
        ENDIF
        *! B609028,1 MMT 10/07/2009 Fix bug of not exporting Task notes to excel[End]



        IF !(lcRpPrjTp $ 'SHM')
          IF !lfFiltrTrrn()
            LOOP
          ENDIF
        ENDIF



        IF (lcRpPrjTp $ 'SHM')
          llTaskFnd = .F.
          FOR lnI =1 TO lnTaskCnt
            lcI = ALLTRIM(STR(lnI))
            m.cOprID&lcI.= ''
            m.cOpDsc&lcI.= ''
            m.COPCTG&lcI.= ''
            m.cTskSt&lcI.= ''
            m.dAcFnh&lcI.= {}
            m.DClFnh&lcI.= {}
            m.mOPCOM&lcI. = ""
            m.nTaskStat = 0
          ENDFOR

          IF lcRpPrjTp = 'S' AND gfSeek(ALLTRIM(PmPrjHd.cstyle),'STYLE')
            =gfSeek('S'+STYLE.SCALE,'SCALE')
            m.ScaleDsc  = SCALE.cscl_desc
            m.Desc1     = STYLE.Desc1
          ELSE
            m.ScaleDsc  = ''
            m.Desc1     = ''
          ENDIF

          lfGetTasks()
          IF llTaskFnd
            INSERT INTO (.lcTempFile) FROM MEMVAR
            *! B608925,1 MMT 07/27/2009 Increase Number of tasks to be 85 instead of 25 [Start]
            INSERT INTO (.lcTempFile2) FROM MEMVAR
            INSERT INTO (.lcTempFile3) FROM MEMVAR
            *! B608925,1 MMT 07/27/2009 Increase Number of tasks to be 85 instead of 25 [End]
          ENDIF
        ENDIF
      ENDSCAN
    *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [Start]      
    ENDIF
    *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [End]

    DO CASE
      CASE .lcRpSrtBy $ 'OC'
        lcSortBy = 'CPRJ_TYP+cPrj_Id'
      CASE .lcRpSrtBy = 'R'
        lcSortBy = 'CPRJ_TYP+DTOS(dReq_Fnsh)+cPrj_Id'
      CASE .lcRpSrtBy $ 'A'
        lcSortBy = 'CPRJ_TYP+ACCOUNT+cPrj_Id'
      CASE .lcRpSrtBy $ 'V'
        IF EMPTY(LCRPRPTTM) OR  LCRPRPTTM = 'N/A'
          lcSortBy = 'CPRJ_TYP+PARTNER+cPrj_Id'
        ELSE
          lcSortBy = 'CPRJ_TYP+VENDOR+cPrj_Id'
        ENDIF
      CASE .lcRpSrtBy $ 'SM'
        IF lcRpPrjTp = 'S'
          lcSortBy = 'CPRJ_TYP+cStyle'
        ELSE
          lcSortBy = 'CPRJ_TYP+cPrj_Id'
        ENDIF
    ENDCASE
    
 
    *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [Start]
    IF EMPTY(LCRPRPTTM) OR  LCRPRPTTM = 'N/A'
    *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [End]
      *  B609386,1 MMT 08/25/2010 Error while export to Excel when same color selected twice[Start]
      IF USED(.lcTempFile)
      *  B609386,1 MMT 08/25/2010 Error while export to Excel when same color selected twice[End]
      SELECT (.lcTempFile)
      *  B609386,1 MMT 08/25/2010 Error while export to Excel when same color selected twice[Start]
      ELSE
        RETURN 
      ENDIF
      *  B609386,1 MMT 08/25/2010 Error while export to Excel when same color selected twice[End]
    *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [Start]
    ENDIF
    *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [End]

    ldSD = oAriaApplication.SystemDate

    *! B608925,1 MMT 07/27/2009 Increase Number of tasks to be 85 instead of 25 [Start]
    *SELECT (.lcTempFile)
    *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [Start]
    IF EMPTY(LCRPRPTTM) OR  LCRPRPTTM = 'N/A'
    *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [End]

      SELECT (.lcTempFile3)
      *! B608925,1 MMT 07/27/2009 Increase Number of tasks to be 85 instead of 25 [End]

      LOCATE
      FOR lnI =1 TO lnTaskCnt
        lcI = ALLTRIM(STR(lnI))
        LOCATE FOR !EMPTY(cOpDsc&lcI.)
        IF FOUND()
          lcDescVal = cOpDsc&lcI.
          REPLACE ALL cOpDsc&lcI. WITH lcDescVal
          LOCATE
        ELSE
          LOCATE
          LOOP
        ENDIF
      ENDFOR
      LOCATE
      *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [Start]
    ENDIF
      *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [End]

    B = 0
    A = 0

    C = 0
    D = 0

    nDyPstCm2 = 0
    E = 0

    IF FILE(oAriaApplication.DefaultPath+'PrjClr.XML')
      lcXmlData = FILETOSTR(oAriaApplication.DefaultPath+'PrjClr.XML')
      LOCAL lobjDOMDocument
      lobjDOMDocument = CREATEOBJECT("MSXML2.DOMDocument")
      lobjDOMDocument.LOADXML(lcXmlData)
      LOCAL loRoot
      loRoot = lobjDOMDocument.childNodes(1).childNodes(0)
      LOCAL lnIndex
      LOCAL loVariable, lcName, lcDataType, lcValue

      FOR lnIndex = 0 TO loRoot.childNodes.LENGTH - 1
        loVariable = loRoot.childNodes(lnIndex)
        lcName     = loVariable.childNodes(0).TEXT
        IF ALLTRIM(UPPER(loRoot.childNodes(lnIndex).nodeName)) == UPPER('BeforeComplete')
          B  = VAL(loRoot.childNodes(lnIndex).TEXT)
        ENDIF

        IF ALLTRIM(UPPER(loRoot.childNodes(lnIndex).nodeName)) == UPPER('BeforeColor')
          A  = VAL(loRoot.childNodes(lnIndex).TEXT)
        ENDIF


        IF ALLTRIM(UPPER(loRoot.childNodes(lnIndex).nodeName)) == UPPER('AftComplete1')
          C   = VAL(loRoot.childNodes(lnIndex).TEXT)
        ENDIF

        IF ALLTRIM(UPPER(loRoot.childNodes(lnIndex).nodeName)) == UPPER('AfterColor1')
          D= VAL(loRoot.childNodes(lnIndex).TEXT)
        ENDIF


        IF ALLTRIM(UPPER(loRoot.childNodes(lnIndex).nodeName)) == UPPER('AfterColor2')
          E = VAL(loRoot.childNodes(lnIndex).TEXT)
        ENDIF
      ENDFOR
    ENDIF



    lcPrjID = 'Project ID'
    DO CASE
      CASE lcRpPrjTp  = 'C'
        lcPrjID= LANG_MFPROJMON_CTNO
      CASE lcRpPrjTp  = 'P'      && PO
        lcPrjID= LANG_MFPROJMON_PONUMBER
      CASE lcRpPrjTp  = 'A'      && Adorment Order
        lcPrjID= LANG_MFPROJMON_ORDNUM
      CASE lcRpPrjTp  = 'D'      && Dye Order
        lcPrjID= LANG_MFPROJMON_ORDNUM
      CASE lcRpPrjTp  = 'N'      && Inter-Location PO
        lcPrjID= LANG_MFPROJMON_ORDNUM
      CASE lcRpPrjTp  = 'R'      && Return PO
        lcPrjID= LANG_MFPROJMON_ORDNUM
      CASE lcRpPrjTp  = 'O'      && SO
        lcPrjID= LANG_MFPROJMON_ORDNUM
      CASE lcRpPrjTp  = 'T'      && EDI Order
        lcPrjID= LANG_MFPROJMON_ORDNUM
      CASE lcRpPrjTp  $ 'H'      && EDI Order
        lcPrjID= LANG_MFPROJMON_PROJID
    ENDCASE

    *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [Start]
    IF EMPTY(LCRPRPTTM) OR  LCRPRPTTM = 'N/A'
    *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [End]
      IF !(lcRpPrjTp $ 'MS')
        lcBrFields =[CPRJ_ID:H =']+lcPrjID+[']+[,w = lfTranBut():5:H = '']
      ENDIF




      IF lcRpPrjTp $ 'PCOT'
        IF lcRpPrjTp = 'O'
          lcBrFields = lcBrFields + [,Store : H =']+LANG_MFPROJMON_STORE+[']+[,PO : H =']+LANG_MFPROJMON_PONUMBER+[']+[,Partner: H =']+LANG_MFPROJMON_VENDOR+[']
        ELSE
          IF lcRpPrjTp $ 'CP'
            lcBrFields = lcBrFields + [,Order : H =']+LANG_MFPROJMON_ORDNUM+[']+[,Store : H =']+LANG_MFPROJMON_STORE+[']
            IF lcRpPrjTp = 'P'
              lcBrFields = lcBrFields + [,Partner: H =']+LANG_MFPROJMON_VENDOR+[']
            ENDIF
          ENDIF
        ENDIF


        lcBrFields = lcBrFields +  [,Account : H = ']+LANG_MFPROJMON_CUSTOMER+[',CUSTPO:H = ']+;
          LANG_MFPROJMON_CUSTPO+[',Style :H = ']+LANG_MFPROJMON_STYLE +[',]+;
          [Complete1:H = ']+ LANG_MFPROJMON_COMPDATE+[',Price:H = ']+;
          LANG_MFPROJMON_PRICE+[',totQty: H =']+LANG_MFPROJMON_TOTQTY+[']

        *lcBrFields = lcBrFields + [,ProFile1:H = ']+LANG_MFPROJMON_PRFLE1+[',ProFile2:H = ']+;
        LANG_MFPROJMON_PRFLE2+[',ProFile3:H = ']+LANG_MFPROJMON_PRFLE3+[']
      ELSE
        IF !(lcRpPrjTp $ 'MS')

          lcBrFields = lcBrFields +  [,cStyle :H = ']+LANG_MFPROJMON_STYLE +[',CPRJ_SDSC:H= ']+;
            LANG_MFPROJMON_PRJDESC+[' ,CPRJ_STTS:H= ']+LANG_MFPROJMON_STATUS+[']

        ELSE
          lcBrFields = [cStyle :H = ']+IIF(lcRpPrjTp = 'S',LANG_MFPROJMON_STYLE ,LANG_MFPROJMON_MATERIAL)+;
            [',w = lfTranBut():5:H = '',CPRJ_SDSC:H= ']+LANG_MFPROJMON_PRJDESC+;
            [' ,CPRJ_STTS:H= ']+LANG_MFPROJMON_STATUS+[']

        ENDIF
      ENDIF

      *! B609028,1 MMT 10/07/2009 Fix bug of not exporting Task notes to excel[Start]
      *lcBrFields = lcBrFields + IIF(llRpShWDy and lcRpPrjTp $ 'PCO',[,nTaskStat:10:H=']+LANG_MFPROJMON_DAYSTTL+['],'')
      IF llRpShWDy AND lcRpPrjTp $ 'PCO'
        lcTskTitl = LANG_MFPROJMON_DAYSTTL + ' '+lcNomDesc
      ENDIF
      lcBrFields = lcBrFields + IIF(llRpShWDy AND lcRpPrjTp $ 'PCO',[,nTaskStat:10:H=']+lcTskTitl +['],'')
      *! B609028,1 MMT 10/07/2009 Fix bug of not exporting Task notes to excel[End]

      *  E302650,2 MMT 12/24/2009 make project monitor work based on report templated selected [Start]
      T2 = .lcTempFile2
      T3 = .lcTempFile3
      *  E302650,2 MMT 12/24/2009 make project monitor work based on report templated selected [End]
      FOR lnI =1 TO lnTaskCnt
        lcI = ALLTRIM(STR(lnI))
        IF EMPTY(cOpDsc&lcI.)
          LOOP
        ENDIF
        *! B609028,1 MMT 10/07/2009 Fix bug of not exporting Task notes to excel[Start]
        *    lcBrFields = lcBrFields + [,z]+lcI+[=lfAddTask(']+lcI+['):37:H =&T3..COPDSC]+lcI
        lcBrFields = lcBrFields + [,z]+lcI+[=lfAddTask(']+lcI+['):37:H =&T3..COPDSC]+lcI +[,mOpCom]+lcI+[:1:H =ALLTRIM(&T3..COPDSC]+lcI+[)+' Notes']
        *! B609028,1 MMT 10/07/2009 Fix bug of not exporting Task notes to excel[End]
      ENDFOR


      FOR lnI =lnTaskCnt + 1 TO 85
        lcI = ALLTRIM(STR(lnI))
        *! B609028,1 MMT 10/07/2009 Fix bug of not exporting Task notes to excel[Start]
        *lcBrFields = lcBrFields + [,&T2..cTskSt]+lcI+[:37:H ='~']
        lcBrFields = lcBrFields + [,&T2..cTskSt]+lcI+[:37:H ='~']+[,mOpCom]+lcI+[:1:H = '~']
        *! B609028,1 MMT 10/07/2009 Fix bug of not exporting Task notes to excel[End]
      ENDFOR


      *! B608925,1 MMT 07/27/2009 Increase Number of tasks to be 85 instead of 25 [Start]
      T2 = .lcTempFile2
      T3 = .lcTempFile3
      lcTempFile = .lcTempFile
      SELECT (.lcTempFile)
      LOCATE
      *! B608925,1 MMT 07/27/2009 Increase Number of tasks to be 85 instead of 25 [End]
    *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [Start]
    ENDIF
    *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [End]
          
    *! B609028,1 MMT 10/07/2009 Fix bug of not exporting Task notes to excel[Start]
    lcTaskIdCnt = ''
    lnBackClor =  0
    *! B609028,1 MMT 10/07/2009 Fix bug of not exporting Task notes to excel[End]
    
   *  B609143,1 MMT 02/15/2010 Error 'Too Many VAriables' in project monitor screen[Start]
    FOR lnI =1 TO 85
      lcI = ALLTRIM(STR(lnI))
      RELEASE m.cOprID&lcI.
      RELEASE m.cOpDsc&lcI.
      RELEASE m.COPCTG&lcI.
      RELEASE m.cTskSt&lcI.
      RELEASE m.dAcFnh&lcI.
      RELEASE m.DClFnh&lcI.
      RELEASE m.mOPCOM&lcI. 
    ENDFOR
    
    *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
    oPross = null
    *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
    *  B609631,1 MMT 06/26/2011 Project Monitor should display 1 line for style color in Extended Size Scale companies[Start]
    llExtendedScale = .F.
    lcColorSep =''
    STORE 0 TO lnScaPosGl ,lnScaLnGl   
    IF !(lcRpPrjTp $ 'MH')
      llExtendedScale =  gfGetMemVar('M_USEEXSSC')
      IF llExtendedScale 
        DECLARE laItemSeg[1]
        =gfItemMask(@laItemSeg)
        FOR lnCount = 1 TO ALEN(laItemSeg,1)
          IF laItemSeg[lnCount,1]='C'
            lcColorSep =laItemSeg[lnCount,6]
          ENDIF
          IF laItemSeg[lnCount,1]='S'
            lnScaLnGl  = LEN(laItemSeg[lnCount,3])
            lnScaPosGl = laItemSeg[lnCount,4]
          ENDIF
        ENDFOR
        IF LEN(lcColorSep)> 0
          lnScaPosGl = lnScaPosGl - LEN(lcColorSep)-1
        ELSE
          lnScaPosGl = lnScaPosGl - 1 
        ENDIF
      ENDIF  
    ELSE
      llExtendedScale = .F.
    ENDIF 
    *  B609631,1 MMT 06/26/2011 Project Monitor should display 1 line for style color in Extended Size Scale companies[END]    
   *  B609143,1 MMT 02/15/2010 Error 'Too Many VAriables' in project monitor screen[End]
    *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [Start]
    lcActModel = oAriaApplication.ActiveModuleID  
    IF !EMPTY(LCRPRPTTM) AND LCRPRPTTM <> 'N/A' AND !EMPTY(loFormSet.LATEMPFL[1,1])AND USED(loFormSet.LATEMPFL[1,1])
    
      *  B609631,1 MMT 06/26/2011 Project Monitor should display 1 line for style color in Extended Size Scale companies[Start]
      IF llExtendedScale
        SELECT(loFormSet.LATEMPFL[1,1])
        lnFldsCnt = AFIELDS(laTmpFlds)
        FOR lnA =1 TO lnFldsCnt
          IF 'STYLE' $ UPPER(laTmpFlds[lnA,1]) AND !('CSTYLE' $ UPPER(laTmpFlds[lnA,1]))
            lcStyFld = laTmpFlds[lnA,1]
            SELECT DISTINCT CPRJ_ID,SUBSTR(&lcStyFld.,1,lnScaPosGl) AS 'STYCL' FROM (loFormSet.LATEMPFL[1,1]) WHERE LINENO = 0 INTO CURSOR 'PRJSTYCLR'
            SELECT 'PRJSTYCLR'
            SCAN 
              lcPrjID = PRJSTYCLR.CPRJ_ID
              lcStyClr = PRJSTYCLR.STYCL
              lcGathFld = ''
              SELECT (loFormSet.LATEMPFL[1,1])
              FOR lnB = 1 TO lnFldsCnt
                 IF (('QTY' $ UPPER(laTmpFlds[lnB ,1]) OR 'ORD' $ UPPER(laTmpFlds[lnB ,1]) OR 'BOOK' $ UPPER(laTmpFlds[lnB ,1]) OR 'PIK' $ UPPER(laTmpFlds[lnB ,1]) OR;
                   'POALO' $ UPPER(laTmpFlds[lnB,1]) OR 'NPCK' $ UPPER(laTmpFlds[lnB,1]) OR 'CUT' $ UPPER(laTmpFlds[lnB,1]) OR 'REC' $ UPPER(laTmpFlds[lnB,1]);
                   OR 'INT' $ UPPER(laTmpFlds[lnB,1]) OR 'OPN' $ UPPER(laTmpFlds[lnB,1]) OR 'STK' $ UPPER(laTmpFlds[lnB,1]) OR 'SHP' $ UPPER(laTmpFlds[lnB,1]);
                   OR 'PLAN' $ UPPER(laTmpFlds[lnB,1]) OR 'RA' $ UPPER(laTmpFlds[lnB,1]) OR 'NWO' $ UPPER(laTmpFlds[lnB,1]) OR 'WIP' $ UPPER(laTmpFlds[lnB,1]);
                   OR 'ALO' $ UPPER(laTmpFlds[lnB,1]) OR 'RET' $ UPPER(laTmpFlds[lnB,1])) AND laTmpFlds[lnB ,2] ='N') OR 'DPORCDAT' $ UPPER(laTmpFlds[lnB,1])
                  lcMemField = 'm.'+laTmpFlds[lnB,1]
                  lcGathFld = lcGathFld + IIF(!EMPTY(lcGathFld ) ,',','')+laTmpFlds[lnB,1]
                  lcFldName = laTmpFlds[lnB,1]
                  SELECT(loFormSet.LATEMPFL[1,1])
                  IF laTmpFlds[lnB ,2] ='N'
                    SUM &lcFldName. FOR CPRJ_ID = lcPrjID AND SUBSTR(&lcStyFld.,1,lnScaPosGl) = lcStyClr AND LINENO = 0 TO &lcMemField.
                  ELSE
                    SELECT MAX(DPORCDAT) as 'MAXRECD'  FROM (loFormSet.LATEMPFL[1,1]) WHERE  CPRJ_ID = lcPrjID AND ;
                           SUBSTR(&lcStyFld.,1,lnScaPosGl) = lcStyClr AND LINENO = 0 INTO CURSOR 'MAXRECDC'
                    m.DPORCDAT = MAXRECDC.MAXRECD
                  ENDIF  
                ENDIF
              ENDFOR
              SELECT(loFormSet.LATEMPFL[1,1])
              LOCATE
              m.&lcStyFld. = lcStyClr
              lcGathFld = lcGathFld +  IIF(!EMPTY(lcGathFld ) ,',','')+lcStyFld
              LOCATE FOR CPRJ_ID = lcPrjID AND SUBSTR(&lcStyFld.,1,lnScaPosGl) = lcStyClr AND LINENO = 0
              GATHER FIELDS LIKE &lcGathFld. MEMO MEMVAR    
              lnRecNo = RECNO()
              SCAN FOR CPRJ_ID = lcPrjID AND SUBSTR(&lcStyFld.,1,lnScaPosGl) = lcStyClr  AND RECNO()<> lnRecNo  AND  LINENO = 0 
                BLANK 
                DELETE
              ENDSCAN    
            ENDSCAN 
            EXIT   
          ENDIF
        ENDFOR 
      ENDIF
      *  B609631,1 MMT 06/26/2011 Project Monitor should display 1 line for style color in Extended Size Scale companies[Start]      
      FOR lnT = 1 TO ALEN(loFormSet.LATEMPFL,1)
        IF !EMPTY(loFormSet.LATEMPFL[lnT,1])
          x= 'T'+ALLTRIM(STR(lnT))
          &x. = loFormSet.LATEMPFL[lnT,1]
          loFormSet.LATEMPFL[lnT,2] = x
        ENDIF
      ENDFOR
      
      SELECT(loFormSet.LATEMPFL[1,1])
      LOCATE
      *  E302694,1 MMT 05/16/2010 Add Shortcut to Project Monitor to complete tasks[Start]
      lcBrFields = "LSelect:5 :H =''," + lcBrFields  
      *  E302694,1 MMT 05/16/2010 Add Shortcut to Project Monitor to complete tasks[End]
      = ARIABROW("'"+lcRpPrjTp+"'",LANG_MFPROJMON_PROJMON ,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, ;
        gnBrFSCol2,'','','CPRJ_ID','laBrowArr', .F.,loFormSet.LATEMPFL[1,1],.F.,loFormSet.LATEMPFL[1,1],'','','PMCTGDT',.F.,lcRpPrjTp,LCRPRPTTM)
    

    ELSE
      *  B609631,1 MMT 06/26/2011 Project Monitor should display 1 line for style color in Extended Size Scale companies[Start]

      IF llExtendedScale
        SELECT(lcTempFile)
        lnFldsCnt = AFIELDS(laTmpFlds)
        FOR lnA =1 TO lnFldsCnt
          IF 'STYLE' $ UPPER(laTmpFlds[lnA,1]) AND !('CSTYLE' $ UPPER(laTmpFlds[lnA,1]))
            lcStyFld = laTmpFlds[lnA,1]
            SELECT DISTINCT CPRJ_ID,SUBSTR(&lcStyFld.,1,lnScaPosGl) AS 'STYCL' FROM (lcTempFile) WHERE LINENO = 0  INTO CURSOR 'PRJSTYCLR'
            SELECT 'PRJSTYCLR'
            SCAN 
              lcPrjID = PRJSTYCLR.CPRJ_ID
              lcStyClr = PRJSTYCLR.STYCL
              lcGathFld = ''
              SELECT (lcTempFile)
              FOR lnB = 1 TO lnFldsCnt
                 IF (('QTY' $ UPPER(laTmpFlds[lnB ,1]) OR 'ORD' $ UPPER(laTmpFlds[lnB ,1]) OR 'BOOK' $ UPPER(laTmpFlds[lnB ,1]) OR 'PIK' $ UPPER(laTmpFlds[lnB ,1]) OR;
                   'POALO' $ UPPER(laTmpFlds[lnB,1]) OR 'NPCK' $ UPPER(laTmpFlds[lnB,1]) OR 'CUT' $ UPPER(laTmpFlds[lnB,1]) OR 'REC' $ UPPER(laTmpFlds[lnB,1]);
                   OR 'INT' $ UPPER(laTmpFlds[lnB,1]) OR 'OPN' $ UPPER(laTmpFlds[lnB,1]) OR 'STK' $ UPPER(laTmpFlds[lnB,1]) OR 'SHP' $ UPPER(laTmpFlds[lnB,1]);
                   OR 'PLAN' $ UPPER(laTmpFlds[lnB,1]) OR 'RA' $ UPPER(laTmpFlds[lnB,1]) OR 'NWO' $ UPPER(laTmpFlds[lnB,1]) OR 'WIP' $ UPPER(laTmpFlds[lnB,1]);
                   OR 'ALO' $ UPPER(laTmpFlds[lnB,1]) OR 'RET' $ UPPER(laTmpFlds[lnB,1])) AND laTmpFlds[lnB ,2] ='N') OR 'DPORCDAT' $ UPPER(laTmpFlds[lnB,1])
                  lcMemField = 'm.'+laTmpFlds[lnB,1]
                  lcGathFld = lcGathFld + IIF(!EMPTY(lcGathFld ) ,',','')+laTmpFlds[lnB,1]
                  lcFldName = laTmpFlds[lnB,1]
                  SELECT(lcTempFile)
                  IF laTmpFlds[lnB ,2] ='N'
                    SUM &lcFldName. FOR CPRJ_ID = lcPrjID AND SUBSTR(&lcStyFld.,1,lnScaPosGl) = lcStyClr AND LINENO = 0  TO &lcMemField.
                  ELSE
                    SELECT MAX(DPORCDAT) as 'MAXRECD'  FROM (lcTempFile) WHERE  CPRJ_ID = lcPrjID AND LINENO = 0 AND SUBSTR(&lcStyFld.,1,lnScaPosGl) = lcStyClr INTO CURSOR 'MAXRECDC'
                    m.DPORCDAT = MAXRECDC.MAXRECD
                  ENDIF  
                ENDIF
              ENDFOR
              SELECT(lcTempFile)
              LOCATE
              m.&lcStyFld. = lcStyClr
              lcGathFld = lcGathFld +  IIF(!EMPTY(lcGathFld ) ,',','')+lcStyFld
              LOCATE FOR CPRJ_ID = lcPrjID AND SUBSTR(&lcStyFld.,1,lnScaPosGl) = lcStyClr AND LINENO = 0
              GATHER MEMO MEMVAR FIELDS LIKE &lcGathFld.
              lnRecNo = RECNO()
              SCAN FOR CPRJ_ID = lcPrjID AND SUBSTR(&lcStyFld.,1,lnScaPosGl) = lcStyClr  AND RECNO()<> lnRecNo AND LINENO = 0 
                BLANK 
                DELETE 
              ENDSCAN 
              LOCATE 
            ENDSCAN 
            EXIT   
          ENDIF
        ENDFOR 
      ENDIF
      SELECT(lcTempFile)
      LOCATE 
      *  B609631,1 MMT 06/26/2011 Project Monitor should display 1 line for style color in Extended Size Scale companies[END]          
      *  E302694,1 MMT 05/16/2010 Add Shortcut to Project Monitor to complete tasks[Start]
      lcBrFields = "LSelect:5 :H =''," + lcBrFields  
      *  E302694,1 MMT 05/16/2010 Add Shortcut to Project Monitor to complete tasks[End]
    
    *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [End]
      = ARIABROW("'"+lcRpPrjTp+"'",LANG_MFPROJMON_PROJMON ,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, ;
        gnBrFSCol2,'','','CPRJ_ID','laBrowArr', .F.,.lcTempFile,.F.,.lcTempFile,'','','PMCTGDT',.F.,'PRJ',lcRpPrjTp)

    *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [Start]
    ENDIF
    oAriaApplication.ActiveModuleID  =  lcActModel
    *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [End]
  ENDWITH
  
  *  B609143,1 MMT 02/11/2010 Error 'Too Many VAriables' in project monitor screen[Start]
  IF LCRPRPTTM = 'N/A'
    FOR lnCountNum =  1 TO 85
      lcNum = ALLTRIM(STR(lnCountNum))
      lcFlNam ='m.' +'cOprID' + lcNum
      RELEASE &lcFlNam.
      lcFlNam ='m.' +'cOpDsc' + lcNum
      RELEASE &lcFlNam.
      lcFlNam ='m.' +'COPCTG' + lcNum
      RELEASE &lcFlNam.
      lcFlNam ='m.' +'dAcFnh'+ lcNum
      RELEASE &lcFlNam.
      lcFlNam ='m.' + 'cTskSt' + lcNum
      RELEASE &lcFlNam.
      lcFlNam ='m.' + 'DClFnh'+ lcNum
      RELEASE &lcFlNam.
    ENDFOR
    SELECT(lcTempFile)
    DIMENSION laTmpStru[1,18]
    aFields(laTmpStru)
    FOR lnFldCnt = 1 TO ALEN(laTmpStru,1)
      lcFlNam ='m.' + laTmpStru[lnFldCnt ,1]
      RELEASE &lcFlNam.
    ENDFOR
    FOR lnI =1 TO 85
        lcI = ALLTRIM(STR(lnI))
        RELEASE m.cOprID&lcI.
        RELEASE m.cOpDsc&lcI.
        RELEASE m.COPCTG&lcI.
        RELEASE m.cTskSt&lcI.
        RELEASE m.dAcFnh&lcI.
        RELEASE m.DClFnh&lcI.
        RELEASE m.mOPCOM&lcI. 
    ENDFOR
    RELEASE lcFlNam,laTmpStru,lnFldCnt,lnCountNum,lcNum,lcI,lnI    
    IF USED(loFormSet.lcTempFile2)
      USE IN (loFormSet.lcTempFile2)
    ENDIF 
    IF USED(loFormSet.lcTempFile3)
      USE IN (loFormSet.lcTempFile3)
    ENDIF 
    IF USED(loFormSet.lcTempFile)
      USE IN (loFormSet.lcTempFile)
    ENDIF 
  ELSE
    FOR lnI = 1 TO ALEN(loFormSet.LATEMPFL,1)

      IF !EMPTY(loFormSet.LATEMPFL[lnI,1])
        x= 'T'+ALLTRIM(STR(lnT))
        RELEASE &x. 
      ENDIF
      RELEASE x 

      IF !EMPTY(loFormSet.LATEMPFL[lnI,1]) AND USED(loFormSet.LATEMPFL[lnI,1])
        SELECT(loFormSet.LATEMPFL[lnI,1])
        DIMENSION laTmpStru[1,18]
        STORE '' TO laTmpStru
        aFields(laTmpStru)
        FOR lnFldCnt = 1 TO ALEN(laTmpStru,1)
          lcFlNam ='m.' + laTmpStru[lnFldCnt ,1]
          RELEASE &lcFlNam.
        ENDFOR
        USE IN (loFormSet.LATEMPFL[lnI,1])
      ENDIF 
    ENDFOR
    RELEASE lnFldCnt ,lcFlNam ,lnI ,laTmpStru
    DIMENSION loFormSet.LATEMPFL[1],loFormSet.LATEMPACT[1],loFormSet.LATABLEUSE[1],;
              loFormSet.LATABLEFLD[1],loFormSet.LATABLEPROF[1],loFormSet.LAFLDMAP[1]   
    STORE '' TO loFormSet.LATEMPFL,loFormSet.LATEMPACT,loFormSet.LATABLEUSE,;
                loFormSet.LATABLEFLD,loFormSet.LATABLEPROF,loFormSet.LAFLDMAP              

  ENDIF 
  
  RELEASE lcPrjTitle ,lcMajPic,lcRpNotfy  ,lcRpOprStt ,lcRpOprtyp ,lcRpPrjStt ,lcTemp,lcActModel ,;
  lcRpRprt   ,lcRpRptly  ,lcRpSrtBy  ,lcRpTemplt ,llRpShLate ,lcSortBy   ,lcOldValue ,LNRPDYCMP ,;
  lcVarChr ,laTmpltOpr,lcTmpTasks,lcRpTaskNom ,llRpShWDy ,lnSetUps ,lcVarPrj ,LCRPRPTTM ,;
  laTmpVal,laTmpDes,laPrjDesc,laPrjVals,lcTaskRef ,lnTaskCnt ,lcSOProf1,lcSOProf2 ,lcSOProf3 ,;
  lcSOProf1DESC ,lcSOProf2DESC ,lcSOProf3DESC ,lcPOProf1,lcPOProf2 ,lcPOProf3 ,lcPOProf1DESC ,;
  lcPOProf3DESC ,lcPOProf2DESC ,laPoCustFld,lcNomDesc ,lcBrFields ,lnPrjPos ,lcSelPrj, lcFltExp ,;
  lnStyPos ,lcSelSty ,lnPrjDPos ,lcSelDate ,lnmajorlen ,lcStyleMask ,lnNtfPos ,lcNTFSel ,llNtfySelected ,lnTaskNum ,;
  lnTskPos ,lcTskSel ,llTskSelected 
  RELEASE m.nTaskStat ,lcCodeNo,lnCodeRec,laSortVal,laSortDesc,loParentForm,llAddRec
  RELEASE B,A ,C , D,nDyPstCm2,e,lnAccPos ,lcSelAcc ,llAccSelected ,lnCmpDPos ,lcCmpDate ,lnVenPos ,lcSelVend ,llVendSelected
  RELEASE lcFilTasks ,lnPCmpDPos ,lcPCmpDate ,m.cprj_typ,m.cprj_id ,m.DREQ_FNSH,m.ACCOUNT ,m.VENDOR ,m.cstyle ,m.LINENO
  RELEASE lnPOs ,lcFldScat,lcMemFld ,llTaskFnd ,lcPrFFld ,lcStyleClr,m.ScaleDsc,m.Desc1     ,lnR , m.cProjStat,m.cProjType
  RELEASE lcFldName ,ldSD ,lnI ,lcI,lcDescVal ,lnIndex ,lcXmlData ,lobjDOMDocument,loRoot ,loVariable, lcName, lcDataType, lcValue
  RELEASE lcPrjID ,T2,T3,lcTempFile ,lcTaskIdCnt ,lnBackClor ,x,lnT,lnU,lnA,t3,t1,t2
  *  B609143,1 MMT 02/11/2010 Error 'Too Many VAriables' in project monitor screen[End]  
  
  *! B609028,1 MMT 10/07/2009 Fix bug of not exporting Task notes to excel[Start]
  =lfvScope(loFormSet)
  *! B609028,1 MMT 10/07/2009 Fix bug of not exporting Task notes to excel[End]
  RETURN
  *-- End of lfvScope




  *!***************************************************************************
  *! Name      : lfvPrjID
  *! Developer : Ahmad Shoukry Mohammed (ASM)
  *! Date      : 08/30/2005
  *! Purpose   : Validation function for the project passed.
  *!***************************************************************************
  *! Parameters: None
  *!***************************************************************************
  *! Returns   : None
  *!***************************************************************************
PROCEDURE lfvPrjID

  PRIVATE lcPrjTitle, lcBrFields, lnCurTag, lnCurAlias, lnSoftSeek, lnOption

  lcPrjID = EVALUATE(OGSYS18())

  lcPrj  = STRTRAN(lcPrjID ," ","")

  IF EMPTY(lcPrj)
    lcPrjID  = ""
  ENDIF

  IF !EMPTY(lcOldValue) AND lcPrjID  = lcOldValue
    RETURN
  ENDIF

  lcFldLocNam = OGSYS18()

  lnCurAlias   = SELECT()
  SELECT PmPrjHd
  lnCurTag     = VAL(SYS(21))
  SET ORDER TO TAG PmPrjHd
  *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [Start]
  *!*  lcBrFields   = "cPrj_ID  : H='Project ID',"+;
  *!*                 "cStyle   : H='Style',"+;
  *!*                 "cPrj_SDsc: H='Project Description'"
  lcBrFields   = "cPrj_ID  : H='Project ID',"+;
    "cStyle   : H='Style',"+;
    "cPrj_SDsc: H='Project Description'"+;
    ",LINENO:H='Line No.'"
  *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [End]
  DO CASE
    CASE lcRpPrjTp  = 'C'
      lcPrjTitle = 'Cutting Tickets Projects'
    CASE lcRpPrjTp  = 'P'      && PO
      lcPrjTitle = 'Purchase Orders Projects'
    CASE lcRpPrjTp  = 'A'      && Adorment Order
      lcPrjTitle = 'Adorment Order Projects'
    CASE lcRpPrjTp  = 'D'      && Dye Order
      lcPrjTitle = 'Dye Order Projects'
    CASE lcRpPrjTp  = 'N'      && Inter-Location PO
      lcPrjTitle = 'Inter-Location PO Projects'
    CASE lcRpPrjTp  = 'R'      && Return PO
      lcPrjTitle = 'Return PO Projects'
    CASE lcRpPrjTp  = 'O'      && SO
      lcPrjTitle = 'Sales Orders Projects'
    CASE lcRpPrjTp  = 'T'      && EDI Order
      lcPrjTitle = 'EDI Order Projects'
    OTHERWISE
      lcPrjTitle = 'Project ID'
  ENDCASE

  llBrowse = IIF(TYPE('llBrowse') = 'U', .F., llBrowse)
  DIMENSION latemp[1]
  IF llBrowse .OR. '?' $ lcPrjID
    GO TOP
    lcPrjID  = IIF(ARIABROW([lcRpPrjTp],lcPrjTitle, ;
      gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','','PMPRJHD.cPrj_ID','laTemp'),;
      PmPrjHd.cprj_id, SPACE(6))
  ELSE
    IF !EMPTY(lcPrjID)
      IF !SEEK(lcRpPrjTp + lcPrjID)
        lnSoftSeek = RECNO(0)
        IF BETWEEN(lnSoftSeek, 1, RECCOUNT())
          GO lnSoftSeek
        ELSE
          GO TOP
        ENDIF
        lcPrjID  = IIF(ARIABROW([lcRpPrjTp],lcPrjTitle, ;
          gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','','PMPRJHD.cPrj_ID','laTemp'),;
          PmPrjHd.cprj_id, SPACE(6))
      ENDIF
    ENDIF
  ENDIF

  &lcFldLocNam = lcPrjID

  SET ORDER TO (lnCurTag)
  SELECT (lnCurAlias)


  RETURN
  *-- End of lfvPrjID


  *!***************************************************************************
  *! Name      : lfvStyMaj
  *! Developer : Ahmad Shoukry Mohammed (ASM)
  *! Date      : 08/30/2005
  *! Purpose   : Validate style major in range entered in grid.
  *!***************************************************************************
  *! Parameters: None
  *!***************************************************************************
  *! Returns   : None
  *!***************************************************************************
PROCEDURE lfvStyMaj

  lcMStyle = EVALUATE(OGSYS18())

  lcSty = STRTRAN(lcMStyle," ","")

  IF EMPTY(lcSty)
    lcMStyle = ""
  ENDIF

  IF !EMPTY(lcOldValue) AND lcMStyle = lcOldValue
    RETURN
  ENDIF

  lcFldLocNam = OGSYS18()

  IF !EMPTY(lcMStyle) AND !SEEK(lcMStyle,'STYLE')
    lcMStyle = gfStyBrw('M',"","",.F.)
    &lcFldLocNam = lcMStyle
  ENDIF


  RETURN
  *-- End of lfvStyMaj



  *!***************************************************************************
  *! Name      : lfvPrjTyp
  *! Developer : Ahmad Shoukry Mohammed (ASM)
  *! Date      : 08/30/2005
  *! Purpose   : Project type valid function
  *!***************************************************************************
  *! Parameters: None
  *!***************************************************************************
  *! Returns   : None
  *!***************************************************************************
PROCEDURE lfvPrjTyp
  PARAMETERS llCallClr


  DO CASE
    CASE lcRpPrjTp $ 'OT'
      lcInFile   = 'OrdHdr'
      lcField    = 'Order'
      lcAccCode  = 'account'
      lcAccOrVen = 'account'

      DIMENSION laSortDesc[3,1]
      DIMENSION laSortVal[3,1]

      laSortDesc[1,1] = LANG_MFPROJMON_ORDER
      laSortVal[1,1]  = 'O'
      laSortDesc[2,1] = LANG_MFPROJMON_ACCOUNT
      laSortVal[2,1]  = 'A'
      laSortDesc[3,1] = LANG_MFPROJMON_REQDATE
      laSortVal[3,1]  = 'R'



      lcPrjTitle  = LANG_MFPROJMON_ORDNUM
      lcRpSrtBy   = 'O'

    CASE lcRpPrjTp = 'C'
      lcInFile   = 'CutTktH'
      lcField    = 'CutTkt'
      lcAccCode  = 'account'
      lcAccOrVen = ''

      DIMENSION laSortDesc[2,1]
      DIMENSION laSortVal[2,1]

      laSortDesc[1,1] = LANG_MFPROJMON_CTKTKT
      laSortVal[1,1]  = 'C'
      laSortDesc[2,1] = LANG_MFPROJMON_REQDATE
      laSortVal[2,1]  = 'R'


      lcPrjTitle  = LANG_MFPROJMON_CTNO
      lcRpSrtBy   = 'C'

    CASE lcRpPrjTp $ 'PADNR'
      lcInFile   = 'POsHdr'
      lcField    = 'PO'
      lcAccCode  = 'Vendor'
      lcAccOrVen = 'vendor'

      DIMENSION laSortDesc[3,1]
      DIMENSION laSortVal[3,1]

      laSortDesc[1,1] = LANG_MFPROJMON_ORDER
      laSortVal[1,1]  = 'O'
      laSortDesc[2,1] = LANG_MFPROJMON_VENDOR
      laSortVal[2,1]  = 'V'
      laSortDesc[3,1] = LANG_MFPROJMON_REQDATE
      laSortVal[3,1]  = 'R'
      lcRpSrtBy   = 'O'
      lcPrjTitle  = LANG_MFPROJMON_ORDNUM

    CASE lcRpPrjTp $ 'SHM'
      DIMENSION laSortDesc[2,1]
      DIMENSION laSortVal[2,1]
      lcPrjTitle  = LANG_MFPROJMON_PROJID
      IF lcRpPrjTp = 'H'
        laSortDesc[1,1] = LANG_MFPROJMON_PROJID
      ELSE
        laSortDesc[1,1] = IIF(lcRpPrjTp = 'S',LANG_MFPROJMON_STYLE,LANG_MFPROJMON_MATERIAL)
      ENDIF
      laSortVal[1,1]  = 'S'
      laSortDesc[2,1] = LANG_MFPROJMON_REQDATE
      laSortVal[2,1]  = 'R'
      lcRpSrtBy   = 'S'

  ENDCASE

  lnPrjPos = ASCAN(loogScroll.laogFxFlt,'PMPRJHD.CPRJ_ID')
  lcSelPrj =''
  IF lnPrjPos  <> 0
    lnPrjPos = ASUBSCRIPT(loogScroll.laogFxFlt,lnPrjPos ,1)
    lcSelPrj  =loogScroll.laogFxFlt[lnPrjPos ,6]
    IF !EMPTY(lcSelPrj) AND USED(lcSelPrj)
      SELECT (lcSelPrj)
      LOCATE
      IF !EOF()
        ZAP
      ENDIF
    ENDIF
  ENDIF


  *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [Start]
  SELECT PMRPRTM
  DO CASE
    CASE lcRpPrjTp $ 'CAD'
      lcWhenType =  'C'
    CASE lcRpPrjTp $ 'PNR'
      lcWhenType = 'P'
    CASE lcRpPrjTp $ 'OT'
      lcWhenType = 'O'
    OTHERWISE
      lcWhenType =  lcRpPrjTp
  ENDCASE
  DIMENSION laTmpVal[1],laTmpDes[1]
  STORE '' TO laTmpVal,laTmpDes
  laTmpVal[1] = 'N/A'
  laTmpDes[1] = 'N/A'

  =gfSeek(lcWhenType)
  SCAN REST WHILE CTMP_TYPE +CTMP_CODE =lcWhenType
    DIMENSION laTmpVal[ALEN(laTmpVal,1)+1],laTmpDes[ALEN(laTmpDes,1)+1]
    laTmpVal[ALEN(laTmpVal,1)] = CTMP_CODE
    *B609555,1 MMT 03/27/2011 Task List screen gives error while completing task[Start]
    *laTmpDes[ALEN(laTmpDes,1)] = CTMP_CODE
    laTmpDes[ALEN(laTmpDes,1)] =  CTMP_CODE+' - '+CTMP_DSC
    *B609555,1 MMT 03/27/2011 Task List screen gives error while completing task[End]
  ENDSCAN
  IF !llCallClr
    LCRPRPTTM ='N/A'
  ENDIF   
  *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [End]


  IF !llCallClr
    =ClearRead()
  ENDIF



  RETURN
  *-- End of lfvPrjTyp


  *!***************************************************************************
  *! Name      : lfvUserId
  *! Developer : Ahmad Shoukry Mohammed (ASM)
  *! Date      : 08/30/2005
  *! Purpose   : Browse function for avilable templates.
  *!***************************************************************************
  *! Parameters: None
  *!***************************************************************************
  *! Returns   : None
  *!***************************************************************************
PROCEDURE lfvUserId
  PRIVATE lcTitle, lcBrFields, lnCurAlias

  lnCurAlias   = SELECT(0)
  lcUser_Id = EVALUATE(OGSYS18())
  lcUser = STRTRAN(lcUser_Id," ","")
  IF EMPTY(lcUser)
    lcUser_Id = ""
  ENDIF
  IF !EMPTY(lcOldValue) AND lcUser_Id = lcOldValue
    RETURN
  ENDIF
  lcFldLocNam = OGSYS18()
  SELECT SYUUSER
  GO TOP
  IF EOF()
    SELECT (lnCurAlias)
    lcUser_Id = PADR(ALLTRIM(lcUser_Id),10)
  ELSE
    *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
    *lcBrFields = "cUser_Id : H='User ID',cUsr_Name : H='User Name'"
    lcBrFields = "cUser_Id : H='"+LANG_MFPROJMON_USER+"',cUsr_Name : H='"+LANG_MFPROJMON_USERNAME+"'"
    *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
    lcSetExact = SET('EXACT')
    SET EXACT ON
    lcTitle    = LANG_MFPROJMON_USER
    DIMENSION latemp[1]
    IF !EMPTY(lcUser_Id) AND !SEEK(ALLTRIM(lcUser_Id))
      lcUser_Id = IIF(ARIABROW('',lcTitle,;
        gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','','SYUUSER.cUser_Id','latemp'),;
        SYUUSER.cUser_Id, SPACE(10))
      &lcFldLocNam = lcUser_Id
      lcRpNotfy = lcUser_Id
    ENDIF
    SET EXACT &lcSetExact
  ENDIF
  SELECT (lnCurAlias)


  RETURN
  *-- End of lfvUserId


  *!***************************************************************************
  *! Name      : lfvPrjTmp
  *! Developer : Ahmad Shoukry Mohammed (ASM)
  *! Date      : 08/30/2005
  *! Purpose   : Browse function for avilable templates.
  *!***************************************************************************
  *! Parameters: None
  *!***************************************************************************
  *! Returns   : None
  *!***************************************************************************
FUNCTION lfvPrjTmp

  PRIVATE lcTitle, lcBrFields, lnCurAlias

  lnCurAlias   = SELECT(0)
  *! B609028,1 MMT 10/07/2009 Fix bug of not exporting Task notes to excel[Start]
  *SELECT PmRprTm
  SELECT PMPTHHD
  *! B609028,1 MMT 10/07/2009 Fix bug of not exporting Task notes to excel[End]
  =gfSeek('')
  GO TOP
  DIMENSION latemp[1]
  *! B609028,1 MMT 10/07/2009 Fix bug of not exporting Task notes to excel[Start]
  *lcBrFields = "cTmp_Code : H='"+LANG_MFPROJMON_TEMPID+"',cTmp_Dsc  : H='"+LANG_MFPROJMON_DESC+"'"
  lcBrFields = "CPATH_ID : H='"+LANG_MFPROJMON_TEMPID+"',CPATH_DSC : H='"+LANG_MFPROJMON_DESC+"'"
  *! B609028,1 MMT 10/07/2009 Fix bug of not exporting Task notes to excel[End]
  lcTitle    = LANG_MFPROJMON_TEMPLATES

  *! B609028,1 MMT 10/07/2009 Fix bug of not exporting Task notes to excel[Start]
  *!*  IF !EMPTY(lcRpTemplt) AND !SEEK(PADR(ALLTRIM(lcRpTemplt),3))
  *!*    lcRpTemplt = IIF(ARIABROW('',lcTitle,;
  *!*                     gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','','PmRprTm.cTmp_Code','latemp'),;
  *!*                     PmRprTm.cTmp_Code, SPACE(3))
  IF !EMPTY(lcRpTemplt) AND !SEEK(PADR(ALLTRIM(lcRpTemplt),4))
    lcRpTemplt = IIF(ARIABROW('',lcTitle,;
      gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','','PMPTHHD.CPATH_ID','latemp'),;
      PMPTHHD.CPATH_ID, SPACE(4))
    *! B609028,1 MMT 10/07/2009 Fix bug of not exporting Task notes to excel[End]
    IF !EMPTY(lcRpTemplt)
      lnPOs = ASCAN(loogScroll.laogFxFlt,'PMPRJDT.COPRT_ID')
      lcSelTask =''
      IF lnPOs <> 0
        lnPOs = ASUBSCRIPT(loogScroll.laogFxFlt,lnPOs,1)
        lcSelTask  = loogScroll.laogFxFlt[lnPos ,6]
        IF !EMPTY(lcSelTask) AND USED(lcSelTask)
          SELECT (lcSelTask)
          ZAP
        ENDIF
      ENDIF
    ENDIF
  ENDIF

  SELECT (lnCurAlias)

  RETURN
  *-- End of lfvPrjTmp

  *!***************************************************************************
  *! Name      : lfFillSrBy
  *! Developer : Ahmad Shoukry Mohammed (ASM)
  *! Date      : 08/30/2005
  *! Purpose   : Fill Sort by Popup array
  *!***************************************************************************
  *! Parameters: None
  *!***************************************************************************
  *! Returns   : None
  *!***************************************************************************
FUNCTION lfFillSrBy

  DIMENSION laSortDesc[IIF(oAriaApplication.ActiveModuleID= "MF" , 2 , 3) ,1]
  DIMENSION laSortVal[IIF(oAriaApplication.ActiveModuleID= "MF" , 2 , 3),1]

  DO CASE
    CASE oAriaApplication.ActiveModuleID= "MF"
      laSortDesc[1,1] =  LANG_MFPROJMON_CTKTKT
      laSortVal[1,1]  = 'C'
      laSortDesc[2,1] =  LANG_MFPROJMON_REQDATE
      laSortVal[2,1]  = 'R'

      lcPrjTitle  = LANG_MFPROJMON_CTNO
      lcRpSrtBy   = 'C'

    CASE oAriaApplication.ActiveModuleID= "SO"
      laSortDesc[1,1] = LANG_MFPROJMON_ORDER
      laSortVal[1,1]  = 'O'
      laSortDesc[2,1] = LANG_MFPROJMON_ACCOUNT
      laSortVal[2,1]  = 'A'
      laSortDesc[3,1] = LANG_MFPROJMON_REQDATE
      laSortVal[3,1]  = 'R'
      lcPrjTitle  = LANG_MFPROJMON_ORDNUM
      lcRpSrtBy   = 'O'

    CASE oAriaApplication.ActiveModuleID= "PO"
      laSortDesc[1,1] = LANG_MFPROJMON_ORDER
      laSortVal[1,1]  = 'O'
      laSortDesc[2,1] = LANG_MFPROJMON_VENDOR
      laSortVal[2,1]  = 'V'
      laSortDesc[3,1] = LANG_MFPROJMON_REQDATE
      laSortVal[3,1]  = 'R'

      lcRpSrtBy   = 'O'
      lcPrjTitle  =  LANG_MFPROJMON_ORDNUM
  ENDCASE

  RETURN
  *-- End of lfFillSrBy



  *!***************************************************************************
  *! Name      : lfOldValue
  *! Developer : Ahmad Shoukry Mohammed (ASM)
  *! Date      : 08/30/2005
  *! Purpose   : Function to store old value of the current filed.
  *!***************************************************************************
  *! Parameters: None
  *!***************************************************************************
  *! Returns   : None
  *!***************************************************************************
FUNCTION lfOldValue

  *lcOldValue = EVALUATE(OGSYS18())

  RETURN
  *-- End of lfOldValue





  *!*  ************************* temp

  *!*  FUNCTION lfCheckFilter()
  *!*  LPARAMETERS lnArrayType, lcFilter
  *!*  LOCAL lcReturn, lnPOS
  *!*  lcReturn = ""
  *!*  DO CASE
  *!*  CASE lnArrayType = 1
  *!*    lnPOS = ASCAN(loOgScroll.laOGFxFlt,lcFilter)
  *!*    IF lnPos > 0
  *!*      lnPOS = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnPos,1)
  *!*      lcReturn = loOgScroll.laOGFxFlt[lnPOS,6]
  *!*    ENDIF
  *!*  CASE lnArrayType = 2
  *!*    lnPOS = ASCAN(loOgScroll.laOGHDFlt,lcFilter)
  *!*    IF lnPos > 0
  *!*      lnPOS = ASUBSCRIPT(loOgScroll.laOGHDFlt,lnPos,1)
  *!*      lcReturn = loOgScroll.laOGHDFlt[lnPOS,6]
  *!*    ENDIF
  *!*  CASE lnArrayType = 3
  *!*    lnPOS = ASCAN(loOgScroll.laOGvrFlt,lcFilter)
  *!*    IF lnPos > 0
  *!*      lnPOS = ASUBSCRIPT(loOgScroll.laOGvrFlt,lnPos,1)
  *!*      lcReturn = loOgScroll.laOGvrFlt[lnPOS,6]
  *!*    ENDIF
  *!*  ENDCASE
  *!*  RETURN lcReturn


  *!*  lcTempAccount = lfCheckFilter(1, 'ORDHDR.ACCOUNT')
  *!*  IF !EMPTY(lcTempAccount) AND USED(lcTempAccount)
  *!*    SELECT &lcTempAccount
  *!*    llACCOUNT = (RECCOUNT() > 0)
  *!*    IF llACCOUNT
  *!*      SCAN
  *!*
  *!*      ENDSCAN
  *!*    ENDIF
  *!*  ENDIF

  *!***************************************************************************
  *! Name      : lfDefTemp
  *! Developer : Ahmad Shoukry Mohammed (ASM)
  *! Date      : 08/30/2005
  *! Purpose   : Browse function for avilable templates.
  *!***************************************************************************
  *! Parameters: None
  *!***************************************************************************
  *! Returns   : None
  *!***************************************************************************
FUNCTION lfDefTemp
  LOCAL lnCurAlias,lnDataSessionID

  *! B131422,1 ASM 03/12/2006 Grid not Maximized and Report Template not working [Start]
  IF TYPE('_Screen.ActiveForm.Parent.lcRpTemplt')='C'
    lcRpTemplt = _SCREEN.ACTIVEFORM.PARENT.lcRpTemplt
    RETURN lcRpTemplt
  ENDIF
  *! B131422,1 ASM 03/12/2006 Grid not Maximized and Report Template not working [End]

  lnCurAlias = SELECT(0)

  IF !USED('SYUUSER')
    gfOpenTable('SYUUSER','CUSER_ID')
  ENDIF

  IF !USED('PMRPRTM')
    gfOpenTable('PMRPRTM','PMRPRTM')
  ENDIF

  SELECT SYUUSER
  IF gfSeek(oAriaApplication.User_ID)
    IF !EMPTY(SYUUSER.CTMP_CODE)
      lcRpTemplt = SYUUSER.CTMP_CODE
    ELSE
      SELECT PMRPRTM
      =gfSeek('')
      LOCATE
      lcRpTemplt = PMRPRTM.CTMP_CODE
    ENDIF
  ENDIF
  *! B609028,1 MMT 10/07/2009 Fix bug of not exporting Task notes to excel[Start]
  lcRpTemplt = ''
  *! B609028,1 MMT 10/07/2009 Fix bug of not exporting Task notes to excel[End]
  *SET DATASESSION TO lnDataSessionID

  RETURN lcRpTemplt
  *-- End of lfDefTemp

  *!***************************************************************************
  *! Name      : lfProjType
  *! Developer : Ahmad Shoukry Mohammed (ASM)
  *! Date      : 08/30/2005
  *! Purpose   : Browse function for avilable templates.
  *!***************************************************************************
  *! Parameters: None
  *!***************************************************************************
  *! Returns   : None
  *!***************************************************************************
FUNCTION lfProjType
  LPARAMETERS lcPrj_Typ

  LOCAL lcReturn
  lcReturn = ""
  DO CASE
    CASE lcPrj_Typ = "C"
      lcReturn = LANG_MFPROJMON_CTKTKT
    CASE lcPrj_Typ = "P"
      lcReturn = LANG_MFPROJMON_PO
    CASE lcPrj_Typ = "A"
      lcReturn = LANG_MFPROJMON_ADORORDER
    CASE lcPrj_Typ = "D"
      lcReturn = LANG_MFPROJMON_DYEORDER
    CASE lcPrj_Typ = "N"
      lcReturn = LANG_MFPROJMON_INTRPO
    CASE lcPrj_Typ = "R"
      lcReturn = LANG_MFPROJMON_RETPO
    CASE lcPrj_Typ = "O"
      lcReturn = LANG_MFPROJMON_SALORD
    CASE lcPrj_Typ = "T"
      lcReturn = LANG_MFPROJMON_TMPEDI
    CASE lcPrj_Typ = "S"
      lcReturn = LANG_MFPROJMON_STYLE
    CASE lcPrj_Typ = "H"
      lcReturn = LANG_MFPROJMON_OTHER
    CASE lcPrj_Typ = "M"
      lcReturn = LANG_MFPROJMON_MATERIAL
  ENDCASE

  RETURN lcReturn
  *-- End of lfProjType.

  *!***************************************************************************
  *! Name      : lfGetProjID
  *! Developer : Ahmad Shoukry Mohammed (ASM)
  *! Date      : 08/30/2005
  *! Purpose   : Browse function for avilable templates.
  *!***************************************************************************
  *! Parameters: None
  *!***************************************************************************
  *! Returns   : None
  *!***************************************************************************
FUNCTION lfGetProjID
  LPARAMETERS lcPrj_Typ,lcPrj_ID,lcStyle

  DO CASE
    CASE lcPrj_Typ = "C"
      *Cutting Ticket
      *  E302650,3 MMT 12/29/2009 make project monitor work based on report templated selected [Start]
      oAriaApplication.ActiveModuleID  = 'MF'
      *  E302650,3 MMT 12/29/2009 make project monitor work based on report templated selected [End]
      =oAriaApplication.DoProgram('AWRMFCUTKT','"'+lcPrj_ID+'"',.F.,'MF')
    CASE lcPrj_Typ = "P"
      *Purchase Order
      *lcBusDocu, lcWorkOrdType, lcPoNo
      *  E302650,3 MMT 12/29/2009 make project monitor work based on report templated selected [Start]
      oAriaApplication.ActiveModuleID  = 'PO'
      *  E302650,3 MMT 12/29/2009 make project monitor work based on report templated selected [End]

      =oAriaApplication.DoProgram('AWRPOSTY','"P","P","'+lcPrj_ID+'"',.F.,'PO')
    CASE lcPrj_Typ = "A"
      *Adorment Order
      *lcBusDocu, lcWorkOrdType, lcPoNo
     *  E302650,3 MMT 12/29/2009 make project monitor work based on report templated selected [Start]
      oAriaApplication.ActiveModuleID  = 'MF'
      *  E302650,3 MMT 12/29/2009 make project monitor work based on report templated selected [End]
      
      =oAriaApplication.DoProgram('AWRMFADPO','"P","A","'+lcPrj_ID+'"',.F.,'MF')
    CASE lcPrj_Typ = "D"
      *Dye Order
      *lcBusDocu, lcWorkOrdType, lcPoNo
      *  E302650,3 MMT 12/29/2009 make project monitor work based on report templated selected [Start]
      oAriaApplication.ActiveModuleID  = 'MF'
      *  E302650,3 MMT 12/29/2009 make project monitor work based on report templated selected [End]
      
      =oAriaApplication.DoProgram('AWRMFDPO','"P","D","'+lcPrj_ID+'"',.F.,'MF')
    CASE lcPrj_Typ = "N"
      *Inter-Location PO
      *lcBusDocu, lcWorkOrdType, lcPoNo
      *  E302650,3 MMT 12/29/2009 make project monitor work based on report templated selected [Start]
      oAriaApplication.ActiveModuleID  = 'PO'
      *  E302650,3 MMT 12/29/2009 make project monitor work based on report templated selected [End]

      =oAriaApplication.DoProgram('AWRPOINTRC','"N","N","'+lcPrj_ID+'"',.F.,'PO')
    CASE lcPrj_Typ = "R"
      *Return PO
      *lcBusDocu, lcWorkOrdType, lcPoNo
      *  E302650,3 MMT 12/29/2009 make project monitor work based on report templated selected [Start]
      oAriaApplication.ActiveModuleID  = 'PO'
      *  E302650,3 MMT 12/29/2009 make project monitor work based on report templated selected [End]
      
      =oAriaApplication.DoProgram('AWRRETPO','"R","P","'+lcPrj_ID+'"',.F.,'PO')
    CASE lcPrj_Typ = "O"
      *Sales Order
      *lcOrdType,lcOrderNo
      *  E302650,3 MMT 12/29/2009 make project monitor work based on report templated selected [Start]
      oAriaApplication.ActiveModuleID  = 'SO'
      *  E302650,3 MMT 12/29/2009 make project monitor work based on report templated selected [End]
      
      =oAriaApplication.DoProgram('AWRSOORD','"O","'+lcPrj_ID+'"',.F.,'SO')
    CASE lcPrj_Typ = "T"
      *EDI Order
      *lcOrdType,lcOrderNo
      *  E302650,3 MMT 12/29/2009 make project monitor work based on report templated selected [Start]
      oAriaApplication.ActiveModuleID  = 'SO'
      *  E302650,3 MMT 12/29/2009 make project monitor work based on report templated selected [End]
      
      =oAriaApplication.DoProgram('AWRSOORD','"T","'+lcPrj_ID+'"',.F.,'SO')
    CASE lcPrj_Typ = "S"
      *Style
      *lcpStyle,lcpColor
      *  E302650,3 MMT 12/29/2009 make project monitor work based on report templated selected [Start]
      oAriaApplication.ActiveModuleID  = 'IC'
      *  E302650,3 MMT 12/29/2009 make project monitor work based on report templated selected [End]
      
      =oAriaApplication.DoProgram('AWRICSTYLE','"'+lcStyle+'",""',.F.,'IC')

    CASE lcPrj_Typ = "M"
      *Material
      PRIVATE lcICInvType
      lcICInvType = '0002'
      *  E302650,3 MMT 12/29/2009 make project monitor work based on report templated selected [Start]
      oAriaApplication.ActiveModuleID  = 'MA'
      *  E302650,3 MMT 12/29/2009 make project monitor work based on report templated selected [End]
      
      =oAriaApplication.DoProgram('AWRICITEM','"0002","'+lcStyle+'",.F.',.F.)


    CASE lcPrj_Typ = "H"
      *Other
      lcOldActiveMode = oAriaApplication.ActiveModuleID
      oAriaApplication.ActiveModuleID ='IC'
      *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [Start]
      *=oAriaApplication.DoProgram('AWRMFPROJ','"H","'+lcPrj_ID+'","'+SPACE(12)+'"," ",.F.,.F.,.T.',.F.)
      =oAriaApplication.DoProgram('AWRMFPROJ','"H","'+lcPrj_ID+'","'+SPACE(19)+'",0,.F.,.F.,.F.,.T.',.F.)
      *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [End]
      oAriaApplication.ActiveModuleID  = lcOldActiveMode

  ENDCASE
  *-- End of lfGetProjID.

  *!*  *!***************************************************************************
  *!*  *! Name      : lfTaskStat
  *!*  *! Developer : Ahmad Shoukry Mohammed (ASM)
  *!*  *! Date      : 08/30/2005
  *!*  *! Purpose   : Get the Task Status.
  *!*  *!***************************************************************************
  *!*  *! Parameters: None
  *!*  *!***************************************************************************
  *!*  *! Returns   : None
  *!*  *!***************************************************************************
  *!*  FUNCTION lfTaskStat
  *!*  LPARAMETERS lcTask

  *!*  LOCAL lcTaskStat
  *!*  lcTaskStat = ""
  *!*  DO CASE
  *!*    CASE EVALUATE("dActStrt"+lcTask) > EVALUATE("dEstStrt"+lcTask)
  *!*      DO CASE
  *!*        CASE EVALUATE("dAcFnh"+lcTask) > EVALUATE("dEstFnsh"+lcTask)
  *!*          lcTaskStat = "1"
  *!*        CASE EVALUATE("dActFnsh"+lcTask) = EVALUATE("dEstFnsh"+lcTask)
  *!*          lcTaskStat = "2"
  *!*        CASE EVALUATE("dActFnsh"+lcTask) < EVALUATE("dEstFnsh"+lcTask)
  *!*          lcTaskStat = "3"
  *!*      ENDCASE
  *!*    CASE EVALUATE("dActStrt"+lcTask) = EVALUATE("dEstStrt"+lcTask)
  *!*      DO CASE
  *!*        CASE EVALUATE("dActFnsh"+lcTask) > EVALUATE("dEstFnsh"+lcTask)
  *!*          lcTaskStat = "4"
  *!*        CASE EVALUATE("dActFnsh"+lcTask) = EVALUATE("dEstFnsh"+lcTask)
  *!*          lcTaskStat = "5"
  *!*        CASE EVALUATE("dActFnsh"+lcTask) < EVALUATE("dEstFnsh"+lcTask)
  *!*          lcTaskStat = "6"
  *!*      ENDCASE
  *!*    CASE EVALUATE("dActStrt"+lcTask) < EVALUATE("dEstStrt"+lcTask)
  *!*      DO CASE
  *!*        CASE EVALUATE("dActFnsh"+lcTask) > EVALUATE("dEstFnsh"+lcTask)
  *!*          lcTaskStat = "7"
  *!*        CASE EVALUATE("dActFnsh"+lcTask) = EVALUATE("dEstFnsh"+lcTask)
  *!*          lcTaskStat = "8"
  *!*        CASE EVALUATE("dActFnsh"+lcTask) < EVALUATE("dEstFnsh"+lcTask)
  *!*          lcTaskStat = "9"
  *!*      ENDCASE
  *!*  ENDCASE
  *!*  RETURN STUFF(cTaskStat,VAL(lcTask),1,lcTaskStat)
  *!*  *-- End of lfTaskStat

  *!*  *!*************************************************************
  *!*  *! Name      : lfHistory
  *!*  *! Developer : AHMED MAHER (AMH)
  *!*  *! Date      : 9/12/2005
  *!*  *! Purpose   : Valid function of History button
  *!*  *!*************************************************************
  *!*  *! Calls     :
  *!*  *!*************************************************************
  *!*  *! Passed Parameters  :
  *!*  *!*************************************************************
  *!*  *! Returns            : ............
  *!*  *!*************************************************************
  *!*  *! Example   :
  *!*  *!*************************************************************
  *!*  FUNCTION lfHistory
  *!*  LPARAMETERS loFormSet

  *!*  DO CASE
  *!*    CASE EVALUATE(loFormSet.lcTempFile+".cPrj_Typ") = 'C'
  *!*      lcProgName = 'MFCUTKT'
  *!*      lcKey      = EVALUATE(loFormSet.lcTempFile+".cPrj_ID")
  *!*    CASE EVALUATE(loFormSet.lcTempFile+".cPrj_Typ") = 'A'      && Adorment Order
  *!*      lcProgName = 'MFADPO'
  *!*      lcKey      = 'A'+EVALUATE(loFormSet.lcTempFile+".cPrj_ID")
  *!*    CASE EVALUATE(loFormSet.lcTempFile+".cPrj_Typ") = 'D'      && Dye Order
  *!*      lcProgName = 'MFDPO'
  *!*      lcKey      = 'D'+EVALUATE(loFormSet.lcTempFile+".cPrj_ID")
  *!*    CASE EVALUATE(loFormSet.lcTempFile+".cPrj_Typ") = 'P'
  *!*      lcProgName = 'POSTY'
  *!*      lcKey      = 'P'+EVALUATE(loFormSet.lcTempFile+".cPrj_ID")
  *!*    CASE EVALUATE(loFormSet.lcTempFile+".cPrj_Typ") = 'N'      && Inter-Location PO
  *!*      lcProgName = 'POINTRC'
  *!*      lcKey      = 'N'+EVALUATE(loFormSet.lcTempFile+".cPrj_ID")
  *!*    CASE EVALUATE(loFormSet.lcTempFile+".cPrj_Typ") = 'R'      && Return PO
  *!*      lcProgName = 'RETPO'
  *!*      lcKey      = 'R'+EVALUATE(loFormSet.lcTempFile+".cPrj_ID")
  *!*    CASE EVALUATE(loFormSet.lcTempFile+".cPrj_Typ") = 'O'
  *!*      lcProgName = 'SOORD'
  *!*      lcKey      = 'O'+EVALUATE(loFormSet.lcTempFile+".cPrj_ID")
  *!*    CASE EVALUATE(loFormSet.lcTempFile+".cPrj_Typ") = 'T'      && EDI Order
  *!*      lcProgName = 'SOEDORD'
  *!*      lcKey      = 'T'+EVALUATE(loFormSet.lcTempFile+".cPrj_ID")
  *!*    CASE EVALUATE(loFormSet.lcTempFile+".cPrj_Typ") = 'S'
  *!*      lcProgName = 'ICSTYLE'
  *!*      lcKey      = EVALUATE(loFormSet.lcTempFile+".cStyle")
  *!*    CASE EVALUATE(loFormSet.lcTempFile+".cPrj_Typ") = 'H'      && Other
  *!*      lcProgName = 'MFPROJ'
  *!*      lcKey      = 'H'+EVALUATE(loFormSet.lcTempFile+".cPrj_ID")
  *!*    *B608373,1 WAM 12/05/2007 Fix errors in the project monitor screen when click the Task History when no records selected
  *!*    OTHERWISE
  *!*      RETURN
  *!*    *B608373,1 WAM 12/05/2007 (End)
  *!*  ENDCASE

  *!*  PRIVATE lnSelect   , lcBrowKey , lcKeyExp , lcKey , lnStarPos , lnLastPos ,;
  *!*          lcOldBrFld

  *!*  *-- Save the old alias
  *!*  lnSelect   = SELECT(0)

  *!*  *-- Get the key that will be used when browsing the Audit Trail file
  *!*  lcKey = PADR(lcKey,20)
  *!*  lcBrowKey = PADR(lcProgName , 10) + lcKey

  *!*  *-- Open the Audit Trail file if it was not opened
  *!*  IF !USED('AUDTRAIL')
  *!*    =gfOpenTable(oAriaApplication.DataDir + 'AUDTRAIL' , 'AUDTRAIL' , 'SH')
  *!*  ENDIF    && End of IF !USED('AUDTRAIL')

  *!*  *-- Open the System Events file if it was not opened
  *!*  lnRemResult = oAriaApplication.remotesystemdata.execute("Select * from SYCEVENT WHERE capobjnam='"+PADR(lcProgName , 10)+;
  *!*                  "' ORDER BY capobjnam,cevent_id",'',"SYCEVENT","",oAriaApplication.SystemConnectionString,3,;
  *!*                  "",loFormSet.DataSessionId)
  *!*  llFound = .f.
  *!*  IF lnRemResult=1
  *!*    LOCATE
  *!*    llFound = FOUND()
  *!*  ENDIF

  *!*  SELECT AUDTRAIL
  *!*  *-- Prepair the browse fields
  *!*  lcBrFields  = "F=IIF(!EMPTY(LOOKUP(SYCEVENT.cDiscrep,cevent_id,SYCEVENT.cevent_id)),LOOKUP(SYCEVENT.cDiscrep,cevent_id,SYCEVENT.cevent_id),CEVENT_ID) :H='Event' ,"+;
  *!*                "cAdd_User :H='User ID' ," +;
  *!*                "cAdd_Time :H='Time' ," +;
  *!*                "dAdd_Date :H='Date' ," +;
  *!*                "NeededInf = mNeededInf: 65 :H='Needed Information'"

  *!*  PRIVATE lcDesc, mNeed , lcAdd_User , lcAdd_Time , ldAdd_Date
  *!*  STORE '' TO lcDesc, mNeed , lcAdd_User , lcAdd_Time , ldAdd_Date
  *!*  *-- Get temprory file name to be used in selecting data
  *!*  lcTmpAudt = gfTempName()

  *!*  *-- select data from AUDIT TRAIL file for the specified Key
  *!*  LOCAL lcSQL
  *!*  lcSQL = "SELECT cAdd_User,cAdd_Time,dAdd_Date,cEvntObjID,cEvent_ID,cAudtralid,capobjnam,key,mNeededInf " + ;
  *!*          "FROM AUDTRAIL " + ;
  *!*          "WHERE Capobjnam + Key + Caudtralid = '" + lcBrowKey + "'"

  *!*  LOCAL lnResult
  *!*  lnResult = oAriaApplication.RemoteSystemData.SQLRun( ;
  *!*             lcSQL, ;
  *!*             lcTmpAudt, ;
  *!*             'AUDTRAIL', ;
  *!*             oAriaApplication.cAriaNativeDataFilesConStr, ;
  *!*             3, ;
  *!*             'SAVE', ;
  *!*             loFormSet.DataSessionId)

  *!*  IF lnResult # 1
  *!*    SELECT (lnSelect)
  *!*    oAriaApplication.RemoteSystemData.CheckRetResult('SQLRun', lnResult, .T.)
  *!*    RETURN
  *!*  ENDIF
  *!*  CURSORSETPROP('Buffering', 3)
  *!*  INDEX ON capobjnam + key + caudtralid TAG (lcTmpAudt)
  *!*  CURSORSETPROP('Buffering', 5)

  *!*  *-- if there is no data foud then display notification message
  *!*  SELECT (lcTmpAudt)

  *!*  LOCATE
  *!*  IF RECCOUNT() = 0
  *!*    SELECT (lnSelect)
  *!*    MESSAGEBOX("No records to display", 64, _screen.Caption)
  *!*    RETURN
  *!*  ENDIF

  *!*  *-- Browse the data
  *!*  LOCAL llResult
  *!*  llResult =ARIABROW("" , 'Audit Trails' ,.F.,.F.,.F.,.F., '' ,;
  *!*                     'Fi\<nd;Or\<der by;\<Descending;Fi\<lter;;\!\?\<Ok')

  *!*  IF llResult
  *!*    LOCAL lcTranCode
  *!*    lcTranCode = oAriaApplication.RemoteSystemData.BeginTran(oAriaApplication.cAriaNativeDataFilesConStr, 3, '', .T.)
  *!*    IF TYPE('lcTranCode') # 'C'
  *!*      SELECT (lnSelect)
  *!*      oAriaApplication.RemoteSystemData.CheckRetResult('BeginTran', lcTranCode, .T.)
  *!*      RETURN
  *!*    ENDIF

  *!*    lnConnHandler = oAriaApplication.RemoteSystemData.SQLUpdate(lcTmpAudt, lcTranCode, loFormSet.DataSessionId, 'Capobjnam+Key+Caudtralid')
  *!*
  *!*    IF lnConnHandler # 1
  *!*      SELECT (lnSelect)
  *!*      oAriaApplication.RemoteSystemData.CheckRetResult('SQLUpdate', lcTranCode, .T.)
  *!*      RETURN .F.
  *!*    ENDIF
  *!*
  *!*    lnConnHandler = oAriaApplication.RemoteSystemData.CommitTran(lcTranCode, .T.)
  *!*    IF lnConnHandler # 1
  *!*      SELECT (lnSelect)
  *!*      =oAriaApplication.RemoteSystemData.RollBackTran(lcTranCode, .T.)
  *!*      RETURN .F.
  *!*    ENDIF
  *!*  ENDIF

  *!*  *-- Restore old alias
  *!*  SELECT (lnSelect)

  *!*  *!*************************************************************
  *!*  *! Name      : lfSchedule
  *!*  *! Developer : AHMED MAHER (AMH)
  *!*  *! Date      : 9/12/2005
  *!*  *! Purpose   : Valid function for push button < Schedule... >
  *!*  *!*************************************************************
  *!*  *! Calls     :
  *!*  *!*************************************************************
  *!*  *! Passed Parameters  :
  *!*  *!*************************************************************
  *!*  *! Returns            : ............
  *!*  *!*************************************************************
  *!*  *! Example   :
  *!*  *!*************************************************************
  *!*  FUNCTION lfSchedule
  *!*  PARAMETERS lcProjType,lcProjID,lcPStyle,ldSchDate,lcSDir,lcTempName,llMSchdul

  *!*  llFromWeb = .F.
  *!*  IF TYPE('lcProjType') = 'C' AND TYPE('lcProjID') = 'C' AND TYPE('lcPStyle') = 'C' AND;
  *!*     TYPE('ldSchDate')  = 'D' AND TYPE('lcSDir')  = 'C'
  *!*    llFromWeb = .T.
  *!*    lcPrj_Typ = lcProjType
  *!*    lcPrj_ID  = lcProjID
  *!*    lcStyle   = lcPStyle
  *!*    dSch_Date = ldSchDate
  *!*  ENDIF

  *!*  IF llFromWeb
  *!*    IF !USED('SYSCHDUL')
  *!*      USE (lcSDir+"SYSCHDUL.DBF") SHARED IN 0
  *!*      SELECT SYSCHDUL
  *!*      SET ORDER TO COPRUSR
  *!*    ENDIF
  *!*    IF !USED('PMCALDT')
  *!*      gfOpenTable(oAriaApplication.DataDir+'PMCALDT','PMCALDT',"SH")
  *!*    ENDIF
  *!*    IF !USED('PMPRJRL')
  *!*      gfOpenTable(oAriaApplication.DataDir+'PMPRJRL','PMPRJRL',"SH")
  *!*    ENDIF
  *!*    IF !USED('PMCALHD')
  *!*      gfOpenTable(oAriaApplication.DataDir+'PMCALHD','PMCALHD',"SH")
  *!*    ENDIF
  *!*
  *!*    =SEEK(lcProjType+lcProjID+lcPStyle,'PMPRJHD')
  *!*
  *!*    DECLARE laHolidays[1]
  *!*    laHolidays = ''
  *!*    lnRows  = 0
  *!*
  *!*    SELECT PMCALDT
  *!*    SCAN
  *!*      ldCal_HFrm = dCal_HFrm
  *!*      DO WHILE ldCal_HFrm <= dCal_HTo
  *!*        lnRows  = lnRows  + 1
  *!*        DIMENSION laHolidays[lnRows]
  *!*        laHolidays[lnRows] = cCal_ID + DTOC(ldCal_HFrm)
  *!*        ldCal_HFrm = ldCal_HFrm + 1
  *!*      ENDDO
  *!*    ENDSCAN

  *!*  ENDIF

  *!*  *-- Check if the current record is being edited by another user.
  *!*  IF PMPRJHD.llok_stat AND PMPRJHD.cLok_User <> oAriaApplication.User_ID
  *!*    =gfModalgen("TRM00029B38030","DIALOG")
  *!*  ELSE
  *!*    SELECT PMPRJHD
  *!*    *-- Proceed with scheduling if locking is successful.
  *!*    IF RLOCK()
  *!*      REPLACE llok_stat WITH .T.
  *!*      UNLOCK
  *!*      *-- Update data date.
  *!*      STORE dSch_Date TO m.dDta_Date
  *!*      STORE .F. TO m.lSchedual, m.llok_stat
  *!*      m.cPrj_Stts = 'I'
  *!*      STORE 'In Progress' TO lcCurStage
  *!*
  *!*      *-- Add audit information ?
  *!*      =lfGetClcDt()
  *!*      IF !llMSchdul
  *!*        SELECT PMPRJHD
  *!*        GATHER MEMVAR MEMO
  *!*      ENDIF
  *!*    ENDIF
  *!*  ENDIF

  *!*  *!*************************************************************
  *!*  *! Name      : lfGetClcDt
  *!*  *! Developer : AHMED MAHER (AMH)
  *!*  *! Date      : 9/12/2005
  *!*  *! Purpose   : Calculates calculated dates for all operations
  *!*  *!             as well as the calculated finish date for the
  *!*  *!             project based on the calculated start date.
  *!*  *!*************************************************************
  *!*  *! Calls     :
  *!*  *!*************************************************************
  *!*  *! Passed Parameters  :
  *!*  *!*************************************************************
  *!*  *! Returns            : ............
  *!*  *!*************************************************************
  *!*  *! Example   :
  *!*  *!*************************************************************
  *!*  FUNCTION lfGetClcDt

  *!*  PRIVATE lnCurAlias

  *!*  lnCurAlias = SELECT(0)

  *!*  lcTmpPrjDt = IIF(llMSchdul,lc_PmPrjDt,'PMPRJDT')
  *!*  lcTmpPrjRl = IIF(llMSchdul,lc_PmPrjRl,'PMPRJRL')

  *!*  *-- Change the tag of Project relations file to predecessors,
  *!*  SET ORDER TO TAG PMPRJRLP IN (lcTmpPrjRl)
  *!*  lnCurTag = VAL(SYS(21))
  *!*  *-- Reset order tag

  *!*  SELECT (lcTmpPrjDt)
  *!*  SET ORDER TO TAG PMPRJDT

  *!*  IF llFromWeb
  *!*    lc_Parser = lcTempName
  *!*    CREATE CURSOR (lc_Parser);
  *!*           (cOprt_Ctg C(3), cOprt_ID C(5), dStrtDate D(8), nDurIndic N(1))
  *!*  ENDIF

  *!*  SELECT (lc_Parser)
  *!*  *-- Add category code field
  *!*  SET RELATION TO SUBSTR(lcPrj_Typ,1,LEN(&lcTmpPrjDt..cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(&lcTmpPrjDt..cPrj_ID)) +;
  *!*                  SUBSTR(lcStyle,1,LEN(&lcTmpPrjDt..cStyle))+ cOprt_Ctg + cOprt_ID INTO (lcTmpPrjDt)

  *!*
  *!*  *-- Scan all operations of the current project
  *!*  SELECT (lcTmpPrjDt)

  *!*  SET RELATION OFF INTO PMPRJRL
  *!*  *-- Add category code field
  *!*  SET FILTER TO  cPrj_Typ +  cPrj_ID +  cStyle + cOprt_Ctg + cOprt_ID = ;
  *!*                SUBSTR(lcPrj_Typ,1,LEN(&lcTmpPrjRl..cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(&lcTmpPrjRl..cPrj_ID)) +;
  *!*                SUBSTR(lcStyle,1,LEN(&lcTmpPrjRl..cStyle));
  *!*                .AND. !lVoid


  *!*  *--Clear all calculated dates fields
  *!*  REPLACE ALL dclc_strt WITH {},;
  *!*              dclc_fnsh WITH {}

  *!*  SCAN FOR EMPTY(dclc_strt)
  *!*    *-- Save the current operation into a variable.
  *!*    lcCurOprt = cOprt_Ctg + cOprt_ID

  *!*
  *!*    *-- Update dates, and get the new finish date.
  *!*    SELECT (lcTmpPrjDt)

  *!*    *-- For the first operation, use the scheduling date as a
  *!*    *-- candidate calculated start date
  *!*    =lfUpdClcDt(dSch_Date)
  *!*    lldclc_fsh = dclc_fnsh
  *!*    lnDurIndic = MIN(nest_dur,1)
  *!*
  *!*    *-- if the new operation is a predecessor to other operations,
  *!*    *-- copy their IDs to the cursor.
  *!*    SELECT (lcTmpPrjRl)
  *!*
  *!*    IF SEEK(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
  *!*            SUBSTR(lcStyle,1,LEN(cStyle)) + lcCurOprt)
  *!*      SCAN REST WHILE cPrj_Typ +  cPrj_ID + cStyle  +  cPrd_Ctg + cPrd_ID = ;
  *!*                     SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
  *!*                     SUBSTR(lcStyle,1,LEN(cStyle)) + lcCurOprt
  *!*        lcNxtOprt = cOprt_Ctg + cOprt_ID
  *!*        INSERT INTO (lc_Parser);
  *!*           VALUES(&lcTmpPrjRl..cOprt_Ctg, &lcTmpPrjRl..cOprt_ID, lldclc_fsh, lnDurIndic)
  *!*      ENDSCAN
  *!*    ENDIF

  *!*    SELECT (lc_Parser)
  *!*    GO TOP
  *!*    IF EOF()
  *!*      SELECT (lcTmpPrjDt)
  *!*      SET ORDER TO PMPRJDT IN (lcTmpPrjDt)
  *!*      =SEEK(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
  *!*          SUBSTR(lcStyle,1,LEN(cStyle)) + lcCurOprt)
  *!*      IF !EMPTY(&lcTmpPrjDt..cOprt_res) OR !EMPTY(&lcTmpPrjDt..cGroup_Id)
  *!*        SELECT SYSCHDUL

  *!*        LOCAL lcLastOrder
  *!*        lcLastOrder = SET("Order")
  *!*        SET ORDER TO Coprusr
  *!*
  *!*        IF SEEK(SUBSTR(lcPrj_Typ,1,1) + SUBSTR(lcPrj_ID,1,6) + lcCurOprt)
  *!*          IF SYSCHDUL.COPERSTAT = 'C'
  *!*            lcStauts = SYSCHDUL.COPERSTAT
  *!*            LOCATE REST WHILE cconttype+cseqnumber+ccont_id+coperstat+cuser_id =;
  *!*                        SUBSTR(lcPrj_Typ,1,1) + SUBSTR(lcPrj_ID,1,6) + lcCurOprt;
  *!*                        FOR COPERSTAT <> lcStauts
  *!*
  *!*          ENDIF
  *!*          REPLACE dtrandate  WITH IIF(EMPTY(&lcTmpPrjDt..dclc_strt),&lcTmpPrjDt..dEst_strt,&lcTmpPrjDt..dclc_strt),;
  *!*                  dcmpltdate WITH IIF(EMPTY(&lcTmpPrjDt..dclc_Fnsh),&lcTmpPrjDt..dEst_fnsh,&lcTmpPrjDt..dclc_Fnsh),;
  *!*                  lpredcomp  WITH .T.

  *!*           lcCond = "CCONTTYPE='" + CCONTTYPE + "' AND CSEQNUMBER='"+CSEQNUMBER+"' AND CCONT_ID='"+CCONT_ID+"' AND COPERSTAT<>'C'"
  *!*           =lfupdateschedule("dtrandate,dcmpltdate,lpredcomp",lcCond)
  *!*        ENDIF
  *!*
  *!*        SELECT SYSCHDUL
  *!*        SET ORDER TO &lcLastOrder.
  *!*
  *!*      ENDIF
  *!*    ENDIF
  *!*    SELECT (lc_Parser)
  *!*    DO WHILE !EOF()
  *!*      *-- Update the estimated date of the current detail record
  *!*      *-- Add category code field
  *!*      lcNxtOprt  = cOprt_Ctg + cOprt_ID
  *!*      lnRecNo    = RECNO()
  *!*
  *!*      SELECT (lcTmpPrjDt)

  *!*      =lfUpdClcDt(IIF(nest_dur = 0, &lc_Parser..dStrtDate,;
  *!*                     &lc_Parser..dStrtDate + &lc_Parser..nDurIndic))
  *!*      lldclc_fsh = dclc_fnsh
  *!*
  *!*      lnDurIndic = MAX(lnDurIndic, MIN(nest_dur,1))
  *!*      SELECT (lcTmpPrjRl)

  *!*      IF SEEK(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
  *!*              SUBSTR(lcStyle,1,LEN(cStyle)) + lcNxtOprt)
  *!*        SCAN REST WHILE cPrj_Typ + cPrj_ID + cStyle + cPrd_Ctg + cPrd_ID = ;
  *!*                       SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
  *!*                       SUBSTR(lcStyle,1,LEN(cStyle)) + lcNxtOprt
  *!*          lcNxtOnPth = cOprt_Ctg + cOprt_ID
  *!*          INSERT INTO (lc_Parser);
  *!*             VALUES(&lcTmpPrjRl..cOprt_Ctg, &lcTmpPrjRl..cOprt_ID, lldclc_fsh, lnDurIndic)
  *!*        ENDSCAN
  *!*      ENDIF
  *!*
  *!*      IF !EMPTY(&lcTmpPrjDt..cOprt_res) OR !EMPTY(&lcTmpPrjDt..cGroup_Id)
  *!*        SELECT SYSCHDUL

  *!*        LOCAL lcLastOrder
  *!*        lcLastOrder = SET("Order")
  *!*        SET ORDER TO Coprusr

  *!*        SET ORDER TO Coprusr
  *!*        IF SEEK(SUBSTR(lcPrj_Typ,1,1) + SUBSTR(lcPrj_ID,1,6) + lcNxtOprt)
  *!*          IF SYSCHDUL.COPERSTAT = 'C'
  *!*            lcStauts = SYSCHDUL.COPERSTAT
  *!*            LOCATE REST WHILE cconttype+cseqnumber+ccont_id+coperstat+cuser_id =;
  *!*                        SUBSTR(lcPrj_Typ,1,1) + SUBSTR(lcPrj_ID,1,6) + lcNxtOprt ;
  *!*                        FOR COPERSTAT <> lcStauts
  *!*
  *!*          ENDIF
  *!*          REPLACE dtrandate  WITH IIF(EMPTY(&lcTmpPrjDt..dclc_strt),&lcTmpPrjDt..dEst_strt,&lcTmpPrjDt..dclc_strt),;
  *!*                  dcmpltdate WITH IIF(EMPTY(&lcTmpPrjDt..dclc_Fnsh),&lcTmpPrjDt..dEst_fnsh,&lcTmpPrjDt..dclc_Fnsh)

  *!*         lcCond = "CCONTTYPE='" + CCONTTYPE + "' AND CSEQNUMBER='"+CSEQNUMBER+"' AND CCONT_ID='"+CCONT_ID+"' AND COPERSTAT<>'C'"
  *!*         =lfupdateschedule("dtrandate,dcmpltdate",lcCond)
  *!*        ENDIF

  *!*        SELECT SYSCHDUL
  *!*        SET ORDER TO &lcLastOrder.
  *!*      ENDIF
  *!*
  *!*
  *!*      SELECT (lc_Parser)
  *!*      IF BETWEEN(lnRecNo, 1, RECCOUNT())
  *!*        GO lnRecNo
  *!*      ENDIF

  *!*      SKIP
  *!*    ENDDO
  *!*
  *!*    SELECT (lc_Parser)
  *!*    ZAP
  *!*
  *!*    SELECT (lcTmpPrjDt)
  *!*    SET ORDER TO PMPRJDT IN (lcTmpPrjDt)
  *!*    =SEEK(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
  *!*         SUBSTR(lcStyle,1,LEN(cStyle)) + lcCurOprt)
  *!*  ENDSCAN

  *!*  *-- After all operations are scanned, get the project estimated finish date.
  *!*  *-- The project estimated finish date is the greatest estimated finish
  *!*  *-- date of all operations/
  *!*  DECLARE laTmpVal[1]
  *!*  SELECT MIN(dclc_strt), MAX(dclc_fnsh) ;
  *!*    FROM (lcTmpPrjDt)   ;
  *!*    WHERE cPrj_Typ +  cPrj_ID +  cStyle + cOprt_Ctg + cOprt_ID = ;
  *!*                SUBSTR(lcPrj_Typ,1,LEN(&lcTmpPrjDt..cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(&lcTmpPrjDt..cPrj_ID)) +;
  *!*                SUBSTR(lcStyle,1,LEN(&lcTmpPrjDt..cStyle)) ;
  *!*         .AND. !lVoid;
  *!*    INTO ARRAY laTmpVal
  *!*  IF !EMPTY(laTmpVal[1])
  *!*    m.dclc_strt = laTmpVal[1]
  *!*    m.dclc_fnsh = laTmpVal[2]
  *!*    IF llFromWeb
  *!*      SELECT PMPRJHD
  *!*      REPLACE dclc_strt WITH m.dclc_strt,;
  *!*              dclc_fnsh WITH m.dclc_fnsh
  *!*
  *!*      SELECT (lcTmpPrjDt)
  *!*    ENDIF
  *!*  ENDIF

  *!*  *--  Restore relations and tags
  *!*  SET ORDER TO TAG PMPRJRL IN (lcTmpPrjRl)
  *!*  SELECT (lcTmpPrjDt)
  *!*  SET ORDER TO (lnCurTag)

  *!*  SET FILTER TO
  *!*  SET RELATION TO cPrj_Typ + cPrj_ID + cStyle + cOprt_Ctg + cOprt_ID ;
  *!*             INTO (lcTmpPrjRl)
  *!*  SELECT (lnCurAlias)

  *!*  *!*************************************************************
  *!*  *! Name      : lfUpdClcDt
  *!*  *! Developer : AHMED MAHER (AMH)
  *!*  *! Date      : 9/12/2005
  *!*  *! Purpose   : Updates the current operation's calculated start
  *!*  *!             and finish date
  *!*  *!*************************************************************
  *!*  *! Calls     :
  *!*  *!*************************************************************
  *!*  *! Passed Parameters  :
  *!*  *!*************************************************************
  *!*  *! Returns            : ............
  *!*  *!*************************************************************
  *!*  *! Example   :
  *!*  *!*************************************************************
  *!*  FUNCTION lfUpdClcDt
  *!*  PARAMETERS ldNxtStDte

  *!*  *--  Get the hidger date value as a start date
  *!*  ldNxtStDte = MAX(ldNxtStDte, dclc_strt)

  *!*  DO CASE
  *!*    *--  Case Actual values are entered
  *!*    CASE !EMPTY(dact_strt) .OR. !EMPTY(dact_fnsh) .OR. nAct_dur > 0
  *!*      *--  If an actual start date exists, default the calculated start
  *!*      *--  date with its value,
  *!*      *--  If an actual finish date exists, default the calculated finish
  *!*      *--  date with its value,
  *!*      *--  else,
  *!*      *--  If an actual duration exists, add it to the calculated start
  *!*      *--  date, else add the estimated duration to the calculated start
  *!*      *--  date.
  *!*      IF !EMPTY(dact_strt)
  *!*        ldClcFshDt = {}
  *!*        REPLACE dclc_strt WITH lfAdjDate(dact_strt, @ldClcFshDt,;
  *!*                                             MAX(0, IIF(nAct_dur > 0, nAct_dur, nest_dur) - 1),;
  *!*                                             cCal_ID, '+'),;
  *!*                dclc_fnsh WITH IIF(!EMPTY(dact_fnsh), dact_fnsh, ldClcFshDt)

  *!*      *--  Else,
  *!*      *--  If an actual start date is not found,
  *!*      ELSE  && ELSEIF !EMPTY(dact_strt)
  *!*        *--  If an actual finish date exists, default the calculated finish
  *!*        *--  date with its value,
  *!*        *--  If an actual finish date exists, default the calculated finish
  *!*        *--  date with its value,
  *!*        *--  Calculate the calculated start date as follows :
  *!*        *--  If an actual duration exists, subtract it from the calculated
  *!*        *--  finish date, else subtract the estimated duration from the
  *!*        *--  calculated finish date.
  *!*        IF !EMPTY(dact_fnsh)
  *!*          ldClcStDt = {}
  *!*          =lfAdjDate(dact_fnsh, @ldClcStDt,;
  *!*                         MAX(0, IIF(nAct_dur > 0, nAct_dur, nest_dur) - 1),;
  *!*                         cCal_ID, '-')
  *!*          REPLACE dclc_strt WITH ldClcStDt,;
  *!*                  dclc_fnsh WITH dact_fnsh
  *!*
  *!*        *--  Else, both actual start and finish dates are emoty,
  *!*        *--  i.e. the actual duration is not empty
  *!*        *--  Calculate the start and finish calculated dates
  *!*        *--  based on the finish date of the previous operation,
  *!*        *--  using actual duration
  *!*        ELSE  && ELSEIF !EMPTY(dact_fnsh)
  *!*          ldClcFshDt = {}
  *!*          REPLACE dclc_strt WITH lfAdjDate(ldNxtStDte, @ldClcFshDt          ,;
  *!*                                 MAX(0, nAct_dur - 1), cCal_ID, '+'),;
  *!*                  dclc_fnsh WITH ldClcFshDt
  *!*        ENDIF && ENDIF !EMPTY(dact_fnsh)
  *!*      ENDIF && ENDIF !EMPTY(dact_strt)
  *!*    *--  Otherwise, No Actual values are entered
  *!*    OTHERWISE
  *!*
  *!*      *--  If the remaining duration is different from the estimated
  *!*      *--  duration, this indicates that the current operation is in work.
  *!*      IF nrem_dur <> nest_dur
  *!*        *--  Get the calculated start date referenced to the
  *!*        *--  scheduling date (dSch_Date)
  *!*          ldClcStDt = {}
  *!*          IF nest_dur = 0
  *!*            STORE ldNxtStDte + 1 TO ldClcStDt, ldClcFshDt
  *!*          ELSE
  *!*            STORE ldNxtStDte TO ldClcStDt, ldClcFshDt
  *!*          ENDIF
  *!*          =lfAdjDate(@ldClcStDt,@ldClcFshDt,nrem_dur, cCal_ID, '+')

  *!*        REPLACE dclc_strt WITH ldClcStDt,;
  *!*                dclc_fnsh WITH ldClcFshDt

  *!*      *--  Otherwise, calculate dates based on the finish date of
  *!*      *--  the previous operation and the remaining duration
  *!*      ELSE
  *!*        ldClcFshDt = {}
  *!*        REPLACE dclc_strt WITH lfAdjDate(ldNxtStDte, @ldClcFshDt,;
  *!*                                         MAX(0, nrem_dur - 1), cCal_ID, '+'),;
  *!*                dclc_fnsh WITH ldClcFshDt
  *!*      ENDIF
  *!*  ENDCASE

  *!*  *!*************************************************************
  *!*  *! Name      : lfupdateschedule
  *!*  *! Developer : AHMED MAHER (AMH)
  *!*  *! Date      : 9/12/2005
  *!*  *! Purpose   : Updates syschdul file.
  *!*  *!*************************************************************
  *!*  *! Calls     :
  *!*  *!*************************************************************
  *!*  *! Passed Parameters  :
  *!*  *!*************************************************************
  *!*  *! Returns            : ............
  *!*  *!*************************************************************
  *!*  *! Example   :
  *!*  *!*************************************************************
  *!*  FUNCTION lfupdateschedule
  *!*  LPARAMETERS tcFieldsToUpd,tcWhereCond

  *!*  LOCAL lnCount
  *!*  DIME laFields[1,1]
  *!*  laFields= ''
  *!*  =gfSubStr(tcFieldsToUpd,@laFields,",")
  *!*  SELECT syschdul
  *!*  SCATTER MEMVAR MEMO
  *!*  lcUpdCmnd = "UPDATE syschdul SET "
  *!*  FOR lnCount = 1 TO ALEN(laFields,1)
  *!*    lcStrtSep = SUBSTR("''  {",ATC(TYPE("m."+laFields[lnCount,1]),"CMNLD"),1)
  *!*    lcEndSep = SUBSTR("''  }",ATC(TYPE("m."+laFields[lnCount,1]),"CMNLD"),1)
  *!*    lcStrtSep = ALLTRIM(lcStrtSep)
  *!*    lcEndSep  = ALLTRIM(lcEndSep)
  *!*  *  lcUpdCmnd = lcUpdCmnd +" SET "+laFields[lnCount,1]+"="+lcStrtSep +"?m."+laFields[lnCount,1]+lcEndSep + IIF(lnCount<ALEN(laFields,1),",","")
  *!*    lcUpdCmnd = lcUpdCmnd +laFields[lnCount,1]+"="+"?m."+laFields[lnCount,1] + IIF(lnCount<ALEN(laFields,1),",","")
  *!*  ENDFOR
  *!*  lcUpdCmnd = lcUpdCmnd +" WHERE "+tcWhereCond
  *!*  lnUpdResult = oAriaApplication.remotesystemdata.execute(lcUpdCmnd,'',"","",oAriaApplication.SystemConnectionString,3,"",SET("Datasession"))

  *!*  RETURN lnUpdResult=1

  *!*  *!*************************************************************
  *!*  *! Name      : lfAdjDate
  *!*  *! Developer : AHMED MAHER (AMH)
  *!*  *! Date      : 9/12/2005
  *!*  *! Purpose   : Adjusts date addition according to calendar
  *!*  *!             holidays and weekends
  *!*  *!*************************************************************
  *!*  *! Calls     :
  *!*  *!*************************************************************
  *!*  *! Passed Parameters  :
  *!*  *!*************************************************************
  *!*  *! Returns            : ............
  *!*  *!*************************************************************
  *!*  *! Example   :
  *!*  *!*************************************************************
  *!*  FUNCTION lfAdjDate
  *!*  PARAMETERS ldOprt_Str, ldOprt_Fsh, lnOprt_Dur, lcCal_ID, lcOperator
  *!*  PRIVATE lnCount, ldOprt_Fsh

  *!*  IF SEEK(lcCal_ID, 'PMCALHD')
  *!*    DO WHILE STR(DOW(ldOprt_Str),1) $ PMCALHD.cCal_WEnd .OR. ;
  *!*                 ASCAN(laHolidays,lcCal_ID + DTOC(ldOprt_Str)) > 0
  *!*      ldOprt_Str = ldOprt_Str &lcOperator 1
  *!*    ENDDO
  *!*
  *!*    lnCount = 1
  *!*    ldOprt_Fsh = ldOprt_Str
  *!*    DO WHILE lnCount <= lnOprt_Dur
  *!*       ldOprt_Fsh = ldOprt_Fsh &lcOperator 1
  *!*       lnCount    = lnCount ;
  *!*                   + IIF(STR(DOW(ldOprt_Fsh),1) $ PMCALHD.cCal_WEnd .OR. ;
  *!*                   ASCAN(laHolidays,lcCal_ID + DTOC(ldOprt_Fsh)) > 0 , 0 , 1)
  *!*    ENDDO
  *!*  ELSE
  *!*    ldOprt_Fsh = ldOprt_Str &lcOperator lnOprt_Dur
  *!*  ENDIF
  *!*  RETURN ldOprt_Str

  *!*  *!*************************************************************
  *!*  *! Name      : lfvSchedul
  *!*  *! Developer : AHMED MAHER (AMH)
  *!*  *! Date      : 09/13/2005
  *!*  *! Purpose   : Valid function for push button < Schedule... >
  *!*  *!*************************************************************
  *!*  *! Calls              : MFSCHD.SPR
  *!*  *!*************************************************************
  *!*  *! Passed Parameters  : None
  *!*  *!*************************************************************
  *!*  *! Returns            :  None
  *!*  *!*************************************************************
  *!*  *! Example            :  lfvSchedul()
  *!*  *!*************************************************************
  *!*  FUNCTION lfvSchedul
  *!*  LPARAMETERS loFormSet

  *!*  *B608373,1 WAM 12/05/2007 Fix errors in the project monitor screen when click the Schedule button when no records selected
  *!*  IF EMPTY( EVALUATE(loFormSet.lcTempFile+".cPrj_Typ"))
  *!*    RETURN
  *!*  ENDIF
  *!*  *B608373,1 WAM 12/05/2007 (End)

  *!*  *-- Default schedule date with todays date
  *!*  dSch_Date = IIF(EMPTY(EVALUATE(loFormSet.lcTempFile+".dClc_strt")),EVALUATE(loFormSet.lcTempFile+".dEst_strt"),oAriaApplication.systemdate)

  *!*  llSchedule = .F.
  *!*  DO FORM (oAriaApplication.ScreenHome+"\MFSCHD.SCX")

  *!*  IF llSchedule
  *!*    =lfSchedule(EVALUATE(loFormSet.lcTempFile+".cPrj_Typ"),EVALUATE(loFormSet.lcTempFile+".cPrj_ID"),;
  *!*                EVALUATE(loFormSet.lcTempFile+".cStyle"),dSch_Date,oAriaApplication.SysPath,gfTempName())
  *!*    STORE "" TO lcProg , lcKey , lcApObjNam ,lcEvent
  *!*    =lfUpdVar(EVALUATE(loFormSet.lcTempFile+".cPrj_Typ"),EVALUATE(loFormSet.lcTempFile+".cPrj_ID"))
  *!*    IF !USED('AUDTRAIL')
  *!*      =gfOpenTable(oAriaApplication.DataDir+'AUDTRAIL','AUDTRAIL','SH')
  *!*    ENDIF
  *!*    SELECT PMPRJDT
  *!*    lcTmpDtFl = 'PMPRJDT'
  *!*    SET ORDER TO PMPRJDT IN PMPRJDT
  *!*    =SEEK(EVALUATE(loFormSet.lcTempFile+".cPrj_Typ")+EVALUATE(loFormSet.lcTempFile+".cPrj_ID")+EVALUATE(loFormSet.lcTempFile+".cStyle"))
  *!*    SCAN REST WHILE cprj_typ+cprj_id+cstyle+coprt_ctg+coprt_id = EVALUATE(loFormSet.lcTempFile+".cPrj_Typ")+;
  *!*                    EVALUATE(loFormSet.lcTempFile+".cPrj_ID")+EVALUATE(loFormSet.lcTempFile+".cStyle");
  *!*               FOR lSchAdToAd
  *!*      =lfPrpAudt(loFormSet)
  *!*      REPLACE lSchAdToAd WITH .F.
  *!*    ENDSCAN
  *!*  ENDIF

  *!*  *!*************************************************************
  *!*  *! Name      : lfUpdVar
  *!*  *! Developer : AHMED MAHER (AMH)
  *!*  *! Date      : 09/13/2005
  *!*  *! Purpose   :
  *!*  *!*************************************************************
  *!*  *! Calls     : None
  *!*  *!*************************************************************
  *!*  *! Passed Parameters  :  None
  *!*  *!*************************************************************
  *!*  *! Returns            :  None
  *!*  *!*************************************************************
  *!*  *! Example            :  =lfUpdVar()
  *!*  *!*************************************************************
  *!*  FUNCTION lfUpdVar
  *!*  LPARAMETERS lcConttype, lcseqnumbr

  *!*  lcApObjNam = 'MFPROJ'

  *!*  DO CASE
  *!*    CASE lcConttype = 'C'     && CT
  *!*      lcEvent = 'RESCHDCT'
  *!*      lcProg  = 'MFCUTKT'
  *!*      lcKey   = lcseqnumbr
  *!*    CASE lcConttype = 'P'      && PO
  *!*      lcEvent = 'RESCHDPO'
  *!*      lcProg  = 'POSTY'
  *!*      lcKey   = lcconttype+lcseqnumbr
  *!*    CASE lcConttype = 'A'      && Adorment PO Order
  *!*      lcEvent = 'RESCHDPA'
  *!*      lcProg  = 'MFADPO'
  *!*      lcKey   = lcconttype+lcseqnumbr
  *!*    CASE lcConttype = 'D'      && Dye PO Order
  *!*      lcEvent = 'RESCHDPD'
  *!*      lcProg  = 'MFDPO'
  *!*      lcKey   = lcconttype+lcseqnumbr
  *!*    CASE lcConttype = 'N'      && Inter-Location PO
  *!*      lcEvent = 'RESCHDPN'
  *!*      lcProg  = 'POINTRC'
  *!*      lcKey   = lcconttype+lcseqnumbr
  *!*    CASE lcConttype = 'R'      && Return PO
  *!*      lcEvent = 'RESCHDPR'
  *!*      lcProg  = 'RETPO'
  *!*      lcKey   = lcconttype+lcseqnumbr
  *!*    CASE lcConttype = 'O'      && SO
  *!*      lcEvent = 'RESCHDSO'
  *!*      lcProg  = 'SOORD'
  *!*      lcKey   = lcconttype+lcseqnumbr
  *!*    CASE lcConttype = 'T'      && EDI Sales Order
  *!*      lcEvent = 'RESCHDST'
  *!*      lcProg  = 'SOEDORD'
  *!*      lcKey   = lcconttype+lcseqnumbr
  *!*    CASE lcPrj_typ = 'S'       && Style
  *!*      lcProg  = 'ICSTYLE'
  *!*      lcEvent = 'RESCHDSS'
  *!*      lcKey   = lcseqnumbr
  *!*    CASE lcPrj_typ = 'H'       && Other
  *!*      lcProg  = 'MFPROJ'
  *!*      lcEvent = 'RESCHDSH'
  *!*      lcKey   = lcconttype+lcseqnumbr
  *!*  ENDCASE

  *!*  *!*************************************************************
  *!*  *! Name      : lfPrpAudt
  *!*  *! Developer : AHMED MAHER (AMH)
  *!*  *! Date      : 09/13/2005
  *!*  *! Purpose   :
  *!*  *!*************************************************************
  *!*  *! Calls     : None
  *!*  *!*************************************************************
  *!*  *! Passed Parameters  :  None
  *!*  *!*************************************************************
  *!*  *! Returns            :  None
  *!*  *!*************************************************************
  *!*  *! Example            :  =lfPrpAudt()
  *!*  *!*************************************************************
  *!*  FUNCTION lfPrpAudt
  *!*  LPARAMETERS loFormSet

  *!*  IF SEEK(EVALUATE(lcTmpDtFl+".cPrj_Typ+"+lcTmpDtFl+".cPrj_ID+"+lcTmpDtFl+".cOprt_Ctg+"+lcTmpDtFl+".cOprt_ID"),'SYSCHDUL')
  *!*    IF !(EVALUATE(lcTmpDtFl+".lOrginal"))
  *!*      SELECT SYSCHDUL
  *!*      lcStauts = SYSCHDUL.COPERSTAT
  *!*      LOCATE REST WHILE cconttype+cseqnumber+ccont_id+coperstat+cuser_id =;
  *!*                       EVALUATE(lcTmpDtFl+".cPrj_Typ+"+lcTmpDtFl+".cPrj_ID+"+lcTmpDtFl+".cOprt_Ctg+"+lcTmpDtFl+".cOprt_ID");
  *!*                       FOR COPERSTAT <> lcStauts
  *!*      IF !FOUND()
  *!*        =SEEK(EVALUATE(lcTmpDtFl+".cPrj_Typ+"+lcTmpDtFl+".cPrj_ID+"+lcTmpDtFl+".cOprt_Ctg+"+lcTmpDtFl+".cOprt_ID")+lcStauts,'SYSCHDUL')
  *!*      ENDIF
  *!*    ENDIF
  *!*
  *!*    IF SYSCHDUL.lPredComp
  *!*      SELECT (lcTmpDtFl)
  *!*      SCATTER MEMVAR MEMO
  *!*      ldest_Fnsh = IIF(EMPTY(m.dAct_Fnsh),IIF(EMPTY(m.dClc_Fnsh),m.dest_Fnsh,m.dClc_Fnsh),m.dAct_Fnsh)
  *!*      lnrem_dur  = IIF(EMPTY(m.nAct_dur),m.nrem_dur,0)
  *!*      lcInform = ""
  *!*      =lfUpdAdTrl(loFormSet)
  *!*      SELECT (loFormSet.lc_PrjAudt)
  *!*      =GFAUDTRL(loFormSet , lcProg, lcKey, lcApObjNam, lcEvent, lcInform)
  *!*    ENDIF
  *!*  ELSE
  *!*    SELECT (lcTmpDtFl)
  *!*    SCATTER MEMVAR MEMO
  *!*    ldest_Fnsh = IIF(EMPTY(m.dAct_Fnsh),IIF(EMPTY(m.dClc_Fnsh),m.dest_Fnsh,m.dClc_Fnsh),m.dAct_Fnsh)
  *!*    lnrem_dur  = IIF(EMPTY(m.nAct_dur),m.nrem_dur,0)
  *!*    lcInform = ""
  *!*    =lfUpdAdTrl(loFormSet)
  *!*    SELECT (loFormSet.lc_PrjAudt)
  *!*    =GFAUDTRL(loFormSet , lcProg, lcKey, lcApObjNam, lcEvent, lcInform)
  *!*  ENDIF

  *!*  *!*************************************************************
  *!*  *! Name      : lfUpdAdTrl
  *!*  *! Developer : AHMED MAHER (AMH)
  *!*  *! Date      : 09/13/2005
  *!*  *! Purpose   :
  *!*  *!*************************************************************
  *!*  *! Calls     : None
  *!*  *!*************************************************************
  *!*  *! Passed Parameters  :  None
  *!*  *!*************************************************************
  *!*  *! Returns            :  None
  *!*  *!*************************************************************
  *!*  *! Example            :  =lfUpdAdTrl()
  *!*  *!*************************************************************
  *!*  FUNCTION lfUpdAdTrl
  *!*  PARAMETERS loFormSet

  *!*  lc_PrjAudt = loFormSet.lc_PrjAudt
  *!*  SELECT (lc_PrjAudt)
  *!*  ZAP

  *!*  lcPrj_Typ = EVALUATE(loFormSet.lcTempFile+".cPrj_Typ")

  *!*  INSERT INTO (loFormSet.lc_PrjAudt) (cPrj_Typ,cOprt_Ctg,cOprt_ID,cOprt_Dsc,dest_Strt,dest_Fnsh,nrem_dur,;
  *!*                            cOprt_res,lVoid);
  *!*                    VALUES (lcPrj_Typ,m.cOprt_ctg,m.cOprt_ID,m.coprt_dsc,;
  *!*                            oAriaApplication.systemdate,ldest_Fnsh,lnrem_dur,;
  *!*                            IIF(EMPTY(m.cOprt_res),m.cGroup_ID,m.cOprt_res),m.lVoid)

  *!*  LOCAL lcFilter
  *!*  lcFilter = FILTER('SYSCHDUL')
  *!*  SET FILTER TO IN 'SYSCHDUL'

  *!*  IF SEEK(m.cPrj_Typ + m.cPrj_ID + m.cOprt_Ctg+m.cOprt_ID, 'SYSCHDUL')
  *!*    IF !(m.lOrginal)
  *!*      SELECT SYSCHDUL
  *!*      lcStauts = SYSCHDUL.COPERSTAT
  *!*      LOCATE REST WHILE cconttype+cseqnumber+ccont_id+coperstat+cuser_id =;
  *!*                      m.cPrj_Typ + m.cPrj_ID + m.cOprt_Ctg+m.cOprt_ID ;
  *!*                      FOR COPERSTAT <> lcStauts
  *!*      IF !FOUND()
  *!*        =SEEK(m.cPrj_Typ + m.cPrj_ID + m.cOprt_Ctg+m.cOprt_ID+lcStauts,'SYSCHDUL')
  *!*      ENDIF
  *!*    ENDIF
  *!*
  *!*    IF !(m.lVoid)
  *!*      REPLACE &lc_PrjAudt..cStatus WITH SYSCHDUL.COPERSTAT
  *!*    ELSE
  *!*      REPLACE &lc_PrjAudt..cStatus WITH 'X'
  *!*    ENDIF
  *!*  ELSE
  *!*    IF !(m.lVoid)
  *!*      REPLACE &lc_PrjAudt..cStatus WITH IIF(m.nact_dur <> 0  OR !EMPTY(m.dAct_Fnsh),'C','O')
  *!*    ELSE
  *!*      REPLACE &lc_PrjAudt..cStatus WITH 'X'
  *!*    ENDIF
  *!*  ENDIF

  *!*  SET FILTER TO &lcFilter. IN 'SYSCHDUL'

  *!*  SELECT (lc_PrjAudt)
  *!*  LOCATE

  *!*  IF !USED("PMCTGHD")
  *!*    =gfOpenTable(oAriaApplication.DataDir+'PMCTGHD','PMCTGHD','SH')
  *!*  ENDIF
  *!*  SELECT (lc_PrjAudt)
  *!*
  *!*  =SEEK(cOprt_Ctg,"PMCTGHD")
  *!*  lcInform = 'Operation Category: '           + PMCTGHD.cCtg_Dsc                     + CHR(13)+;
  *!*             'Activity: '                         + cOprt_Dsc                            + CHR(13)+;
  *!*             'User: '                         + cOprt_res                            + CHR(13)+;
  *!*             'Remaining Time on Completion: ' + ALLTRIM(STR(nRem_Dur,3)) + ' day(s)' + CHR(13)+;
  *!*             'Transaction Date: '             + DTOC(dEst_strt)                      + CHR(13)+;
  *!*             'Completion Date: '              + DTOC(dEst_Fnsh)                      + CHR(13)+;
  *!*             'Status: '+IIF(cStatus=[O],[Open],IIF(cStatus=[C],[Complete],IIF(cStatus=[P],[In Work],;
  *!*                        IIF(cStatus=[H],[Hold],IIF(cStatus=[X],[Void],[])))))


  *!*************************************************************
  *! Name      : lfvDayCmp
  *! Developer : Mariam Mazhar
  *! Date      : 06/01/2009
  *! Purpose   : Validate No. of days before complete
  *!*************************************************************
FUNCTION lfvDayCmp
  IF LNRPDYCMP < 0
    = gfModalGen('TRM42000B40011','DIALOG')
    LNRPDYCMP = 0
  ENDIF

  *!*************************************************************
  *! Name      : lfSrvTask
  *! Developer : Mariam Mazhar
  *! Date      : 06/01/2009
  *! Purpose   : Set Rest of Tasks Browser
  *!*************************************************************
FUNCTION lfSrvTask
  PARAMETERS lcParams
  lcCurAls = ALIAS()

  DO CASE
    CASE lcParams = 'S' AND !EMPTY(lcRpTemplt)
      *! B609028,1 MMT 10/07/2009 Fix bug of not exporting Task notes to excel[Start]
      *!*      IF gfSeek(lcRpTemplt,'PmRprTm')
      *!*        DIMENSION laTmpltOpr[1]
      *!*        lcTmp_Memo = PmRprTm.mTmp_Oprt
      *!*        =gfSubStr(lcTmp_Memo,@laTmpltOpr,'|')
      IF  gfSeek(lcRpTemplt,'PMPTHDT')
        *! B609028,1 MMT 10/07/2009 Fix bug of not exporting Task notes to excel[End]
        SELECT (lcTmpTasks)
        *! B609028,1 MMT 10/07/2009 Fix bug of not exporting Task notes to excel[Start]
        *SET FILTER TO ASCAN(laTmpltOpr,cOprt_Ctg+cOprt_Id) <> 0
        SET FILTER TO SEEK(PADR(lcRpTemplt,4)+COPRT_CTG+COPRT_ID,'PMPTHDT')
        *! B609028,1 MMT 10/07/2009 Fix bug of not exporting Task notes to excel[End]
      ENDIF
    CASE lcParams = 'R'
      SELECT(lcTmpTasks)
      SET FILTER TO
  ENDCASE
  SELECT (lcCurAls)

  *!*************************************************************
  *! Name      : lfvPrjTask
  *! Developer : Mariam Mazhar
  *! Date      : 06/01/2009
  *! Purpose   : Browse Activities to selct nominated one
  *!*************************************************************
FUNCTION lfvPrjTask
  *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
  *lcBrFields = [cOprt_ID : H = 'Activity ID', cOprt_Dsc : H = 'Activity Name']
  lcBrFields = [cOprt_ID : H = ']+LANG_MFPROJMON_ACTID+[', cOprt_Dsc : H = ']+LANG_MFPROJMON_ACTNAME+[']
  *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
  lnPOs = ASCAN(loogScroll.laogFxFlt,'PMPRJDT.COPRT_ID')
  lcSelTask =''
  IF lnPOs <> 0
    lnPOs = ASUBSCRIPT(loogScroll.laogFxFlt,lnPOs,1)
    lcSelTask  = loogScroll.laogFxFlt[lnPos ,6]
    SELECT(lcTmpTasks)
    IF !EMPTY(lcSelTask) AND USED(lcSelTask) AND RECCOUNT(lcSelTask)>0
      SET FILTER TO SEEK(COPRT_CTG+"-"+COPRT_ID, lcSelTask)
      *! B609028,1 MMT 10/07/2009 Fix bug of not exporting Task notes to excel[Start]
    ELSE
      IF !EMPTY(lcRpTemplt)
        =gfSeek(PADR(lcRpTemplt,4),'PMPTHDT')
        SELECT(lcTmpTasks)
        SET FILTER TO SEEK(PADR(lcRpTemplt,4)+COPRT_CTG+COPRT_ID,'PMPTHDT')
      ENDIF
      *! B609028,1 MMT 10/07/2009 Fix bug of not exporting Task notes to excel[End]
    ENDIF
    lnDashPos = ATC('-',lcRpTaskNom)
    IF !SEEK(PADR(SUBSTR(lcRpTaskNom,1,lnDashPos -1),3)+PADR(SUBSTR(lcRpTaskNom,lnDashPos +1),5),lcTmpTasks)
    *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
*!*        lcRpTaskNom = IIF(ARIABROW("",'Category\Activity code' ,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, ;
*!*          gnBrFSCol2,'','','COPRT_CTG+"-"+COPRT_ID','laBrowArr', .F.,lcTmpTasks,.F.,lcTmpTasks,'','','PMCTGDT'),COPRT_CTG+"-"+COPRT_ID,"")
      lcRpTaskNom = IIF(ARIABROW("",LANG_MFPROJMON_ACTCAT ,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, ;
        gnBrFSCol2,'','','COPRT_CTG+"-"+COPRT_ID','laBrowArr', .F.,lcTmpTasks,.F.,lcTmpTasks,'','','PMCTGDT'),COPRT_CTG+"-"+COPRT_ID,"")
    *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
    ENDIF
  ENDIF
  SELECT(lcTmpTasks)
  SET FILTER TO

  *!*************************************************************
  *! Name      : lfCopyExp
  *! Developer : Mariam Mazhar
  *! Date      : 06/01/2009
  *! Purpose   : Copy Fxflt array
  *!*************************************************************
FUNCTION lfCopyExp
  DIMENSION loFormSet.laFxFltCpy[ALEN(loogScroll.laogFxFlt,1),ALEN(loogScroll.laogFxFlt,2)]
  STORE '' TO loFormSet.laFxFltCpy
  ACOPY(loogScroll.laogFxFlt,loFormSet.laFxFltCpy)
  IF llRpShWDy AND EMPTY(LEFT(lcRpTaskNom,3))
    lcBrowMsg = LANG_MFPROJMON_NOMMSG
    gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcBrowMsg)
    RETURN .F.
  ENDIF

  *! B609028,1 MMT 10/07/2009 Fix bug of not exporting Task notes to excel[Start]
  lnTskPos = ASCAN(loogScroll.laogFxFlt,'PMPRJDT.COPRT_ID')
  lcTskSel =''
  llTskSelected = .F.
  IF lnTskPos <> 0
    lnTskPos = ASUBSCRIPT(loogScroll.laogFxFlt,lnTskPos ,1)
    lcTskSel =loogScroll.laogFxFlt[lnTskPos ,6]
    IF !EMPTY (lcTskSel) AND USED(lcTskSel)
      SELECT (lcTskSel)
      LOCATE
      IF !EOF()
        llTskSelected = .T.
      ENDIF
    ENDIF
  ENDIF
  
  
  IF !llTskSelected AND !EMPTY(lcRpTemplt)
    *! B609028,1 MMT 10/07/2009 Fix bug of not exporting Task notes to excel[End]
    *!*    IF gfSeek(lcRpTemplt,'PmRprTm')
    *!*      lcFilTasks = gfTempName()
    *!*      CREATE CURSOR (lcFilTasks) (KeyExp C(9))
    *!*      SELECT (lcFilTasks)
    *!*      INDEX on KeyExp TAG (lcFilTasks)
    *!*      lcTmp_Memo = PmRprTm.mTmp_Oprt
    *!*      lnStart=1
    *!*      lnEnd=AT('|',lcTmp_Memo)
    *!*      DO WHILE lnEnd <> 0
    *!*        SELECT(lcFilTasks)
    *!*        APPEND BLANK
    *!*        lcCurrVal = SUBSTR(lcTmp_Memo,lnStart,lnEnd-1)
    *!*        REPLACE KeyExp   WITH SUBSTR(lcCurrVal ,1,3)+"-"+SUBSTR(lcCurrVal ,4,5)
    *!*        lcTmp_Memo = STUFF(lcTmp_Memo ,lnStart,lnEnd,"")
    *!*        lnEnd=AT('|',lcTmp_Memo)
    *!*      ENDDO
    *!*      IF lnEnd = 0
    *!*        SELECT(lcFilTasks)
    *!*        APPEND BLANK
    *!*        REPLACE KeyExp  WITH SUBSTR(lcTmp_Memo,1,3)+"-"+SUBSTR(lcTmp_Memo,4,5)
    *!*      ENDIF
    IF gfSeek(lcRpTemplt,'PMPTHDT')
      lcFilTasks = gfTempName()
      CREATE CURSOR (lcFilTasks) (KeyExp C(9))
      SELECT (lcFilTasks)
      INDEX ON KeyExp TAG (lcFilTasks)
      SELECT  PMPTHDT
      SCAN REST WHILE CPATH_ID+COPRT_CTG+COPRT_ID =lcRpTemplt
        SELECT (lcFilTasks)
        APPEND BLANK
        REPLACE KeyExp  WITH PMPTHDT.COPRT_CTG+"-"+PMPTHDT.COPRT_ID
      ENDSCAN
      SELECT (lcFilTasks)
      LOCATE
      IF !EOF()
        loogScroll.laogFxFlt[lnTskPos ,6]=  lcFilTasks
        llTskSelected = .T.
        lcTskSel =lcFilTasks
      ENDIF
    ENDIF
  ENDIF
  lnTskCounter = 0
  IF llTskSelected AND !EMPTY(lcTskSel) AND USED(lcTskSel)
    SELECT (lcTskSel)
    COUNT FOR !DELETED() TO lnTskCounter
  ELSE
    IF !USED('PMCTGDT')
      gfOpenTable('PMCTGDT','PMCTGDT')
    ENDIF
    SELECT 'PMCTGDT'
    =gfSeek('')
    COUNT FOR !DELETED() TO lnTskCounter

    *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [Start]
    IF !EMPTY(LCRPRPTTM) AND LCRPRPTTM <> 'N/A'
      lcFilTasks = gfTempName()
      CREATE CURSOR (lcFilTasks) (KeyExp C(9))
      SELECT (lcFilTasks)
      INDEX ON KeyExp TAG (lcFilTasks)
      SELECT 'PMCTGDT'
      SCAN
    SELECT (lcFilTasks)
        APPEND BLANK
        REPLACE KeyExp  WITH PMCTGDT.COPRT_CTG+"-"+PMCTGDT.COPRT_ID        
      ENDSCAN  
    ENDIF 
    *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [End]
  ENDIF

  *! B608925,1 MMT 07/27/2009 Increase Number of tasks to be 85 instead of 25 [Start]
  *IF lnTskCounter > 25
  *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [Start]  
  *IF lnTskCounter > 85
  IF lnTskCounter > 85 AND (EMPTY(LCRPRPTTM) OR LCRPRPTTM = 'N/A')
  *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [End]
    *! B608925,1 MMT 07/27/2009 Increase Number of tasks to be 85 instead of 25 [End]

    lcBrowMsg = LANG_MFPROJMON_MAXMSG
    gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcBrowMsg)
    RETURN .F.
  ENDIF

  *!*************************************************************
  *! Name      : lfCreateTemp
  *! Developer : Mariam Mazhar
  *! Date      : 06/01/2009
  *! Purpose   : Create Temp. File
  *!*************************************************************
FUNCTION lfCreateTemp
  PARAMETERS loFormSet
  *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [Start]
  IF LCRPRPTTM <> 'N/A'
    lcBrFields = ''
    loFormSet.ADDPROPERTY('LATEMPFL[1]')
    loFormSet.ADDPROPERTY('LATEMPACT[1]')
    loFormSet.ADDPROPERTY('LATABLEUSE[1]')
    loFormSet.ADDPROPERTY('LATABLEFLD[1]')
    loFormSet.ADDPROPERTY('LATABLEPROF[1]')
    loFormSet.ADDPROPERTY('LAFLDMAP[1]')    
    DIMENSION loFormSet.LATEMPFL[1,2]
    STORE '' TO loFormSet.LATEMPFL
    DIMENSION loFormSet.LATEMPACT[1,2]
    STORE '' TO loFormSet.LATEMPACT
    DIMENSION loFormSet.LATABLEUSE[1]
    STORE '' TO  loFormSet.LATABLEUSE
    DIMENSION loFormSet.LATABLEFLD[1,2]
    STORE '' TO loFormSet.LATABLEFLD
    DIMENSION loFormSet.LATABLEPROF[1,2]
    STORE '' TO loFormSet.LATABLEPROF
    DIMENSION loFormSet.LAFLDMAP[1,2]
    STORE '' TO loFormSet.LAFLDMAP
    llFileCreated  = .F.
    lnFileID = 1
    llCreateTable = .F.
    loFormSet.ADDPROPERTY('LCTEMPFL'+ALLTRIM(STR(lnFileID)),gfTempName())
    lcCurrFile = EVALUATE('loFormSet.'+'LCTEMPFL'+ALLTRIM(STR(lnFileID)))
    loFormSet.LATEMPFL[1,1] = lcCurrFile
    DO CASE
      CASE lcRpPrjTp $ 'CAD'
        lcWhenType =  'C'
      CASE lcRpPrjTp $ 'PNR'
        lcWhenType = 'P'
      CASE lcRpPrjTp $ 'OT'
        lcWhenType = 'O'
      OTHERWISE
        lcWhenType =  lcRpPrjTp
    ENDCASE
    SELECT PMRPTMDT
    =gfSeek(lcWhenType +PADR(LCRPRPTTM ,3))
    DIMENSION laFileStruct[1,4]
    STORE '' TO laFileStruct
      *  E302694,1 MMT 05/16/2010 Add Shortcut to Project Monitor to complete tasks[Start]
*    DIMENSION laFileStruct[7,4]
    DIMENSION laFileStruct[8,4]
      *  E302694,1 MMT 05/16/2010 Add Shortcut to Project Monitor to complete tasks[End]
    laFileStruct[1,1] = 'CPRJ_TYP'
    laFileStruct[1,2] = 'C'
    laFileStruct[1,3] = 1
    laFileStruct[1,4] = 0

    laFileStruct[2,1] = 'CPRJ_ID'
    laFileStruct[2,2] = 'C'
    laFileStruct[2,3] = 6
    laFileStruct[2,4] = 0

    laFileStruct[3,1] = 'DREQ_FNSH'
    laFileStruct[3,2] = 'D'
    laFileStruct[3,3] = 8
    laFileStruct[3,4] = 0

    laFileStruct[4,1] = 'ACCOUNT'
    laFileStruct[4,2] = 'C'
    laFileStruct[4,3] = 6
    laFileStruct[4,4] = 0

    laFileStruct[5,1] = 'VENDOR'
    laFileStruct[5,2] = 'C'
    laFileStruct[5,3] = 8
    laFileStruct[5,4] = 0

    laFileStruct[6,1] = 'CSTYLE'
    laFileStruct[6,2] = 'C'
    laFileStruct[6,3] = 19
    laFileStruct[6,4] = 0

    laFileStruct[7,1] = 'LINENO'
    laFileStruct[7,2] = 'N'
    laFileStruct[7,3] = 6
    laFileStruct[7,4] = 0   

    *  E302694,1 MMT 05/16/2010 Add Shortcut to Project Monitor to complete tasks[Start]
    laFileStruct[8,1] = 'LSelect'
    laFileStruct[8,2] = 'L'
    laFileStruct[8,3] = 1
    laFileStruct[8,4] = 0   
    *  E302694,1 MMT 05/16/2010 Add Shortcut to Project Monitor to complete tasks[End]
*!*      laFileStruct[8,1] = 'NNumric'
*!*      laFileStruct[8,2] = 'N'
*!*      laFileStruct[8,3] = 15
*!*      laFileStruct[8,4] = 3    

*!*      laFileStruct[9,1] = 'CCharacter'
*!*      laFileStruct[9,2] = 'C'
*!*      laFileStruct[9,3] = 60
*!*      laFileStruct[9,4] = 0    

*!*      laFileStruct[10,1] = 'LLOGICAL'
*!*      laFileStruct[10,2] = 'L'
*!*      laFileStruct[10,3] = 1
*!*      laFileStruct[10,4] = 0    
*!*      
*!*      laFileStruct[11,1] = 'DDATEFLD'
*!*      laFileStruct[11,2] = 'D'
*!*      laFileStruct[11,3] = 8
*!*      laFileStruct[11,4] = 0    
    
    DO CASE
      CASE loFormSet.lcRpSrtBy $ 'OCT'
        lcSortBy = 'CPRJ_TYP+CPRJ_ID+CSTYLE+STR(LINENO,6)'

      CASE loFormSet.lcRpSrtBy = 'R'
        lcSortBy = 'CPRJ_TYP+DTOS(DREQ_FNSH)+CPRJ_ID+CSTYLE+STR(LINENO,6)'

      CASE loFormSet.lcRpSrtBy $ 'A'
        lcSortBy = 'CPRJ_TYP+ACCOUNT+CPRJ_ID+CSTYLE+STR(LINENO,6)'

      CASE loFormSet.lcRpSrtBy $ 'V'
        lcSortBy = 'CPRJ_TYP+VENDOR+CPRJ_ID+CSTYLE+STR(LINENO,6)'

      CASE loFormSet.lcRpSrtBy $ 'SM'
        IF lcRpPrjTp = 'S'
          lcSortBy = 'CPRJ_TYP+CSTYLE'
        ELSE
          lcSortBy = 'CPRJ_TYP+CPRJ_ID+CSTYLE+STR(LINENO,6)'
        ENDIF
    ENDCASE
    
    SELECT PMRPTMDT
    lnNum = 1
    SCAN REST WHILE CTMP_TYPE +CTMP_CODE+STR(NTMPSEQ,6)= lcWhenType +PADR(LCRPRPTTM ,3)
      DO CASE
        CASE TYPE  = 'F'
          IF !(ALLTRIM(KEY1) = 'PMPRJHD' AND INLIST(ALLTRIM(KEY2),'CPRJ_ID','CPRJ_TYP','DREQ_FNSH','CSTYLE','LINENO')) AND !INLIST(ALLTRIM(KEY2),'VENDOR','ACCOUNT')
            lnFldWdth = 0
            lcFldType = ''
            lnDecWdth = 0
            lcFldHead = ''
            IF ASCAN(loFormSet.LATABLEUSE,ALLTRIM(KEY1)) = 0
              IF EMPTY(loFormSet.LATABLEUSE[1])
                loFormSet.LATABLEUSE[1] = ALLTRIM(KEY1)
              ELSE
                DIMENSION loFormSet.LATABLEUSE[ALEN(loFormSet.LATABLEUSE,1)+1]
                loFormSet.LATABLEUSE[ALEN(loFormSet.LATABLEUSE,1)] = ALLTRIM(KEY1)
              ENDIF
            ENDIF
          
            IF INLIST(ALLTRIM(KEY1),'POSHDR','POSLN','CUTPICK','ITEM','PMPRJHD')
              =SEEK(SUBSTR(KEY2,1,10),'TmpSQLFIELD')
              lnFldWdth = TmpSQLFIELD.nfld_wdth
              lcFldType = TmpSQLFIELD.cdata_typ
              lnDecWdth = TmpSQLFIELD.nfld_dec
              *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
              *lcFldHead = TmpSQLFIELD.cfld_head
              lcFldHead = IIF(EMPTY(PMRPTMDT.CDESCRIP) OR ISNULL(PMRPTMDT.CDESCRIP),TmpSQLFIELD.cfld_head,PMRPTMDT.CDESCRIP)
              *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
            ELSE
              =SEEK(SUBSTR(KEY2,1,10),'TmpFoxFIELD')
              lnFldWdth = TmpFoxFIELD.nfld_wdth
              lcFldType = TmpFoxFIELD.cdata_typ
              lnDecWdth = TmpFoxFIELD.nfld_dec
              *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
              *lcFldHead = TmpFoxFIELD.cfld_head
              lcFldHead = IIF(EMPTY(PMRPTMDT.CDESCRIP) OR ISNULL(PMRPTMDT.CDESCRIP),TmpFoxFIELD.cfld_head,PMRPTMDT.CDESCRIP)              
              *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
            ENDIF
            *  E302650,2 MMT 12/24/2009 make project monitor work based on report templated selected [Start]
            llKeyFld = .F.
            IF INLIST(SUBSTR(KEY2,1,10),PADR('CPRJ_ID',10), PADR('ORDER',10)  ,PADR('PO',10),PADR('STYLE',10),;
                                               PADR('VENDOR',10) , PADR('ACCOUNT',10),PADR('STORE',10),PADR('CVENDCODE',10)) 
              llKeyFld = .T.                                 
            ENDIF 
            *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
            *IF  PADR('VENDOR',10) = SUBSTR(KEY2,1,10) AND lcRpPrjTp = 'N'
            IF (EMPTY(PMRPTMDT.CDESCRIP) OR ISNULL(PMRPTMDT.CDESCRIP)) AND PADR('VENDOR',10) = SUBSTR(KEY2,1,10) AND lcRpPrjTp = 'N'
            *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
              lcFldHead  = 'SHIP TO'
              llKeyFld = .F.
            ENDIF 
            *  E302650,2 MMT 12/24/2009 make project monitor work based on report templated selected [End]     

            
            lcNewFldName  = SUBSTR(KEY2,1,10)
            llFldChanged = .F.
            IF ASCAN(laFileStruct,ALLTRIM(KEY2),1,0,1) = 0
              IF EMPTY(laFileStruct[1,1])
                laFileStruct[1,1] = SUBSTR(KEY2,1,10)
                laFileStruct[1,2] = lcFldType
                laFileStruct[1,3] = lnFldWdth
                laFileStruct[1,4] = lnDecWdth
              ELSE
                DIMENSION laFileStruct[ALEN(laFileStruct,1)+1,4]
                laFileStruct[ALEN(laFileStruct,1),1] = SUBSTR(KEY2,1,10)
                laFileStruct[ALEN(laFileStruct,1),2] = lcFldType
                laFileStruct[ALEN(laFileStruct,1),3] = lnFldWdth
                laFileStruct[ALEN(laFileStruct,1),4] = lnDecWdth
              ENDIF
            ELSE
              lcNewFldName = PADR(SUBSTR(ALLTRIM(KEY1),1,4)+'_'+RIGHT(ALLTRIM(key2),5),10)
              *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
              *IF INLIST(UPPER(ALLTRIM(key1)),'ORDHDR','ORDLINE','POSHDR','POSLN','CUTPICK')
              IF (EMPTY(PMRPTMDT.CDESCRIP) OR ISNULL(PMRPTMDT.CDESCRIP)) AND INLIST(UPPER(ALLTRIM(key1)),'ORDHDR','ORDLINE','POSHDR','POSLN','CUTPICK')
              *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
                lcFldHead = IIF(SUBSTR(ALLTRIM(key1),1,2) = 'PO','PO',IIF(SUBSTR(ALLTRIM(key1),1,2) ='OR','SO','ALO.'))+' '+ ALLTRIM(lcFldHead)
              ENDIF   
              IF EMPTY(laFileStruct[1,1])
                laFileStruct[1,1] = lcNewFldName  
                laFileStruct[1,2] = lcFldType
                laFileStruct[1,3] = lnFldWdth
                laFileStruct[1,4] = lnDecWdth
              ELSE
                DIMENSION laFileStruct[ALEN(laFileStruct,1)+1,4]
                laFileStruct[ALEN(laFileStruct,1),1] = lcNewFldName  
                laFileStruct[ALEN(laFileStruct,1),2] = lcFldType
                laFileStruct[ALEN(laFileStruct,1),3] = lnFldWdth
                laFileStruct[ALEN(laFileStruct,1),4] = lnDecWdth
              ENDIF
              IF EMPTY(loFormSet.LAFLDMAP[1,1])
                loFormSet.LAFLDMAP[1,1] = lcNewFldName 
                loFormSet.LAFLDMAP[1,2] = ALLTRIM(KEY1)+'.'+ALLTRIM(key2)
              ELSE
                DIMENSION loFormSet.LAFLDMAP[ALEN(loFormSet.LAFLDMAP,1)+1,2]
                loFormSet.LAFLDMAP[ALEN(loFormSet.LAFLDMAP,1),1] = lcNewFldName 
                loFormSet.LAFLDMAP[ALEN(loFormSet.LAFLDMAP,1),2] = ALLTRIM(KEY1)+'.'+ALLTRIM(key2)
              ENDIF   
            ENDIF 
            IF ASCAN(loFormSet.LATABLEFLD,ALLTRIM(KEY1),1,0,1) = 0
              IF EMPTY(loFormSet.LATABLEFLD[1,1])
                loFormSet.LATABLEFLD[1,1] = ALLTRIM(KEY1)
                loFormSet.LATABLEFLD[1,2] = lcNewFldName 
              ELSE
                DIMENSION loFormSet.LATABLEFLD[ALEN(loFormSet.LATABLEFLD,1)+1,2]
                loFormSet.LATABLEFLD[ALEN(loFormSet.LATABLEFLD,1),1] = ALLTRIM(KEY1)
                loFormSet.LATABLEFLD[ALEN(loFormSet.LATABLEFLD,1),2] = lcNewFldName 
              ENDIF
            ELSE
              lnPos = ASUBSCRIPT(loFormSet.LATABLEFLD ,ASCAN(loFormSet.LATABLEFLD,ALLTRIM(KEY1),1,0,1),1)
              loFormSet.LATABLEFLD[lnPOS,2] = loFormSet.LATABLEFLD[lnPOS,2] + ","+ lcNewFldName 
            ENDIF

            *  E302650,2 MMT 12/24/2009 make project monitor work based on report templated selected [Start]
            *IF INLIST(lcNewFldName ,PADR('CPRJ_ID',10), PADR('ORDER',10)  ,PADR('PO',10),PADR('STYLE',10),;
                                               PADR('VENDOR',10) , PADR('ACCOUNT',10),PADR('STORE',10),PADR('CVENDCODE',10)) 
            * lcBrFields = lcBrFields + [,z]+IIF(UPPER(ALLTRIM(KEY2)) <>'STORE',IIF(UPPER(ALLTRIM(KEY2)) <>'CVENDCODE',SUBSTR(KEY2,1,1),'D'),'T')+[=lfFLD(']+IIF(UPPER(ALLTRIM(KEY2)) <>'STORE',IIF(UPPER(ALLTRIM(KEY2)) <>'CVENDCODE',SUBSTR(KEY2,1,1),'D'),'T')+['):37:H =']+ALLTRIM(lcFldHead)+[']                                                
            IF llKeyFld                                                                   
              lcBrFields = lcBrFields + [,z]+ALLTRIM(lcNewFldName)+[=lfFLD(']+ALLTRIM(lcNewFldName)+['):37:H =']+ALLTRIM(lcFldHead)+['] 
            *  E302650,2 MMT 12/24/2009 make project monitor work based on report templated selected [End]
            ELSE
              *  E302650,3 MMT 12/29/2009 make project monitor work based on report templated selected [Start]
              IF UPPER(ALLTRIM(KEY1)) = 'STYLE' AND UPPER(ALLTRIM(KEY2)) = 'MAKE'
                lcBrFields = lcBrFields + ",lcMaked = IIF("+lcCurrFile+".MAKE,'Y','N') : H='"+ALLTRIM(lcFldHead)+"'"
              ELSE 
              *  E302650,3 MMT 12/29/2009 make project monitor work based on report templated selected [End]
              lcBrFields = lcBrFields + "," +lcCurrFile+"."+lcNewFldName  +": H='"+ALLTRIM(lcFldHead)+"'"
              *  E302650,3 MMT 12/29/2009 make project monitor work based on report templated selected [Start]
              ENDIF
              *  E302650,3 MMT 12/29/2009 make project monitor work based on report templated selected [End]
            ENDIF    
          ELSE
            IF ALLTRIM(KEY1) = 'PMPRJHD' OR ALLTRIM(KEY2) = 'VENDOR'
              =SEEK(SUBSTR(KEY2,1,10),'TmpSQLFIELD')
              *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
              *lcFldHead = TmpSQLFIELD.cfld_head
              lcFldHead = IIF(EMPTY(PMRPTMDT.CDESCRIP) OR ISNULL(PMRPTMDT.CDESCRIP),TmpSQLFIELD.cfld_head,PMRPTMDT.CDESCRIP)              
              *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
            ELSE
              =SEEK(SUBSTR(KEY2,1,10),'TmpFoxFIELD')
              *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
              *lcFldHead = TmpFoxFIELD.cfld_head
              lcFldHead = IIF(EMPTY(PMRPTMDT.CDESCRIP) OR ISNULL(PMRPTMDT.CDESCRIP),TmpFoxFIELD.cfld_head,PMRPTMDT.CDESCRIP)              
              *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
            ENDIF
            *  E302650,2 MMT 12/24/2009 make project monitor work based on report templated selected [Start]
            *IF INLIST(UPPER(SUBSTR(KEY2,1,10)),PADR('CPRJ_ID',10), PADR('ORDER',10)  ,PADR('PO',10),PADR('STYLE',10),;
                                               PADR('VENDOR',10) , PADR('ACCOUNT',10),PADR('CVENDCODE',10),PADR('STORE',10)) 
            *lcBrFields = lcBrFields + [,z]+IIF(UPPER(ALLTRIM(KEY2)) <>'STORE',IIF(UPPER(ALLTRIM(KEY2)) <>'CVENDCODE',SUBSTR(KEY2,1,1),'D'),'T')+[=lfFLD(']+IIF(UPPER(ALLTRIM(KEY2)) <>'STORE',IIF(UPPER(ALLTRIM(KEY2)) <>'CVENDCODE',SUBSTR(KEY2,1,1),'D'),'T')+['):37:H =']+ALLTRIM(lcFldHead)+['] 
                        
            llKeyFld = .F.
            IF INLIST(SUBSTR(KEY2,1,10),PADR('CPRJ_ID',10), PADR('ORDER',10)  ,PADR('PO',10),PADR('STYLE',10),;
                                               PADR('VENDOR',10) , PADR('ACCOUNT',10),PADR('STORE',10),PADR('CVENDCODE',10)) 
              llKeyFld = .T.                                 
            ENDIF 
            *  E302650,2 MMT 12/24/2009 make project monitor work based on report templated selected [End]
                      
            lnFldWdth = 0
            lcFldType = ''
            lnDecWdth = 0
            lcFldHead = ''
            IF ASCAN(loFormSet.LATABLEUSE,ALLTRIM(KEY1)) = 0
              IF EMPTY(loFormSet.LATABLEUSE[1])
                loFormSet.LATABLEUSE[1] = ALLTRIM(KEY1)
              ELSE
                DIMENSION loFormSet.LATABLEUSE[ALEN(loFormSet.LATABLEUSE,1)+1]
                loFormSet.LATABLEUSE[ALEN(loFormSet.LATABLEUSE,1)] = ALLTRIM(KEY1)
              ENDIF
            ENDIF
          
            IF INLIST(ALLTRIM(KEY1),'POSHDR','POSLN','CUTPICK','ITEM','PMPRJHD')
              =SEEK(SUBSTR(KEY2,1,10),'TmpSQLFIELD')
              lnFldWdth = TmpSQLFIELD.nfld_wdth
              lcFldType = TmpSQLFIELD.cdata_typ
              lnDecWdth = TmpSQLFIELD.nfld_dec
              *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
              *lcFldHead = TmpSQLFIELD.cfld_head
              lcFldHead = IIF(EMPTY(PMRPTMDT.CDESCRIP) OR ISNULL(PMRPTMDT.CDESCRIP),TmpSQLFIELD.cfld_head,PMRPTMDT.CDESCRIP)
              *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
            ELSE
              =SEEK(SUBSTR(KEY2,1,10),'TmpFoxFIELD')
              lnFldWdth = TmpFoxFIELD.nfld_wdth
              lcFldType = TmpFoxFIELD.cdata_typ
              lnDecWdth = TmpFoxFIELD.nfld_dec
              *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
              *lcFldHead = TmpFoxFIELD.cfld_head
              lcFldHead = IIF(EMPTY(PMRPTMDT.CDESCRIP) OR ISNULL(PMRPTMDT.CDESCRIP),TmpFoxFIELD.cfld_head,PMRPTMDT.CDESCRIP)              
              *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
            ENDIF
            *  E302650,2 MMT 12/24/2009 make project monitor work based on report templated selected [Start]
            *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
            *IF  PADR('VENDOR',10) = SUBSTR(KEY2,1,10) AND lcRpPrjTp = 'N'
            IF (EMPTY(PMRPTMDT.CDESCRIP) OR ISNULL(PMRPTMDT.CDESCRIP)) AND PADR('VENDOR',10) = SUBSTR(KEY2,1,10) AND lcRpPrjTp = 'N'
            *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
              lcFldHead  = 'SHIP TO'
              llKeyFld = .F.
            ENDIF 
            *  E302650,2 MMT 12/24/2009 make project monitor work based on report templated selected [End]
            
            lcNewFldName  = SUBSTR(KEY2,1,10)
            llFldChanged = .F.
            IF ASCAN(laFileStruct,ALLTRIM(KEY2),1,0,1) = 0
              IF EMPTY(laFileStruct[1,1])
                laFileStruct[1,1] = SUBSTR(KEY2,1,10)
                laFileStruct[1,2] = lcFldType
                laFileStruct[1,3] = lnFldWdth
                laFileStruct[1,4] = lnDecWdth
              ELSE
                DIMENSION laFileStruct[ALEN(laFileStruct,1)+1,4]
                laFileStruct[ALEN(laFileStruct,1),1] = SUBSTR(KEY2,1,10)
                laFileStruct[ALEN(laFileStruct,1),2] = lcFldType
                laFileStruct[ALEN(laFileStruct,1),3] = lnFldWdth
                laFileStruct[ALEN(laFileStruct,1),4] = lnDecWdth
              ENDIF
            ELSE
              lcNewFldName = PADR(SUBSTR(ALLTRIM(KEY1),1,4)+'_'+RIGHT(ALLTRIM(key2),5),10)
              *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
              *IF INLIST(UPPER(ALLTRIM(key1)),'ORDHDR','ORDLINE','POSHDR','POSLN','CUTPICK')
              IF (EMPTY(PMRPTMDT.CDESCRIP) OR ISNULL(PMRPTMDT.CDESCRIP)) AND INLIST(UPPER(ALLTRIM(key1)),'ORDHDR','ORDLINE','POSHDR','POSLN','CUTPICK')
              *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
                lcFldHead = IIF(SUBSTR(ALLTRIM(key1),1,2) = 'PO','PO',IIF(SUBSTR(ALLTRIM(key1),1,2) ='OR','SO','ALO.'))+' '+ ALLTRIM(lcFldHead)
              ENDIF   
              IF EMPTY(laFileStruct[1,1])
                laFileStruct[1,1] = lcNewFldName  
                laFileStruct[1,2] = lcFldType
                laFileStruct[1,3] = lnFldWdth
                laFileStruct[1,4] = lnDecWdth
              ELSE
                DIMENSION laFileStruct[ALEN(laFileStruct,1)+1,4]
                laFileStruct[ALEN(laFileStruct,1),1] = lcNewFldName  
                laFileStruct[ALEN(laFileStruct,1),2] = lcFldType
                laFileStruct[ALEN(laFileStruct,1),3] = lnFldWdth
                laFileStruct[ALEN(laFileStruct,1),4] = lnDecWdth
              ENDIF
              IF EMPTY(loFormSet.LAFLDMAP[1,1])
                loFormSet.LAFLDMAP[1,1] = lcNewFldName 
                loFormSet.LAFLDMAP[1,2] = ALLTRIM(KEY1)+'.'+ALLTRIM(key2)
              ELSE
                DIMENSION loFormSet.LAFLDMAP[ALEN(loFormSet.LAFLDMAP,1)+1,2]
                loFormSet.LAFLDMAP[ALEN(loFormSet.LAFLDMAP,1),1] = lcNewFldName 
                loFormSet.LAFLDMAP[ALEN(loFormSet.LAFLDMAP,1),2] = ALLTRIM(KEY1)+'.'+ALLTRIM(key2)
              ENDIF   
            ENDIF 
            IF ASCAN(loFormSet.LATABLEFLD,ALLTRIM(KEY1),1,0,1) = 0
              IF EMPTY(loFormSet.LATABLEFLD[1,1])
                loFormSet.LATABLEFLD[1,1] = ALLTRIM(KEY1)
                loFormSet.LATABLEFLD[1,2] = lcNewFldName 
              ELSE
                DIMENSION loFormSet.LATABLEFLD[ALEN(loFormSet.LATABLEFLD,1)+1,2]
                loFormSet.LATABLEFLD[ALEN(loFormSet.LATABLEFLD,1),1] = ALLTRIM(KEY1)
                loFormSet.LATABLEFLD[ALEN(loFormSet.LATABLEFLD,1),2] = lcNewFldName 
              ENDIF
            ELSE
              lnPos = ASUBSCRIPT(loFormSet.LATABLEFLD ,ASCAN(loFormSet.LATABLEFLD,ALLTRIM(KEY1),1,0,1),1)
              loFormSet.LATABLEFLD[lnPOS,2] = loFormSet.LATABLEFLD[lnPOS,2] + ","+ lcNewFldName 
            ENDIF

            *  E302650,2 MMT 12/24/2009 make project monitor work based on report templated selected [Start]
            IF llKeyFld                                                                   
              lcBrFields = lcBrFields + [,z]+ALLTRIM(lcNewFldName)+[=lfFLD(']+ALLTRIM(lcNewFldName)+['):37:H =']+ALLTRIM(lcFldHead)+['] 
                                                                                
            *  E302650,2 MMT 12/24/2009 make project monitor work based on report templated selected [End]
                                               
            ELSE
              lcBrFields = lcBrFields + "," + loFormSet.LATEMPFL[1,1]+"."+ALLTRIM(lcNewFldName)+": H='"+ALLTRIM(lcFldHead)+"'"
            ENDIF   
          ENDIF

        CASE TYPE  = 'P'
          IF EMPTY(laFileStruct[1,1])
            laFileStruct[1,1] = 'P'+SUBSTR(KEY2,1,10)
            laFileStruct[1,2] = 'C'
            laFileStruct[1,3] = 30
            laFileStruct[1,4] = 0
          ELSE
            DIMENSION laFileStruct[ALEN(laFileStruct,1)+1,4]
            laFileStruct[ALEN(laFileStruct,1),1] = 'P'+SUBSTR(KEY2,1,6)
            laFileStruct[ALEN(laFileStruct,1),2] = "C"
            laFileStruct[ALEN(laFileStruct,1),3] = 30
            laFileStruct[ALEN(laFileStruct,1),4] = 0
          ENDIF
          IF EMPTY(loFormSet.LATABLEPROF[1,1])
            loFormSet.LATABLEPROF[1,1] = ALLTRIM(KEY2)
            loFormSet.LATABLEPROF[1,2] = ALLTRIM(KEY1)
          ELSE
            DIMENSION loFormSet.LATABLEPROF[ALEN(loFormSet.LATABLEPROF,1)+1,2]
            loFormSet.LATABLEPROF[ALEN(loFormSet.LATABLEPROF,1),1] = ALLTRIM(KEY2)
            loFormSet.LATABLEPROF[ALEN(loFormSet.LATABLEPROF,1),2] = ALLTRIM(KEY1)
          ENDIF

          lcBrFields = lcBrFields + "," +lcCurrFile+"."+'P'+SUBSTR(KEY2,1,6)+":H='"+ALLTRIM(gfCodDes(SUBSTR(KEY2 ,1,6), 'CPRO_CODE'))+"'"

        CASE TYPE = 'A'
          lcNum =ALLTRIM(STR(lnNum))
          IF 253 - ALEN(laFileStruct,1) < 1
            IF lnFileID = 1
              =gfCrtTmp(lcCurrFile ,@laFileStruct,lcSortBy,lcCurrFile)
              llFileCreated  = .T.
            ELSE
              =gfCrtTmp(lcCurrFile ,@laFileStruct,"RECNO()",lcCurrFile)
              llFileCreated  = .T.
            ENDIF
            lnFileID = lnFileID +  1
            loFormSet.ADDPROPERTY('LCTEMPFL'+ALLTRIM(STR(lnFileID)),gfTempName())
            lcCurrFile = EVALUATE('loFormSet.'+'LCTEMPFL'+ALLTRIM(STR(lnFileID)))
            DIMENSION loFormSet.LATEMPFL[ALEN(loFormSet.LATEMPFL,1)+1,2]
            loFormSet.LATEMPFL[ALEN(loFormSet.LATEMPFL,1),1] = lcCurrFile
            DIMENSION laFileStruct[1,4]
            STORE '' TO laFileStruct
            llFileCreated  = .F.
          ENDIF

          IF EMPTY(loFormSet.LATEMPACT[1,1])
            loFormSet.LATEMPACT[1,1] = SUBSTR(KEY1,1,3)+SUBSTR(KEY2,1,5)
            loFormSet.LATEMPACT[1,2] = lcCurrFile
          ELSE
            DIMENSION loFormSet.LATEMPACT[ALEN(loFormSet.LATEMPACT,1)+1,2]
            loFormSet.LATEMPACT[ALEN(loFormSet.LATEMPACT,1),1]  =SUBSTR(KEY1,1,3)+SUBSTR(KEY2,1,5)
            loFormSet.LATEMPACT[ALEN(loFormSet.LATEMPACT,1),2] = lcCurrFile
          ENDIF

          IF !EMPTY(laFileStruct[1,1])
            DIMENSION laFileStruct[ALEN(laFileStruct,1)+1,4]
          ENDIF

          *DIMENSION laFileStruct[ALEN(laFileStruct,1)+1,4]
          laFileStruct[ALEN(laFileStruct,1) ,1] = 'cTskSt' + lcNum
          laFileStruct[ALEN(laFileStruct,1) ,2] = 'C'
          laFileStruct[ALEN(laFileStruct,1) ,3] = 30
          laFileStruct[ALEN(laFileStruct,1) ,4] = 0
          *  E302650,2 MMT 12/24/2009 make project monitor work based on report templated selected [Start]
          *lcActDesc = ''
          lcActDesc = SUBSTR(KEY2,1,5)
          *  E302650,2 MMT 12/24/2009 make project monitor work based on report templated selected [End]
          IF !USED('PMCTGDT_T')
            gfOpenTable('PMCTGDT','PMCTGDT','SH','PMCTGDT_T')
          ENDIF
          *  E302650,2 MMT 12/24/2009 make project monitor work based on report templated selected [Start]
          *IF gfSeek(PADR(ALLTRIM(KEY1),3)+PADR(ALLTRIM(KEY2),5),'PMCTGDT_T','PMCTGDT')
          IF gfSeek(PADR(ALLTRIM(KEY1),3)+PADR(ALLTRIM(KEY2),5),'PMCTGDT_T','PMCTGDT') AND !EMPTY(PMCTGDT_T.COPRT_DSC)
          *  E302650,2 MMT 12/24/2009 make project monitor work based on report templated selected [End]
            lcActDesc = PMCTGDT_T.COPRT_DSC
          ENDIF
          lcBrFields = lcBrFields + [,z]+lcNum+[=lfTSK(']+lcNum+['):37:H =']+ALLTRIM(lcActDesc)+[']
          lnNum = lnNum +  1
      ENDCASE

      IF ALEN(laFileStruct,1) = 253
        IF lnFileID = 1
          =gfCrtTmp(lcCurrFile ,@laFileStruct,lcSortBy,lcCurrFile)
          llFileCreated  = .T.
        ELSE
          =gfCrtTmp(lcCurrFile ,@laFileStruct,"RECNO()",lcCurrFile)
          llFileCreated  = .T.
        ENDIF
        lnFileID = lnFileID +  1
        loFormSet.ADDPROPERTY('LCTEMPFL'+ALLTRIM(STR(lnFileID)),gfTempName())
        lcCurrFile = EVALUATE('loFormSet.'+'LCTEMPFL'+ALLTRIM(STR(lnFileID)))
        DIMENSION loFormSet.LATEMPFL[ALEN(loFormSet.LATEMPFL,1)+1,2]
        loFormSet.LATEMPFL[ALEN(loFormSet.LATEMPFL,1),1] = lcCurrFile
        DIMENSION laFileStruct[1,4]
        STORE '' TO laFileStruct
        llFileCreated  = .F.
      ENDIF
    ENDSCAN
  
  
    FOR lnW = 1  TO ALEN(loFormSet.LATEMPACT,1)
      IF 253 - ALEN(laFileStruct,1) < 6
        IF lnFileID = 1
          =gfCrtTmp(lcCurrFile ,@laFileStruct,lcSortBy,lcCurrFile)
          llFileCreated  = .T.
        ELSE
          =gfCrtTmp(lcCurrFile ,@laFileStruct,"RECNO()",lcCurrFile)
          llFileCreated  = .T.
        ENDIF
        lnFileID = lnFileID +  1
        loFormSet.ADDPROPERTY('LCTEMPFL'+ALLTRIM(STR(lnFileID)),gfTempName())
        lcCurrFile = EVALUATE('loFormSet.'+'LCTEMPFL'+ALLTRIM(STR(lnFileID)))
        DIMENSION loFormSet.LATEMPFL[ALEN(loFormSet.LATEMPFL,1)+1,2]
        loFormSet.LATEMPFL[ALEN(loFormSet.LATEMPFL,1),1] = lcCurrFile
        DIMENSION laFileStruct[1,4]
        STORE '' TO laFileStruct
        llFileCreated  = .F.
      ENDIF       
      lcNum = ALLTRIM(STR(lnW))
      IF !EMPTY(laFileStruct[1,1])
        DIMENSION laFileStruct[ALEN(laFileStruct,1)+1,4]
      ENDIF
      
      laFileStruct[ALEN(laFileStruct,1) ,1] = 'cOprID' + lcNum
      laFileStruct[ALEN(laFileStruct,1) ,2] = 'C'
      laFileStruct[ALEN(laFileStruct,1) ,3] = 5
      laFileStruct[ALEN(laFileStruct,1),4] = 0

      DIMENSION laFileStruct[ALEN(laFileStruct,1)+1,4]
      laFileStruct[ALEN(laFileStruct,1) ,1] = 'cOpDsc' + lcNum
      laFileStruct[ALEN(laFileStruct,1),2] = 'C'
      laFileStruct[ALEN(laFileStruct,1),3] = 40
      laFileStruct[ALEN(laFileStruct,1) ,4] = 0

      DIMENSION laFileStruct[ALEN(laFileStruct,1)+1,4]
      laFileStruct[ALEN(laFileStruct,1) ,1] = 'COPCTG' + lcNum
      laFileStruct[ALEN(laFileStruct,1),2] = 'C'
      laFileStruct[ALEN(laFileStruct,1) ,3] = 3
      laFileStruct[ALEN(laFileStruct,1) ,4] = 0

      DIMENSION laFileStruct[ALEN(laFileStruct,1)+1,4]
      laFileStruct[ALEN(laFileStruct,1) ,1] = 'dAcFnh'+ lcNum
      laFileStruct[ALEN(laFileStruct,1) ,2] = 'D'
      laFileStruct[ALEN(laFileStruct,1) ,3] = 8
      laFileStruct[ALEN(laFileStruct,1) ,4] = 0

    

      DIMENSION laFileStruct[ALEN(laFileStruct,1)+1,4]
      laFileStruct[ALEN(laFileStruct,1) ,1] = 'DClFnh'+ lcNum
      laFileStruct[ALEN(laFileStruct,1) ,2] = 'D'
      laFileStruct[ALEN(laFileStruct,1),3] = 8
      laFileStruct[ALEN(laFileStruct,1) ,4] = 0

      DIMENSION laFileStruct[ALEN(laFileStruct,1)+1,4]
      laFileStruct[ALEN(laFileStruct,1) ,1] = 'mOpCom'+ lcNum
      laFileStruct[ALEN(laFileStruct,1) ,2] = 'M'
      laFileStruct[ALEN(laFileStruct,1),3] = 10
      laFileStruct[ALEN(laFileStruct,1) ,4] = 0
    ENDFOR 
  
  
    IF !llFileCreated  AND !EMPTY(laFileStruct[1,1])
      IF lnFileID = 1
        =gfCrtTmp(lcCurrFile ,@laFileStruct,lcSortBy,lcCurrFile)
      ELSE
        =gfCrtTmp(lcCurrFile ,@laFileStruct,"RECNO()",lcCurrFile)
      ENDIF
    ENDIF

    IF !EMPTY(loFormSet.LATEMPFL[1,1])AND USED(loFormSet.LATEMPFL[1,1])
      IF USED(loFormSet.LATEMPFL[1,1])
        USE IN (loFormSet.LATEMPFL[1,1])
      ENDIF
      USE oAriaApplication.WorkDir+loFormSet.LATEMPFL[1,1]+'.DBF'  IN 0 SHARED
      SELECT (loFormSet.LATEMPFL[1,1])
      SET ORDER TO 1
      FOR lnT = 2 TO ALEN(loFormSet.LATEMPFL,1)
        IF !EMPTY(loFormSet.LATEMPFL[lnT,1])AND USED(loFormSet.LATEMPFL[lnT,1])
          USE IN (loFormSet.LATEMPFL[lnT,1])
          USE oAriaApplication.WorkDir+loFormSet.LATEMPFL[lnT,1]+'.DBF'  IN 0 SHARED ORDER 1
          SELECT (loFormSet.LATEMPFL[1,1])
          SET RELATION TO RECNO() INTO (loFormSet.LATEMPFL[lnT,1]) ADDITIVE
        ENDIF
      ENDFOR
    ENDIF

    IF SUBSTR(lcBrFields,1,1) = ','
      lcBrFields = SUBSTR(lcBrFields,2)
    ENDIF
  ELSE
  *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [End]


    DIMENSION laFlFldCust[3,4]
    laFlFldCust[1,1] = 'cfld_name'
    laFlFldCust[1,2] = 'C'
    laFlFldCust[1,3] = 10
    laFlFldCust[1,4] = 0

    laFlFldCust[2,1] = 'cfld_head'
    laFlFldCust[2,2] = 'C'
    laFlFldCust[2,3] = 25
    laFlFldCust[2,4] = 0

    laFlFldCust[3,1] = 'cupgrdlvl'
    laFlFldCust[3,2] = 'C'
    laFlFldCust[3,3] = 1
    laFlFldCust[3,4] = 0

    =gfCrtTmp(loFormSet.lcTempFld ,@laFlFldCust,'cfld_head',loFormSet.lcTempFld)
    SELECT PmPrjHd
    lnFldCnt = AFIELDS(laFileStru)
    lfAddFldtoArray(@laFileStru,'cProjStat','C',8,0)
    lfAddFldtoArray(@laFileStru,'cProjType','C',20,0)
    lfAddFldtoArray(@laFileStru,'ScaleDsc','C',10,0)
    *  E302694,1 MMT 05/16/2010 Add Shortcut to Project Monitor to complete tasks[Start]
    lfAddFldtoArray(@laFileStru,'LSelect','L',1,0)
    *  E302694,1 MMT 05/16/2010 Add Shortcut to Project Monitor to complete tasks[End]
    IF !(lcRpPrjTp $ 'SHM')
      lfAddFldtoArray(@laFileStru,'STYLE','C',19,0)
      lfAddFldtoArray(@laFileStru,'COMPLETE2','D',8,0)
      lfAddFldtoArray(@laFileStru,'ENTERED2','D',8,0)
      lfAddFldtoArray(@laFileStru,'AVIALABLE','D',8,0)
      *! B609028,1 MMT 10/07/2009 Fix bug of not exporting Task notes to excel[Start]
      *lfAddFldtoArray(@laFileStru,'TOTQTY','N',7,0)
      IF !(lcRpPrjTp $ 'PO')
        lfAddFldtoArray(@laFileStru,'TOTQTY','N',12,3)
      ELSE
        lfAddFldtoArray(@laFileStru,'TOTQTY','N',12,0)
      ENDIF
      *! B609028,1 MMT 10/07/2009 Fix bug of not exporting Task notes to excel[ENd]
      lfAddFldtoArray(@laFileStru,'DESC1','C',60,0)
      lfAddFldtoArray(@laFileStru,'SEASON','C',6,0)
      lfAddFldtoArray(@laFileStru,'CDIVISION','C',6,0)
      lfAddFldtoArray(@laFileStru,'CWARECODE','C',6,0)

      *! B609091,1 MMT 11/18/2009 Add Order Status Field in Browse Fields[Start]
      *lfAddFldtoArray(@laFileStru,'Status','C',10,0)
      lfAddFldtoArray(@laFileStru,'Status','C',1,0)
      *! B609091,1 MMT 11/18/2009 Add Order Status Field in Browse Fields[End]
    ENDIF

    IF lcRpPrjTp $ 'POCT'

      lfAddFldtoArray(@laFileStru,'nTaskStat','N',9,0)
      lfAddFldtoArray(@laFileStru,'Order','C',6,0)
      lfAddFldtoArray(@laFileStru,'Store','C',8,0)
      lfAddFldtoArray(@laFileStru,'ACCOUNT','C',5,0)
      lfAddFldtoArray(@laFileStru,'BTNAME','C',30,0)
      lfAddFldtoArray(@laFileStru,'CUSTPO','C',15,0)
      lfAddFldtoArray(@laFileStru,'COMPLETE1','D',8,0)
      lfAddFldtoArray(@laFileStru,'ENTERED1','D',8,0)
      lfAddFldtoArray(@laFileStru,'START','D',8,0)
      lfAddFldtoArray(@laFileStru,'PRICE','N',13,3)
      lfAddFldtoArray(@laFileStru,'TOTWIP','N',7,0)
      lfAddFldtoArray(@laFileStru,'DYFACT','N',5,0)

      *! B609028,1 MMT 10/07/2009 Fix bug of not exporting Task notes to excel[Start]
      lfAddFldtoArray(@laFileStru,'QTY1','N',11,IIF(!(lcRpPrjTp $ 'PO'), 3,0))
      lfAddFldtoArray(@laFileStru,'QTY2','N',11,IIF(!(lcRpPrjTp $ 'PO'), 3,0))
      lfAddFldtoArray(@laFileStru,'QTY4','N',11,IIF(!(lcRpPrjTp $ 'PO'), 3,0))
      lfAddFldtoArray(@laFileStru,'QTY3','N',11,IIF(!(lcRpPrjTp $ 'PO'), 3,0))
      lfAddFldtoArray(@laFileStru,'QTY5','N',11,IIF(!(lcRpPrjTp $ 'PO'), 3,0))
      lfAddFldtoArray(@laFileStru,'QTY6','N',11,IIF(!(lcRpPrjTp $ 'PO'), 3,0))
      lfAddFldtoArray(@laFileStru,'QTY7','N',11,IIF(!(lcRpPrjTp $ 'PO'), 3,0))
      lfAddFldtoArray(@laFileStru,'QTY8','N',11,IIF(!(lcRpPrjTp $ 'PO'), 3,0))
      lfAddFldtoArray(@laFileStru,'CSTYGROUP','C',6,0)
      *! B609028,1 MMT 10/07/2009 Fix bug of not exporting Task notes to excel[End]


      IF !EMPTY(lcSOProf1)
        lfAddFldtoArray(@laFileStru,'PROFILE1','C',30,0)
      ENDIF

      IF !EMPTY(lcSOProf2)
        lfAddFldtoArray(@laFileStru,'PROFILE2','C',30,0)
      ENDIF

      IF !EMPTY(lcSOProf3)
        lfAddFldtoArray(@laFileStru,'PROFILE3','C',30,0)
      ENDIF

      IF !EMPTY(lcPOProf1)
        lfAddFldtoArray(@laFileStru,'PROFILE11','C',30,0)
      ENDIF

      IF !EMPTY(lcPOProf2)
        lfAddFldtoArray(@laFileStru,'PROFILE12','C',30,0)
      ENDIF

      IF !EMPTY(lcPOProf3)
        lfAddFldtoArray(@laFileStru,'PROFILE13','C',30,0)
      ENDIF


      lfAddFldtoArray(@laFileStru,'SHIPVIA','C',6,0)
      lfAddFldtoArray(@laFileStru,'CTERMCODE','C',6,0)
      lfAddFldtoArray(@laFileStru,'SPCINST','C',6,0)
      lfAddFldtoArray(@laFileStru,'CPURCODE','C',6,0)
      lfAddFldtoArray(@laFileStru,'REP1','C',3,0)
      lfAddFldtoArray(@laFileStru,'REP2','C',3,0)
      lfAddFldtoArray(@laFileStru,'CCURRCODE','C',3,0)
      lfAddFldtoArray(@laFileStru,'DEPT','C',3,0)
      lfAddFldtoArray(@laFileStru,'PO','C',6,0)



      *Add User Defind Fields of POSHDR Table
      *E303030,1 BEGIN
*!*	      lnFldResult = oAriaApplication.remotesystemdata.execute;
*!*	        ("SELECT SYDFIELD.CFLD_NAME,SYDFIELD.CFLD_HEAD,SYDFLFLD.CUPGRDLVL,SYDFIELD.cdata_typ ,SYDFIELD.nfld_wdth ,SYDFIELD.nfld_dec  FROM SYDFIELD,SYDFLFLD ;
*!*	        WHERE SYDFIELD.cfld_name = SYDFLFLD.cfld_name AND SYDFLFLD.cfile_nam = '"+PADR('POSHDR',8)+;
*!*	        "' AND sydFlflD.cupgrdlvl = 'U' ORDER BY SYDFIELD.CFLD_HEAD",'','TmpFLFld',"",oAriaApplication.cAria4Sysfiles,3,;
*!*	        "",SET("Datasession"))
      lnFldResult = oAriaApplication.remotesystemdata.execute;
        ("SELECT SYDFIELD.CFLD_NAME,SYDFIELD.CFLD_HEAD,SYDFLFLD.CUPGRDLVL,SYDFIELD.cdata_typ ,SYDFIELD.nfld_wdth ,SYDFIELD.nfld_dec  FROM SYDFIELD,SYDFLFLD ;
        WHERE SYDFIELD.cfld_name = SYDFLFLD.cfld_name AND SYDFLFLD.cfile_nam = '"+PADR('POSHDR',oAriaApplication.FileW)+;
        "' AND sydFlflD.cupgrdlvl = 'U' ORDER BY SYDFIELD.CFLD_HEAD",'','TmpFLFld',"",oAriaApplication.cAria4Sysfiles,3,;
        "",SET("Datasession"))
      *E303030,1 END

      IF lnFldResult >= 1
        SELECT 'TmpFLFld'
        LOCATE
        IF !EOF()
          SCAN
            lfAddFldtoArray(@laFileStru,TmpFLFld.cfld_name,TmpFLFld.cdata_typ ,TmpFLFld.nfld_wdth ,TmpFLFld.nfld_dec)
            IF  EMPTY(laPoCustFld[1,1])
              laPoCustFld[1,1] = TmpFLFld.cfld_name
              laPoCustFld[1,2] = TmpFLFld.cdata_typ
            ELSE
              DIMENSION laPoCustFld[ALEN(laPoCustFld,1)+1,2]
              laPoCustFld[ALEN(laPoCustFld,1),1] = TmpFLFld.cfld_name
              laPoCustFld[ALEN(laPoCustFld,1),2] = TmpFLFld.cdata_typ
            ENDIF
            SELECT(loFormSet.lcTempFld)
            APPEND BLANK
            REPLACE cfld_name WITH TmpFLFld.cfld_name,;
              cupgrdlvl WITH TmpFLFld.cupgrdlvl ,;
              cfld_head WITH TmpFLFld.cfld_head
          ENDSCAN
        ENDIF
      ENDIF


    ENDIF

    IF lcRpPrjTp $ 'POCTANDRC'
      lfAddFldtoArray(@laFileStru,'Partner','C',8,0)
    ENDIF

    lnTaskCnt = 0
    lnPOs = ASCAN(loFormSet.laFxFltCpy,'PMPRJDT.COPRT_ID')
    lcSelTask =''
    IF lnPOs <> 0
      lnPOs = ASUBSCRIPT(loFormSet.laFxFltCpy,lnPOs,1)
      lcSelTask  =loFormSet.laFxFltCpy[lnPos ,6]
      IF !EMPTY(lcSelTask) AND USED(lcSelTask)
        SELECT (lcSelTask)
        COUNT FOR !DELETED() TO lnTaskCnt
      ENDIF
    ENDIF
    IF lnTaskCnt = 0 AND !EMPTY(lcRpTemplt)

      *! B609028,1 MMT 10/07/2009 Fix bug of not exporting Task notes to excel[Start]
      *!*    IF gfSeek(lcRpTemplt,'PmRprTm')
      *!*      DIMENSION laTmpltOpr[1]
      *!*      laTmpltOpr = .F.
      *!*      lcTmp_Memo = PmRprTm.mTmp_Oprt
      *!*      IF EOF('PmRprTm')
      *!*        GO TOP IN PmRprTm
      *!*      ENDIF
      *!*      =gfSubStr(lcTmp_Memo,@laTmpltOpr,'|')
      *!*
      *!*      lnTaskCnt = ALEN(laTmpltOpr,1)
      IF gfSeek(PADR(lcRpTemplt,4),'PMPTHDT')
        SELECT PMPTHDT
        COUNT FOR CPATH_ID+COPRT_CTG+COPRT_ID = PADR(lcRpTemplt,4)    TO lnTaskCnt
        *! B609028,1 MMT 10/07/2009 Fix bug of not exporting Task notes to excel[End]
      ENDIF
    ENDIF

    IF lnTaskCnt = 0
      IF !USED('PMCTGDT')
        gfOpenTable('PMCTGDT','PMCTGDT')
      ENDIF
      SELECT 'PMCTGDT'
      =gfSeek('')
      COUNT FOR !DELETED() TO lnTaskCnt
    ENDIF

    IF lnTaskCnt <> 0
      FOR lnCountNum=1 TO lnTaskCnt
        lcNum = ALLTRIM(STR(lnCountNum))
        *!*      lfAddFldtoArray(@laFileStru,'cOprID' + lcNum,'C',5,0)
        *!*      lfAddFldtoArray(@laFileStru,'cOpDsc' + lcNum,'C',40,0)
        *!*      lfAddFldtoArray(@laFileStru,'COPCTG' + lcNum,'C',3,0)
        *!*      lfAddFldtoArray(@laFileStru,'cTskSt'+ lcNum,'C',20,0)
        *!*      lfAddFldtoArray(@laFileStru,'dAcFnh'+ lcNum,'D',8,0)
        *!*      lfAddFldtoArray(@laFileStru,'DClFnh'+ lcNum,'D',8,0)
        lfAddFldtoArray(@laFileStru,'mOpCom'+ lcNum,'M',10,0)
      ENDFOR
      *! B608925,1 MMT 07/27/2009 Increase Number of tasks to be 85 instead of 25 [Start]
      *IF ALEN(laFileStru,1) > 256
      IF lnTaskCnt > 85
        *! B608925,1 MMT 07/27/2009 Increase Number of tasks to be 85 instead of 25 [End]

        lcBrowMsg = LANG_MFPROJMON_MAXMSG
        gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcBrowMsg)
        RETURN .F.
      ENDIF
      *! B608925,1 MMT 07/27/2009 Increase Number of tasks to be 85 instead of 25 [Start]
      * FOR lnCountNum = lnTaskCnt + 1 TO 25
      FOR lnCountNum = lnTaskCnt + 1 TO 85
        *! B608925,1 MMT 07/27/2009 Increase Number of tasks to be 85 instead of 25 [End]
        lcNum = ALLTRIM(STR(lnCountNum))

        *! B608925,1 MMT 07/27/2009 Increase Number of tasks to be 85 instead of 25 [Start]
        *!*      lfAddFldtoArray(@laFileStru,'cOprID' + lcNum,'C',5,0)
        *!*      lfAddFldtoArray(@laFileStru,'cOpDsc' + lcNum,'C',40,0)
        *!*      lfAddFldtoArray(@laFileStru,'COPCTG' + lcNum,'C',3,0)
        *!*      lfAddFldtoArray(@laFileStru,'cTskSt'+ lcNum,'C',20,0)
        *!*      lfAddFldtoArray(@laFileStru,'dAcFnh'+ lcNum,'D',8,0)
        *!*      lfAddFldtoArray(@laFileStru,'DClFnh'+ lcNum,'D',8,0)
        *! B608925,1 MMT 07/27/2009 Increase Number of tasks to be 85 instead of 25 [End]

        lfAddFldtoArray(@laFileStru,'mOpCom'+ lcNum,'M',10,0)
      ENDFOR





      DO CASE
        CASE loFormSet.lcRpSrtBy $ 'OCT'
          lcSortBy = 'CPRJ_TYP+cPrj_Id'
        CASE loFormSet.lcRpSrtBy = 'R'
          lcSortBy = 'CPRJ_TYP+DTOS(dReq_Fnsh)+cPrj_Id'
        CASE loFormSet.lcRpSrtBy $ 'A'
          lcSortBy = 'CPRJ_TYP+ACCOUNT+cPrj_Id'
        CASE loFormSet.lcRpSrtBy $ 'V'
          lcSortBy = 'CPRJ_TYP+Partner+cPrj_Id'
        CASE loFormSet.lcRpSrtBy $ 'SM'
          IF lcRpPrjTp = 'S'
            lcSortBy = 'CPRJ_TYP+cStyle'
          ELSE
            lcSortBy = 'CPRJ_TYP+cPrj_Id'
          ENDIF
      ENDCASE
      =gfCrtTmp(loFormSet.lcTempFile,@laFileStru,lcSortBy,loFormSet.lcTempFile)

      IF USED(loFormSet.lcTempFile)
        USE IN (loFormSet.lcTempFile)
      ENDIF
      USE oAriaApplication.WorkDir+loFormSet.lcTempFile+'.DBF'  IN 0 SHARED



      *! B608925,1 MMT 07/27/2009 Increase Number of tasks to be 85 instead of 25 [Start]
      IF lnTaskCnt <> 0
        DIMENSION laTmpFlStru2[255,4]
        DIMENSION laTmpFlStru3[255,4]
        *!*    laTmpFlStru2[1,1] = 'nRecNo'
        *!*    laTmpFlStru2[1,2] = 'N'
        *!*    laTmpFlStru2[1,3] = 8
        *!*    laTmpFlStru2[1,4] = 0
        *!*    laTmpFlStru3[1,1] = 'nRecNo'
        *!*    laTmpFlStru3[1,2] = 'N'
        *!*    laTmpFlStru3[1,3] = 8
        *!*    laTmpFlStru3[1,4] = 0
        lnCnter = 1
        FOR lnCountNum =  1 TO 85
          lcNum = ALLTRIM(STR(lnCountNum))
          laTmpFlStru2[lnCnter ,1] = 'cOprID' + lcNum
          laTmpFlStru2[lnCnter ,2] = 'C'
          laTmpFlStru2[lnCnter ,3] = 5
          laTmpFlStru2[lnCnter ,4] = 0

          laTmpFlStru3[lnCnter ,1] = 'cOpDsc' + lcNum
          laTmpFlStru3[lnCnter ,2] = 'C'
          laTmpFlStru3[lnCnter ,3] = 40
          laTmpFlStru3[lnCnter ,4] = 0
          lnCnter = lnCnter + 1

          laTmpFlStru2[lnCnter ,1] = 'COPCTG' + lcNum
          laTmpFlStru2[lnCnter ,2] = 'C'
          laTmpFlStru2[lnCnter ,3] = 3
          laTmpFlStru2[lnCnter ,4] = 0

          laTmpFlStru3[lnCnter ,1] = 'dAcFnh'+ lcNum
          laTmpFlStru3[lnCnter ,2] = 'D'
          laTmpFlStru3[lnCnter ,3] = 8
          laTmpFlStru3[lnCnter ,4] = 0
          lnCnter = lnCnter + 1

          laTmpFlStru2[lnCnter ,1] = 'cTskSt' + lcNum
          laTmpFlStru2[lnCnter ,2] = 'C'
          laTmpFlStru2[lnCnter ,3] = 20
          laTmpFlStru2[lnCnter ,4] = 0

          laTmpFlStru3[lnCnter ,1] = 'DClFnh'+ lcNum
          laTmpFlStru3[lnCnter ,2] = 'D'
          laTmpFlStru3[lnCnter ,3] = 8
          laTmpFlStru3[lnCnter ,4] = 0
          lnCnter = lnCnter + 1
        ENDFOR
        =gfCrtTmp(loFormSet.lcTempFile2,@laTmpFlStru2,"RECNo()",loFormSet.lcTempFile2)
        =gfCrtTmp(loFormSet.lcTempFile3,@laTmpFlStru3,"RECNo()",loFormSet.lcTempFile3)
      ENDIF
      *! B608925,1 MMT 07/27/2009 Increase Number of tasks to be 85 instead of 25 [End]

      SELECT(loFormSet.lcTempFile)
      SET ORDER TO  (loFormSet.lcTempFile)
      *! B608925,1 MMT 07/27/2009 Increase Number of tasks to be 85 instead of 25 [Start]
      SET RELATION TO  RECNO() INTO (loFormSet.lcTempFile2) ADDITIVE
      SET RELATION TO  RECNO() INTO (loFormSet.lcTempFile3) ADDITIVE
      *! B608925,1 MMT 07/27/2009 Increase Number of tasks to be 85 instead of 25 [End]
    ENDIF
    *



    SELECT(loFormSet.lcTempFld)
    *E303030,1 BEGIN
*!*	    lnFldResult = oAriaApplication.remotesystemdata.execute;
*!*	      ("SELECT SYDFIELD.CFLD_NAME,SYDFIELD.CFLD_HEAD,SYDFIELD.CUPGRDLVL FROM SYDFIELD,SYDFLFLD ;
*!*	WHERE SYDFIELD.cfld_name = SYDFLFLD.cfld_name AND SYDFLFLD.cfile_nam = '"+PADR('PMPRJHD',8)+"' ORDER BY SYDFIELD.CFLD_HEAD",'','TempFld',"",oAriaApplication.cAria4Sysfiles,3,;
*!*	      "",SET("Datasession"))
    lnFldResult = oAriaApplication.remotesystemdata.execute;
      ("SELECT SYDFIELD.CFLD_NAME,SYDFIELD.CFLD_HEAD,SYDFIELD.CUPGRDLVL FROM SYDFIELD,SYDFLFLD ;
WHERE SYDFIELD.cfld_name = SYDFLFLD.cfld_name AND SYDFLFLD.cfile_nam = '"+PADR('PMPRJHD',oAriaApplication.FileW)+"' ORDER BY SYDFIELD.CFLD_HEAD",'','TempFld',"",oAriaApplication.cAria4Sysfiles,3,;
      "",SET("Datasession"))
    *E303030,1 END

    IF USED('TempFld')
      SELECT 'TempFld'
      SCAN
        SCATTER MEMO MEMVAR
        INSERT INTO (loFormSet.lcTempFld) FROM MEMVAR
      ENDSCAN
    ENDIF


    SELECT(loFormSet.lcTempFld)
    APPEND BLANK
      *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
*!*      REPLACE cfld_name WITH 'cProjStat',;
*!*        cupgrdlvl WITH 'A',;
*!*        cfld_head WITH 'Project Status'
    REPLACE cfld_name WITH 'cProjStat',;
      cupgrdlvl WITH 'A',;
      cfld_head WITH LANG_MFPROJMON_PROJECTSTATUS
      *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]

    APPEND BLANK
      *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
*!*      REPLACE cfld_name WITH 'cProjType',;
*!*        cupgrdlvl WITH 'A',;
*!*        cfld_head WITH 'Project Type'
    REPLACE cfld_name WITH 'cProjType',;
      cupgrdlvl WITH 'A',;
      cfld_head WITH LANG_MFPROJMON_PROJECTTYPE 
      *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
    APPEND BLANK
   *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]    
*!*      REPLACE cfld_name WITH 'ScaleDsc',;
*!*        cupgrdlvl WITH 'A',;
*!*        cfld_head WITH 'Scale Desc.'
    REPLACE cfld_name WITH 'ScaleDsc',;
      cupgrdlvl WITH 'A',;
      cfld_head WITH LANG_MFPROJMON_SCALEDESC
      *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]

    IF !(lcRpPrjTp $ 'SH')

      APPEND BLANK
      *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
*!*        REPLACE cfld_name WITH 'CWARECODE',;
*!*          cupgrdlvl WITH 'A',;
*!*          cfld_head WITH 'Location'
      REPLACE cfld_name WITH 'CWARECODE',;
        cupgrdlvl WITH 'A',;
        cfld_head WITH LANG_MFPROJMON_LOCATION
      *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
      APPEND BLANK
      *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
*!*        REPLACE cfld_name WITH 'COMPLETE2',;
*!*          cupgrdlvl WITH 'A',;
*!*          cfld_head WITH 'PO Comp. Date'
      REPLACE cfld_name WITH 'COMPLETE2',;
        cupgrdlvl WITH 'A',;
        cfld_head WITH LANG_MFPROJMON_POCOMPDATE
      *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]

      APPEND BLANK
      *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
*!*        REPLACE cfld_name WITH 'ENTERED2',;
*!*          cupgrdlvl WITH 'A',;
*!*          cfld_head WITH 'PO Entered Date'
      REPLACE cfld_name WITH 'ENTERED2',;
              cupgrdlvl WITH 'A',;
              cfld_head WITH LANG_MFPROJMON_POENTERED
      *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]

      APPEND BLANK
      *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
*!*        REPLACE cfld_name WITH 'AVIALABLE',;
*!*          cupgrdlvl WITH 'A',;
*!*          cfld_head WITH 'PO Avail. Date'
      REPLACE cfld_name WITH 'AVIALABLE',;
        cupgrdlvl WITH 'A',;
        cfld_head WITH LANG_MFPROJMON_POAVLDATE
      *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]

      APPEND BLANK
      *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
*!*        REPLACE cfld_name WITH 'DESC1',;
*!*          cupgrdlvl WITH 'A',;
*!*          cfld_head WITH 'Style Description'
      REPLACE cfld_name WITH 'DESC1',;
        cupgrdlvl WITH 'A',;
        cfld_head WITH LANG_MFPROJMON_STYDESC  
      *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
      APPEND BLANK
      *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
*!*        REPLACE cfld_name WITH 'SEASON',;
*!*          cupgrdlvl WITH 'A',;
*!*          cfld_head WITH 'Season'
      REPLACE cfld_name WITH 'SEASON',;
              cupgrdlvl WITH 'A',;
              cfld_head WITH LANG_MFPROJMON_SEASON
      *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]

      APPEND BLANK
      *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
*!*        REPLACE cfld_name WITH 'CDIVISION',;
*!*          cupgrdlvl WITH 'A',;
*!*          cfld_head WITH 'Division'
      REPLACE cfld_name WITH 'CDIVISION',;
        cupgrdlvl WITH 'A',;
        cfld_head WITH LANG_MFPROJMON_DIVISION
      *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]

      APPEND BLANK
      *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
*!*        REPLACE cfld_name WITH 'STATUS',;
*!*          cupgrdlvl WITH 'A',;
*!*          cfld_head WITH 'Status'
      REPLACE cfld_name WITH 'STATUS',;
        cupgrdlvl WITH 'A',;
        cfld_head WITH LANG_MFPROJMON_STATUS
      *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
    ENDIF

    IF lcRpPrjTp $ 'POC'
      APPEND BLANK
      *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
*!*        REPLACE cfld_name WITH 'ENTERED1',;
*!*          cupgrdlvl WITH 'A',;
*!*          cfld_head WITH 'SO Entered Date'
      REPLACE cfld_name WITH 'ENTERED1',;
        cupgrdlvl WITH 'A',;
        cfld_head WITH LANG_MFPROJMON_SOENTERED
      *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]

      APPEND BLANK
      *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
*!*        REPLACE cfld_name WITH 'START',;
*!*          cupgrdlvl WITH 'A',;
*!*          cfld_head WITH 'SO Start Date'
      REPLACE cfld_name WITH 'START',;
        cupgrdlvl WITH 'A',;
        cfld_head WITH LANG_MFPROJMON_SOSTART
      *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]

      IF !EMPTY(lcPOProf1)
        APPEND BLANK
        REPLACE cfld_name WITH 'PROFILE11',;
          cupgrdlvl WITH 'A',;
          cfld_head WITH lcPOProf1DESC
      ENDIF

      IF !EMPTY(lcPOProf2)
        APPEND BLANK
        REPLACE cfld_name WITH 'PROFILE12',;
          cupgrdlvl WITH 'A',;
          cfld_head WITH lcPOProf2DESC
      ENDIF

      IF !EMPTY(lcPOProf3)
        APPEND BLANK
        REPLACE cfld_name WITH 'PROFILE13',;
          cupgrdlvl WITH 'A',;
          cfld_head WITH lcPOProf3DESC
      ENDIF

      IF !EMPTY(lcSOProf1)
        APPEND BLANK
        REPLACE cfld_name WITH 'PROFILE1',;
          cupgrdlvl WITH 'A',;
          cfld_head WITH lcSOProf1DESC
      ENDIF

      IF !EMPTY(lcSOProf2)
        APPEND BLANK
        REPLACE cfld_name WITH 'PROFILE2',;
          cupgrdlvl WITH 'A',;
          cfld_head WITH lcSOProf2DESC
      ENDIF

      IF !EMPTY(lcSOProf3)
        APPEND BLANK
        REPLACE cfld_name WITH 'PROFILE3',;
          cupgrdlvl WITH 'A',;
          cfld_head WITH lcSOProf3DESC
      ENDIF

      APPEND BLANK
      *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
*!*        REPLACE cfld_name WITH 'SHIPVIA',;
*!*          cupgrdlvl WITH 'A',;
*!*          cfld_head WITH 'Ship Via'
      REPLACE cfld_name WITH 'SHIPVIA',;
        cupgrdlvl WITH 'A',;
        cfld_head WITH LANG_MFPROJMON_SHIPVIA
      *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
      APPEND BLANK
      *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
*!*        REPLACE cfld_name WITH 'CTERMCODE',;
*!*          cupgrdlvl WITH 'A',;
*!*          cfld_head WITH 'Term Code'
      REPLACE cfld_name WITH 'CTERMCODE',;
        cupgrdlvl WITH 'A',;
        cfld_head WITH LANG_MFPROJMON_TRMCODE
      *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]

      APPEND BLANK
      *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
*!*        REPLACE cfld_name WITH 'SPCINST',;
*!*          cupgrdlvl WITH 'A',;
*!*          cfld_head WITH 'Special Inst.'
      REPLACE cfld_name WITH 'SPCINST',;
        cupgrdlvl WITH 'A',;
        cfld_head WITH LANG_MFPROJMON_SPCINST
      *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]

      APPEND BLANK
      *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
*!*        REPLACE cfld_name WITH 'CPURCODE',;
*!*          cupgrdlvl WITH 'A',;
*!*          cfld_head WITH 'Purchase Group'
      REPLACE cfld_name WITH 'CPURCODE',;
        cupgrdlvl WITH 'A',;
        cfld_head WITH LANG_MFPROJMON_PURGRP  
      *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]

      APPEND BLANK
      *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
*!*        REPLACE cfld_name WITH 'REP1',;
*!*          cupgrdlvl WITH 'A',;
*!*          cfld_head WITH 'Sales Rep. 1'
      REPLACE cfld_name WITH 'REP1',;
        cupgrdlvl WITH 'A',;
        cfld_head WITH LANG_MFPROJMON_SRP1
      *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]

      APPEND BLANK
      *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
*!*        REPLACE cfld_name WITH 'REP2',;
*!*          cupgrdlvl WITH 'A',;
*!*          cfld_head WITH 'Sales Rep. 2'
      REPLACE cfld_name WITH 'REP2',;
        cupgrdlvl WITH 'A',;
        cfld_head WITH LANG_MFPROJMON_SRP2
      *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]


      APPEND BLANK
      *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
*!*        REPLACE cfld_name WITH 'CCURRCODE',;
*!*          cupgrdlvl WITH 'A',;
*!*          cfld_head WITH 'Currency Code'
      REPLACE cfld_name WITH 'CCURRCODE',;
        cupgrdlvl WITH 'A',;
        cfld_head WITH LANG_MFPROJMON_CURRCODE
      *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[ENd]

      APPEND BLANK
      *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
*!*        REPLACE cfld_name WITH 'DEPT',;
*!*          cupgrdlvl WITH 'A',;
*!*          cfld_head WITH 'Department'
      REPLACE cfld_name WITH 'DEPT',;
        cupgrdlvl WITH 'A',;
        cfld_head WITH LANG_MFPROJMON_DEPT
      *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[ENd]

      *! B609028,1 MMT 10/07/2009 Fix bug of not exporting Task notes to excel[Start]
      APPEND BLANK
      *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
*!*        REPLACE cfld_name WITH 'CSTYGROUP',;
*!*          cupgrdlvl WITH 'A',;
*!*          cfld_head WITH 'Style Group'
*!*        APPEND BLANK
*!*        REPLACE cfld_name WITH 'QTY1',;
*!*          cupgrdlvl WITH 'A',;
*!*          cfld_head WITH 'Qty1'
*!*        APPEND BLANK
*!*        REPLACE cfld_name WITH 'QTY2',;
*!*          cupgrdlvl WITH 'A',;
*!*          cfld_head WITH 'Qty2'
*!*        APPEND BLANK
*!*        REPLACE cfld_name WITH 'QTY3',;
*!*          cupgrdlvl WITH 'A',;
*!*          cfld_head WITH 'Qty3'
*!*        APPEND BLANK
*!*        REPLACE cfld_name WITH 'QTY4',;
*!*          cupgrdlvl WITH 'A',;
*!*          cfld_head WITH 'Qty4'
*!*        APPEND BLANK
*!*        REPLACE cfld_name WITH 'QTY5',;
*!*          cupgrdlvl WITH 'A',;
*!*          cfld_head WITH 'Qty5'
*!*        APPEND BLANK
*!*        REPLACE cfld_name WITH 'QTY6',;
*!*          cupgrdlvl WITH 'A',;
*!*          cfld_head WITH 'Qty6'
*!*        APPEND BLANK
*!*        REPLACE cfld_name WITH 'QTY7',;
*!*          cupgrdlvl WITH 'A',;
*!*          cfld_head WITH 'Qty7'
*!*        APPEND BLANK
*!*        REPLACE cfld_name WITH 'QTY8',;
*!*          cupgrdlvl WITH 'A',;
*!*          cfld_head WITH 'Qty8'
*!*        APPEND BLANK
*!*        REPLACE cfld_name WITH 'TOTWIP',;
*!*          cupgrdlvl WITH 'A',;
*!*          cfld_head WITH 'Total Work in process qty'
      REPLACE cfld_name WITH 'CSTYGROUP',;
        cupgrdlvl WITH 'A',;
        cfld_head WITH LANG_MFPROJMON_STYGROUP  
      APPEND BLANK
      REPLACE cfld_name WITH 'QTY1',;
        cupgrdlvl WITH 'A',;
        cfld_head WITH LANG_MFPROJMON_QTY1  
      APPEND BLANK
      REPLACE cfld_name WITH 'QTY2',;
        cupgrdlvl WITH 'A',;
        cfld_head WITH LANG_MFPROJMON_QTY2
      APPEND BLANK
      REPLACE cfld_name WITH 'QTY3',;
        cupgrdlvl WITH 'A',;
        cfld_head WITH LANG_MFPROJMON_QTY3
      APPEND BLANK
      REPLACE cfld_name WITH 'QTY4',;
        cupgrdlvl WITH 'A',;
        cfld_head WITH LANG_MFPROJMON_QTY4
      APPEND BLANK
      REPLACE cfld_name WITH 'QTY5',;
        cupgrdlvl WITH 'A',;
        cfld_head WITH LANG_MFPROJMON_QTY5
      APPEND BLANK
      REPLACE cfld_name WITH 'QTY6',;
        cupgrdlvl WITH 'A',;
        cfld_head WITH LANG_MFPROJMON_QTY6
      APPEND BLANK
      REPLACE cfld_name WITH 'QTY7',;
        cupgrdlvl WITH 'A',;
        cfld_head WITH LANG_MFPROJMON_QTY7
      APPEND BLANK
      REPLACE cfld_name WITH 'QTY8',;
        cupgrdlvl WITH 'A',;
        cfld_head WITH LANG_MFPROJMON_QTY8
      APPEND BLANK
      REPLACE cfld_name WITH 'TOTWIP',;
        cupgrdlvl WITH 'A',;
        cfld_head WITH LANG_MFPROJMON_TWIPOT
      *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[ENd]
      *! B609028,1 MMT 10/07/2009 Fix bug of not exporting Task notes to excel[End]
    ENDIF
  *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [Start]
  ENDIF
  *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [End]

*PADR

  *!*************************************************************
  *! Name      : lfAddTask
  *! Developer : Mariam Mazhar
  *! Date      : 06/01/2009
  *! Purpose   : Add Task to browse screen
  *!*************************************************************
FUNCTION lfAddTask
  PARAMETERS lcIndx

  IF TYPE('oBrowse') <> 'O'
    lcExpRet = '<VFP:CustomObject SourceControl = "cTskSt'+lcIndx+'"'
  ELSE

    *! B609028,1 MMT 10/07/2009 Fix bug of not exporting Task notes to excel[Start]
    *  lcClrBind = 'IIF(EMPTY(dAcFnh&lcIndx) AND nDyb4Cmp>0 AND BETW(DClFnh&lcIndx,lcSDt,'+;
    'lcSDt+nDyb4Cmp),nClb4Cm,IIF(EMPTY(dAcFnh&lcIndx.) AND nDyPstCm1>0 AND BETW(DClFnh&lcIndx,lcSDt-1,'+;
    'lcSDt-nDyPstCm1),nClPstCm1,'+;
    'IIF(EMPTY(dAcFnh&lcIndx) AND nDyPstCm1>0 And DClFnh&lcIndx. < lcSDt-nDyPstCm1,nClrPastCmp2,0)))"'

    lcClrBind = [IIF(EMPT(EVAL(T3+'.dAcFnh&lcIndx')) AND B>0 AND BETW(EVAL(T3+'.DClFnh&lcIndx'),ldSD,]+;
      [ldSD+B),A,IIF(EMPT(EVAL(T3+'.dAcFnh&lcIndx')) AND C>0 AND BETW(EVAL(T3+'.DClFnh&lcIndx'),ldSD-C,]+;
      [ldSD-1),D,]+;
      [IIF(EMPT(EVAL(T3+'.dAcFnh&lcIndx')) AND C>0 And EVAL(T3+'.DClFnh&lcIndx')<ldSD-C,E,0)))"]

    lcExpRet = '<VFP:CustomObject txtBoxDelg ="lfGetNot" ObjectName = "PMPRJCNT"  SourceControl = "&T2..cTskSt'+lcIndx+'" HandlerObject= "loParentForm" Delegate = "lfEdtTask"  DynamicForeColor = "'+lcClrBind+'"'

    IF TYPE('GLOBALBROWSEWINDOW') = 'O'
      IF GLOBALBROWSEWINDOW.Container1.chkColumn.ENABLED = .F.
        GLOBALBROWSEWINDOW.Container1.chkColumn.ENABLED = .T.
        GLOBALBROWSEWINDOW.Custfields = loFormSet.lcTempFld

        *! B609028,2 MMT 10/13/2009 Fix bug of Wrong Order of exporting Task notes to excel[Start]
        BINDEVENT(GLOBALBROWSEWINDOW.Container1.chkExport,'Valid',loParentForm,'lfReOrderBrow')
        BINDEVENT(GLOBALBROWSEWINDOW.AriaGrid1,'restorepreference',loParentForm,'lfRestorePreference',1)
 
        *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [Start]
        GLOBALBROWSEWINDOW.Container1.cmdselect.VISIBLE = .F.
        *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [End]
        *! B609028,2 MMT 10/13/2009 Fix bug of Wrong Order of exporting Task notes to excel[End]
        *  E302694,1 MMT 05/16/2010 Add Shortcut to Project Monitor to complete tasks[Start]
        GLOBALBROWSEWINDOW.AriaGrid1.Column1.Readonly = .F.
        *  E302694,1 MMT 05/16/2010 Add Shortcut to Project Monitor to complete tasks[EnD]
      ENDIF
    ENDIF

  ENDIF

  IF TYPE('GLOBALBROWSEWINDOW.laforecolors[1]') = 'U' OR TYPE('GLOBALBROWSEWINDOW.laforecolors[1]') = 'L'
    IF A <> 0 OR D <> 0 OR E <> 0
      DIMENSION GLOBALBROWSEWINDOW.laforecolors[1]
      STORE '' TO GLOBALBROWSEWINDOW.laforecolors
      *B609386,1 MMT 08/25/2010 Error while export to Excel when same color selected twice[Start]
      *IF A <> 0
      IF A <> 0 AND ASCAN(GLOBALBROWSEWINDOW.laforecolors,A) = 0
      *B609386,1 MMT 08/25/2010 Error while export to Excel when same color selected twice[End]      
        IF EMPTY(GLOBALBROWSEWINDOW.laforecolors[1])
          GLOBALBROWSEWINDOW.laforecolors[1]= A
        ELSE
          DIMENSION GLOBALBROWSEWINDOW.laforecolors[ALEN(GLOBALBROWSEWINDOW.laforecolors,1)+1]
          GLOBALBROWSEWINDOW.laforecolors[ALEN(GLOBALBROWSEWINDOW.laforecolors,1)]= A
        ENDIF
      ENDIF
      *B609386,1 MMT 08/25/2010 Error while export to Excel when same color selected twice[Start]
      *IF D <> 0
      IF D <> 0 AND ASCAN(GLOBALBROWSEWINDOW.laforecolors,D) = 0
      *B609386,1 MMT 08/25/2010 Error while export to Excel when same color selected twice[End]      
        IF EMPTY(GLOBALBROWSEWINDOW.laforecolors[1])
          GLOBALBROWSEWINDOW.laforecolors[1]= D
        ELSE
          DIMENSION GLOBALBROWSEWINDOW.laforecolors[ALEN(GLOBALBROWSEWINDOW.laforecolors,1)+1]
          GLOBALBROWSEWINDOW.laforecolors[ALEN(GLOBALBROWSEWINDOW.laforecolors,1)]= D
        ENDIF
      ENDIF
      *B609386,1 MMT 08/25/2010 Error while export to Excel when same color selected twice[Start]
      *IF E <> 0      
      IF E <> 0 AND ASCAN(GLOBALBROWSEWINDOW.laforecolors,E) = 0
      *B609386,1 MMT 08/25/2010 Error while export to Excel when same color selected twice[End]      
        IF EMPTY(GLOBALBROWSEWINDOW.laforecolors[1])
          GLOBALBROWSEWINDOW.laforecolors[1]= E
        ELSE
          DIMENSION GLOBALBROWSEWINDOW.laforecolors[ALEN(GLOBALBROWSEWINDOW.laforecolors,1)+1]
          GLOBALBROWSEWINDOW.laforecolors[ALEN(GLOBALBROWSEWINDOW.laforecolors,1)]= E
        ENDIF
      ENDIF
    ENDIF
  ENDIF
  *! B609028,1 MMT 10/07/2009 Fix bug of not exporting Task notes to excel[End]
  RETURN lcExpRet


  *!*************************************************************
  *! Name      : lfFiltrTrrn
  *! Developer : Mariam Mazhar
  *! Date      : 06/01/2009
  *! Purpose   : Get Details of Transcation
  *!*************************************************************
FUNCTION lfFiltrTrrn
  IF (lcRpPrjTp $ 'PADNRC')
    DO CASE
      CASE lcRpPrjTp  = 'P'
        =gfSeek('PP'+PmPrjHd.cprj_id,'POSHDR')
      CASE lcRpPrjTp  = 'A'
        =gfSeek('PA'+PmPrjHd.cprj_id,'POSHDR')
      CASE lcRpPrjTp  = 'D'
        =gfSeek('PD'+PmPrjHd.cprj_id,'POSHDR')
      CASE lcRpPrjTp  = 'N'
        =gfSeek('NN'+PmPrjHd.cprj_id,'POSHDR')
      CASE lcRpPrjTp  = 'R'
        =gfSeek('RP'+PmPrjHd.cprj_id,'POSHDR')
      CASE lcRpPrjTp  = 'C'
        =gfSeek('PU'+PmPrjHd.cprj_id,'POSHDR')
    ENDCASE
    IF (lcRpPrjTp $ 'PADNR')
      IF llVendSelected AND !SEEK(POSHDR.VENDOR,lcSelVend)
        RETURN .F.
      ENDIF
      IF lcRpPrjTp  = 'P' AND !EMPTY(lcPCmpDate)
        DO CASE
          CASE ATC("|",lcPCmpDate) = 1
            IF !(POSHDR.COMPLETE=<CTOD(SUBSTR(lcPCmpDate,2)))
              RETURN .F.
            ENDIF
          OTHERWISE
            IF !BETWEEN(POSHDR.COMPLETE,CTOD(SUBSTR(lcPCmpDate,1,10)),CTOD(SUBSTR(lcPCmpDate,12,21)))
              RETURN .F.
            ENDIF
        ENDCASE
      ENDIF
    ENDIF
    m.Partner =  POSHDR.VENDOR
  ENDIF

  DO CASE
    CASE lcRpPrjTp $ 'TO' &&   Sales  Order
      =gfSeek(lcRpPrjTp+PmPrjHd.cprj_id,'Ordhdr','Ordhdr')
      m.Partner = OrdHdr.ACCOUNT
      *! B609091,1 MMT 11/18/2009 Add Order Status Field in Browse Fields[Start]
      m.Status = OrdHdr.STATUS
      *  B609778,1 MMT 12/25/2011 fix bug of not Displaying the Cancelled projects[Start]
*!*        IF m.Status = 'X'
*!*          RETURN .F.
*!*        ENDIF
      *  B609778,1 MMT 12/25/2011 fix bug of not Displaying the Cancelled projects[END]
      *! B609091,1 MMT 11/18/2009 Add Order Status Field in Browse Fields[End]
      IF llAccSelected AND !SEEK(OrdHdr.ACCOUNT,lcSelAcc)
        RETURN .F.
      ENDIF
      IF !EMPTY(lcCmpDate)
        DO CASE
          CASE ATC("|",lcCmpDate) = 1
            IF !(OrdHdr.COMPLETE =< CTOD(SUBSTR(lcCmpDate,2)))
              RETURN .F.
            ENDIF

          OTHERWISE
            IF !EMPTY(lcCmpDate) AND !BETWEEN(OrdHdr.COMPLETE,CTOD(SUBSTR(lcCmpDate,1,10)),CTOD(SUBSTR(lcCmpDate,12,21)))
              RETURN .F.
            ENDIF
        ENDCASE
      ENDIF
      m.ACCOUNT = OrdHdr.ACCOUNT
      m.BTNAme = IIF(gfSeek('M'+OrdHdr.ACCOUNT ,'CUSTOMER','CUSTOMER'),Customer.BTNAme,'')
      m.CUSTPO = OrdHdr.CUSTPO
      SELECT Ordline 
      =gfSeek(lcRpPrjTp+PmPrjHd.cprj_id)
      *B609286,1 MMT 06/07/2010 SO - project monitor screen not working for style filter [Start]
*!*        SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) =lcRpPrjTp+PmPrjHd.cprj_id FOR IIF(SUBSTR(PmPrjHd.cstyle,1,lnmajorlen) = REPLICATE('*',lnmajorlen),.T.,IIF(PmPrjHd.LINENO <> 0,STYLE = PmPrjHd.cstyle,STYLE = ALLT(PmPrjHd.cstyle))) AND ;
*!*              IIF(PmPrjHd.LINENO <> 0,LINENo = PmPrjHd.LINENO,.T.)
      SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) =lcRpPrjTp+PmPrjHd.cprj_id FOR IIF(SUBSTR(PmPrjHd.cstyle,1,lnmajorlen) = REPLICATE('*',lnmajorlen),.T.,IIF(PmPrjHd.LINENO <> 0,STYLE = PmPrjHd.cstyle,STYLE = ALLT(PmPrjHd.cstyle))) AND ;
            IIF(PmPrjHd.LINENO <> 0,LINENo = PmPrjHd.LINENO,.T.) AND IIF(llStyleSelect ,SEEK(SUBSTR(Style,1,lnmajorlen),lcSelSty),.T.)
      *B609286,1 MMT 06/07/2010 SO - project monitor screen not working for style filter [End]        
        IF lcRpPrjTp = 'O' AND !EMPTY(lcPCmpDate)
          RETURN .F.
        ENDIF
        *  B609631,1 MMT 06/26/2011 Project Monitor should display 1 line for style color in Extended Size Scale companies[T20110620.0022][START]
        m.style = ordline.style        
        *  B609631,1 MMT 06/26/2011 Project Monitor should display 1 line for style color in Extended Size Scale companies[T20110620.0022][End]
        m.PO        = ""
        m.Profile11 = ''
        m.Profile12 = ''
        m.Profile13 = ''
        m.AVIALABLE = {}
        m.ENTERED2  = {}
        m.COMPLETE2 = {}
        m.TOTWIP    = 0
        m.CPURCODE  = ''
        m.Partner = ''    
        =gfSeek(ordline.STYLE,'STYLE')
        =gfSeek('S'+STYLE.SCALE,'SCALE')
        m.ScaleDsc  = SCALE.cscl_desc

        m.Desc1     = ordline.Desc1
        IF !EMPTY(ordline.STORE)
          m.Store = ordline.STORE
        ENDIF
        m.COMPLETE1 = ordline.COMPLETE
        m.ENTERED1  = OrdHdr.ENTERED
        m.START     = ordline.START
        m.TOTWIP    = STYLE.TOTWIP
        m.CSTYGROUP = STYLE.CSTYGROUP
        m.SEASON    = ordline.SEASON
        m.CDivision = OrdHdr.CDivision
        m.SHIPVIA   = OrdHdr.SHIPVIA
        m.CTERMCODE = OrdHdr.CTERMCODE
        m.SPCINST   = OrdHdr.SPCINST
        m.CPURCODE  = POSHDR.CPURCODE
        m.REP1      = OrdHdr.REP1
        m.REP2      = OrdHdr.REP2
        m.CWARECODE = OrdHdr.CWARECODE
        m.CCURRCODE = OrdHdr.CCURRCODE
        m.DEPT      = OrdHdr.DEPT
        m.PRICE   = 0
        m.TOTQTY  = 0
        m.Qty1 = 0
        m.Qty2 = 0
        m.Qty3 = 0
        m.Qty4 = 0
        m.Qty5 = 0
        m.Qty6 = 0
        m.Qty7 = 0
        m.Qty8 = 0

        
        IF gfSeek("SO"+lcRpPrjTp+ordline.ORDER+STR(ordline.LINENO,6),"PROFVALU")
          lnPrfCnt = 1
          SELECT PROFVALU
          SCAN REST WHILE cpro_type+ckey+cpro_code = "SO"+lcRpPrjTp +ordline.ORDER+STR(ordline.LINENO,6)
            lcPrfCnt = STR(lnPrfCnt,1)
            m.Profile&lcPrfCnt. = PROFVALU.cpro_value
            lnPrfCnt = lnPrfCnt + 1
            IF lnPrfCnt > 3
              EXIT
            ENDIF
          ENDSCAN
        ENDIF

        IF lcRpPrjTp = 'O' AND gfSeek('2'+PmPrjHd.cprj_id+STR(ORDLINE.LINENO,6),'CUTPICK','CUTORD')

          SELECT CUTPICK
          SCAN REST WHILE trancd+ORDER+cordline = '2'+PmPrjHd.cprj_id+STR(ORDLINE.LINENO,6) ;
              FOR IIF(SUBSTR(PmPrjHd.cstyle,1,lnmajorlen) = REPLICATE('*',lnmajorlen),.T.,IIF(PmPrjHd.LINENO <> 0,STYLE = PmPrjHd.cstyle,STYLE = ALLT(PmPrjHd.cstyle))) 
            =gfSeek('PP'+CUTPICK.CTKTNO,'POSHDR')
            
            *! B609028,1 MMT 10/07/2009 Fix bug of not exporting Task notes to excel[Start]
            IF !EMPTY(laPoCustFld[1,1])
              FOR lnT = 1 TO ALEN(laPoCustFld,1)
                lcFldName = laPoCustFld[lnT,1]
                m.&lcFldName. = POSHDR.&lcFldName.
              ENDFOR
            ENDIF
            *! B609028,1 MMT 10/07/2009 Fix bug of not exporting Task notes to excel[End]

            IF !EMPTY(lcPCmpDate)
              DO CASE
                CASE ATC("|",lcPCmpDate) = 1
                  IF !(POSHDR.COMPLETE =< CTOD(SUBSTR(lcPCmpDate,2)))
                    LOOP
                  ENDIF

                OTHERWISE
                  IF !EMPTY(lcPCmpDate)AND !BETWEEN(POSHDR.COMPLETE,CTOD(SUBSTR(lcPCmpDate,1,10)),CTOD(SUBSTR(lcPCmpDate,12,21)))
                    LOOP
                  ENDIF
              ENDCASE
            ENDIF
            m.Profile1 = ''
            m.Profile2 = ''
            m.Profile3 = ''
            m.Profile12= ''
            m.Profile11= ''
            m.Profile13= ''

            m.Style     = CUTPICK.STYLE
            m.PO        = CUTPICK.CTKTNO
            m.Store = OrdHdr.STORE
            m.AVIALABLE = POSHDR.AVAiLABLE
            m.ENTERED2  = POSHDR.ENTERED
            m.COMPLETE2 = POSHDR.COMPLETE
            m.Partner =  POSHDR.VENDOR

            *! B609028,1 MMT 10/07/2009 Fix bug of not exporting Task notes to excel[Start]
            *!*          m.PRICE     = ordline.PRICE
            *!*          m.TOTQTY    = ordline.TOTQTY
            *!*          m.TOTWIP    =
            =gfSeek('PP'+CUTPICK.CTKTNO+'0001' + CUTPICK.STYLE + PADR(CUTPICK.CTKTLINENO,6)+'1','POSLN','POSLN')
            m.TOTQTY    = CUTPICK.TOTQTY
            m.PRICE     = POSLN.NiCOST1
            m.Qty1 = CUTPICK.Qty1
            m.Qty2 = CUTPICK.Qty2
            m.Qty3 = CUTPICK.Qty3
            m.Qty4 = CUTPICK.Qty4
            m.Qty5 = CUTPICK.Qty5
            m.Qty6 = CUTPICK.Qty6
            m.Qty7 = CUTPICK.Qty7
            m.Qty8 = CUTPICK.Qty8
            *! B609028,1 MMT 10/07/2009 Fix bug of not exporting Task notes to excel[End]

            IF gfSeek("SOO"+PmPrjHd.cprj_id+STR(ordline.LINENO,6),"PROFVALU") AND (!EMPTY(lcSOProf1) OR !EMPTY(lcSOProf2) OR !EMPTY(lcSOProf3))
              SELECT PROFVALU
              lnPrfCnt = 1
              SCAN REST WHILE cpro_type+ckey+cpro_code = "SOO"+PmPrjHd.cprj_id+STR(ordline.LINENO,6) FOR ;
                  INLIST(cpro_code,lcSOProf1,lcSOProf2,lcSOProf3)
                DO CASE
                  CASE cpro_code = lcSOProf1
                    m.Profile1 = PROFVALU.cpro_value
                  CASE cpro_code = lcSOProf2
                    m.Profile2 = PROFVALU.cpro_value
                  CASE cpro_code = lcSOProf3
                    m.Profile3 = PROFVALU.cpro_value
                ENDCASE
              ENDSCAN
            ENDIF
            IF gfSeek("POPP"+CUTPICK.CTKTNO+CUTPICK.CTKTLINENO,"PROFVALU") AND (!EMPTY(lcPOProf1) OR !EMPTY(lcPOProf2) OR !EMPTY(lcPOProf3))
              SELECT PROFVALU
              lnPrfCnt = 1
              SCAN REST WHILE cpro_type+ckey+cpro_code = "POPP"+CUTPICK.CTKTNO+CUTPICK.CTKTLINENO FOR ;
                  INLIST(cpro_code,lcPOProf1,lcPOProf2,lcPOProf3)
                DO CASE
                  CASE cpro_code = lcPOProf1
                    m.Profile11 = PROFVALU.cpro_value
                  CASE cpro_code = lcPOProf2
                    m.Profile12 = PROFVALU.cpro_value
                  CASE cpro_code = lcPOProf3
                    m.Profile13 = PROFVALU.cpro_value
                ENDCASE
              ENDSCAN
            ENDIF
            
          ENDSCAN
        ENDIF 
        
        
         IF lcRpPrjTp = 'O' AND gfSeek('1'+PmPrjHd.cprj_id+STR(ORDLINE.LINENO,6),'CUTPICK','CUTORD')

          SELECT CUTPICK
          SCAN REST WHILE trancd+ORDER+cordline = '1'+PmPrjHd.cprj_id+STR(ORDLINE.LINENO,6) ;
              FOR IIF(SUBSTR(PmPrjHd.cstyle,1,lnmajorlen) = REPLICATE('*',lnmajorlen),.T.,IIF(PmPrjHd.LINENO <> 0,STYLE = PmPrjHd.cstyle,STYLE = ALLT(PmPrjHd.cstyle))) 
            =gfSeek('PU'+CUTPICK.CTKTNO,'POSHDR')
            
            *! B609028,1 MMT 10/07/2009 Fix bug of not exporting Task notes to excel[Start]
            IF !EMPTY(laPoCustFld[1,1])
              FOR lnT = 1 TO ALEN(laPoCustFld,1)
                lcFldName = laPoCustFld[lnT,1]
                m.&lcFldName. = POSHDR.&lcFldName.
              ENDFOR
            ENDIF
            *! B609028,1 MMT 10/07/2009 Fix bug of not exporting Task notes to excel[End]

            IF !EMPTY(lcPCmpDate)
              DO CASE
                CASE ATC("|",lcPCmpDate) = 1
                  IF !(POSHDR.COMPLETE =< CTOD(SUBSTR(lcPCmpDate,2)))
                    LOOP
                  ENDIF

                OTHERWISE
                  IF !EMPTY(lcPCmpDate)AND !BETWEEN(POSHDR.COMPLETE,CTOD(SUBSTR(lcPCmpDate,1,10)),CTOD(SUBSTR(lcPCmpDate,12,21)))
                    LOOP
                  ENDIF
              ENDCASE
            ENDIF
            m.Profile1 = ''
            m.Profile2 = ''
            m.Profile3 = ''
            m.Profile12= ''
            m.Profile11= ''
            m.Profile13= ''

            m.Style     = CUTPICK.STYLE
            m.PO        = CUTPICK.CTKTNO
            m.Store = OrdHdr.STORE
            m.AVIALABLE = POSHDR.AVAiLABLE
            m.ENTERED2  = POSHDR.ENTERED
            m.COMPLETE2 = POSHDR.COMPLETE
            m.Partner =  POSHDR.VENDOR

            *! B609028,1 MMT 10/07/2009 Fix bug of not exporting Task notes to excel[Start]
            *!*          m.PRICE     = ordline.PRICE
            *!*          m.TOTQTY    = ordline.TOTQTY
            *!*          m.TOTWIP    =
            =gfSeek('PU'+CUTPICK.CTKTNO+'0001' + CUTPICK.STYLE + PADR(CUTPICK.CTKTLINENO,6)+'1','POSLN','POSLN')
            m.TOTQTY    = CUTPICK.TOTQTY
            m.PRICE     = POSLN.NiCOST1
            m.Qty1 = CUTPICK.Qty1
            m.Qty2 = CUTPICK.Qty2
            m.Qty3 = CUTPICK.Qty3
            m.Qty4 = CUTPICK.Qty4
            m.Qty5 = CUTPICK.Qty5
            m.Qty6 = CUTPICK.Qty6
            m.Qty7 = CUTPICK.Qty7
            m.Qty8 = CUTPICK.Qty8
            *! B609028,1 MMT 10/07/2009 Fix bug of not exporting Task notes to excel[End]

            IF gfSeek("SOO"+PmPrjHd.cprj_id+STR(ordline.LINENO,6),"PROFVALU") AND (!EMPTY(lcSOProf1) OR !EMPTY(lcSOProf2) OR !EMPTY(lcSOProf3))
              SELECT PROFVALU
              lnPrfCnt = 1
              SCAN REST WHILE cpro_type+ckey+cpro_code = "SOO"+PmPrjHd.cprj_id+STR(ordline.LINENO,6) FOR ;
                  INLIST(cpro_code,lcSOProf1,lcSOProf2,lcSOProf3)
                DO CASE
                  CASE cpro_code = lcSOProf1
                    m.Profile1 = PROFVALU.cpro_value
                  CASE cpro_code = lcSOProf2
                    m.Profile2 = PROFVALU.cpro_value
                  CASE cpro_code = lcSOProf3
                    m.Profile3 = PROFVALU.cpro_value
                ENDCASE
              ENDSCAN
            ENDIF
            IF gfSeek("CTPU"+CUTPICK.CTKTNO+CUTPICK.CTKTLINENO,"PROFVALU") AND (!EMPTY(lcPOProf1) OR !EMPTY(lcPOProf2) OR !EMPTY(lcPOProf3))
              SELECT PROFVALU
              lnPrfCnt = 1
              SCAN REST WHILE cpro_type+ckey+cpro_code = "CTPU"+CUTPICK.CTKTNO+CUTPICK.CTKTLINENO FOR ;
                  INLIST(cpro_code,lcPOProf1,lcPOProf2,lcPOProf3)
                DO CASE
                  CASE cpro_code = lcPOProf1
                    m.Profile11 = PROFVALU.cpro_value
                  CASE cpro_code = lcPOProf2
                    m.Profile12 = PROFVALU.cpro_value
                  CASE cpro_code = lcPOProf3
                    m.Profile13 = PROFVALU.cpro_value
                ENDCASE
              ENDSCAN
            ENDIF
            
          ENDSCAN
        ENDIF 
        
        
        llTaskFnd = .F.
        FOR lnI =1 TO lnTaskCnt
          lcI = ALLTRIM(STR(lnI))
          m.cOprID&lcI.= ''
          m.cOpDsc&lcI.= ''
          m.COPCTG&lcI.= ''
          m.cTskSt&lcI.= ''
          m.dAcFnh&lcI.= {}
          m.DClFnh&lcI.= {}
          m.mOPCOM&lcI. = ""
        ENDFOR
        lfGetTasks()
        IF llTaskFnd
          INSERT INTO (.lcTempFile) FROM MEMVAR
          INSERT INTO (.lcTempFile2) FROM MEMVAR
          INSERT INTO (.lcTempFile3) FROM MEMVAR
        ENDIF
      *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [Start]
      ENDSCAN 
      *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [End]
      
      
      
    CASE lcRpPrjTp $ 'NADRPC' &&   PO or Cut Tkt
      *! B609091,1 MMT 11/18/2009 Add Order Status Field in Browse Fields[Start]
      m.Status = POSHDR.STATUS
      *  B609778,1 MMT 12/25/2011 fix bug of not Displaying the Cancelled projects[Start]
*!*        IF m.Status = 'X'
*!*          RETURN .F.
*!*        ENDIF
      *  B609778,1 MMT 12/25/2011 fix bug of not Displaying the Cancelled projects[END]      
      *! B609091,1 MMT 11/18/2009 Add Order Status Field in Browse Fields[End]
      lcSeekExp = IIF(lcRpPrjTp  = 'P','2','1')+PmPrjHd.cprj_id
      =gfSeek(POSHDR.CBUSDOCU+POSHDR.Cstytype+POSHDR.PO,'POSLN')
      SELECT POSLN
      *B609286,1 MMT 06/07/2010 SO - project monitor screen not working for style filter [Start]      
*!*        SCAN REST WHILE CBUSDOCU+Cstytype+PO+CINVTYPE+STYLE+STR(LINENO,6)+trancd = POSHDR.CBUSDOCU+POSHDR.Cstytype+POSHDR.PO ;
*!*                  FOR trancd = '1' AND IIF(SUBSTR(PmPrjHd.cstyle,1,lnmajorlen)= REPLICATE('*',lnmajorlen),.T.,IIF(PmPrjHd.LINENO <> 0,STYLE = PmPrjHd.cstyle,STYLE = ALLTRIM(PmPrjHd.cstyle))) AND ;
*!*                  IIF(PmPrjHd.LINENO <> 0,LINENO = PmPrjHd.LINENO,.T.) 
      SCAN REST WHILE CBUSDOCU+Cstytype+PO+CINVTYPE+STYLE+STR(LINENO,6)+trancd = POSHDR.CBUSDOCU+POSHDR.Cstytype+POSHDR.PO ;
                FOR trancd = '1' AND IIF(SUBSTR(PmPrjHd.cstyle,1,lnmajorlen)= REPLICATE('*',lnmajorlen),.T.,IIF(PmPrjHd.LINENO <> 0,STYLE = PmPrjHd.cstyle,STYLE = ALLTRIM(PmPrjHd.cstyle))) AND ;
                IIF(PmPrjHd.LINENO <> 0,LINENO = PmPrjHd.LINENO,.T.) AND IIF(llStyleSelect ,SEEK(SUBSTR(Style,1,lnmajorlen),lcSelSty),.T.)
      *B609286,1 MMT 06/07/2010 SO - project monitor screen not working for style filter [End]      
        m.Style     = POSLN.STYLE
        m.Order = ''
        m.Store = ''
        m.PO        = POSLN.PO
        m.AVIALABLE =POSHDR.AVAiLABLE
        m.ENTERED2  = POSHDR.ENTERED
        m.COMPLETE2 = POSLN.COMPLETE
        m.ACCOUNT   = ""
        m.BTNAme    = ""
        m.CUSTPO    = ""
        m.Desc1     = ""
        m.COMPLETE1 = {}
        m.ENTERED1  = {}
        m.START     = {}
        =gfSeek(POSLN.STYLE,'Style','Style')
        m.TOTQTY    = POSLN.TOTQTY
        m.TOTWIP    = STYLE.TOTWIP
        m.PRICE     = POSLN.NiCOST1
        m.CSTYGROUP = STYLE.CSTYGROUP
        m.Qty1 = POSLN.Qty1
        m.Qty2 = POSLN.Qty2
        m.Qty3 = POSLN.Qty3
        m.Qty4 = POSLN.Qty4
        m.Qty5 = POSLN.Qty5
        m.Qty6 = POSLN.Qty6
        m.Qty7 = POSLN.Qty7
        m.Qty8 = POSLN.Qty8
        m.Profile1  = ''
        m.Profile2  = ''
        m.Profile3  = ''

        IF lcRpPrjTp ='P' AND gfSeek("POPP"+POSLN.PO+STR(POSLN.LINENO,6),"PROFVALU") AND;
            (!EMPTY(lcPOProf1) OR !EMPTY(lcPOProf2) OR !EMPTY(lcPOProf3))
          SELECT PROFVALU
          lnPrfCnt = 1
          SCAN REST WHILE cpro_type+ckey+cpro_code = "POPP"+POSLN.PO+STR(POSLN.LINENO,6) FOR ;
              INLIST(cpro_code,lcPOProf1,lcPOProf2,lcPOProf3)
            DO CASE
              CASE cpro_code = lcPOProf1
                m.Profile11 = PROFVALU.cpro_value
              CASE cpro_code = lcPOProf2
                m.Profile12 = PROFVALU.cpro_value
              CASE cpro_code = lcPOProf3
                m.Profile13 = PROFVALU.cpro_value
            ENDCASE
          ENDSCAN
        ELSE
          m.Profile11 = ''
          m.Profile12 = ''
          m.Profile13 = ''
        ENDIF
        m.SEASON    = ""
        m.CDivision = POSHDR.CDivision
        m.SHIPVIA   = POSHDR.SHIPVIA
        m.CTERMCODE = POSHDR.CTERMCODE
        m.SPCINST   = ""
        m.CPURCODE  = POSHDR.CPURCODE
        m.REP1      = ""
        m.REP2      = ""
        m.CWARECODE = POSHDR.CWARECODE
        m.CCURRCODE = POSHDR.CPRICECUR
        m.DEPT      = ""

        =gfSeek(POSLN.STYLE,'STYLE')
        =gfSeek('S'+STYLE.SCALE,'SCALE')
        m.ScaleDsc  = SCALE.cscl_desc
        m.Desc1     = STYLE.Desc1
        m.COMPLETE1 = {}
        m.ENTERED1  = {}
        m.START     = {}
        m.CSTYGROUP = STYLE.CSTYGROUP
        m.PRICE  = POSLN.NiCOST1
        m.Qty1 = POSLN.Qty1
        m.Qty2 = POSLN.Qty2
        m.Qty3 = POSLN.Qty3
        m.Qty4 = POSLN.Qty4
        m.Qty5 = POSLN.Qty5
        m.Qty6 = POSLN.Qty6
        m.Qty7 = POSLN.Qty7
        m.Qty8 = POSLN.Qty8
        m.TOTQTY = POSLN.TOTQTY
        m.TOTWIP = STYLE.TOTWIP
        m.Style     = POSLN.STYLE
        llTaskFnd   = .F.
  
        IF gfSeek(lcSeekExp,'CUTPICK') AND !(lcRpPrjTp $ 'NADR')
          SELECT CUTPICK
          *  B609631,1 MMT 06/26/2011 Project Monitor should display 1 line for style color in Extended Size Scale companies[T20110620.0022][Start]
*!*            SCAN REST WHILE trancd+CTKTNO+STYLE  = lcSeekExp FOR ;
*!*                 IIF(SUBSTR(PmPrjHd.cstyle,1,lnmajorlen) = REPLICATE('*',lnmajorlen),.T.,IIF(PmPrjHd.LINENO <> 0,STYLE = PmPrjHd.cstyle,STYLE = ALLTRIM(PmPrjHd.cstyle))) AND ;
*!*                 IIF(PmPrjHd.LINENO <> 0,CTKTLINENO = STR(PmPrjHd.LINENO,6),.T.)
          SCAN REST WHILE trancd+CTKTNO+STYLE  = lcSeekExp FOR CTKTLINENO = STR(POSLN.LINENO,6)
          *  B609631,1 MMT 06/26/2011 Project Monitor should display 1 line for style color in Extended Size Scale companies[T20110620.0022][End]
    
            =gfSeek('O'+CUTPICK.ORDER,'ORDHDR')
            m.Order = CUTPICK.ORDER
            m.Store = OrdHdr.STORE
            m.ACCOUNT   = OrdHdr.ACCOUNT
            m.BTNAme    = IIF(gfSeek('M'+OrdHdr.ACCOUNT ,'CUSTOMER','CUSTOMER'),Customer.BTNAme,'')
            m.CUSTPO    = OrdHdr.CUSTPO
            m.Style     = CUTPICK.STYLE
            m.PO        = CUTPICK.CTKTNO
            m.AVIALABLE = POSHDR.AVAiLABLE
            m.ENTERED2  = POSHDR.ENTERED
            m.COMPLETE2 = POSHDR.COMPLETE
            =gfSeek('O'+OrdHdr.ORDER+CUTPICK.cordline,'ORDLINE','ORDLINE')
            m.Desc1     = ordline.Desc1
            IF !EMPTY(ordline.STORE)
              m.Store = ordline.STORE
            ENDIF
            m.COMPLETE1 = ordline.COMPLETE
            m.ENTERED1  = OrdHdr.ENTERED
            m.START     = ordline.START
            *  B609631,1 MMT 06/26/2011 Project Monitor should display 1 line for style color in Extended Size Scale companies[T20110620.0022][Start]
            *=gfSeek(IIF(lcRpPrjTp  = 'P','PP','PU')+CUTPICK.CTKTNO+'0001' + CUTPICK.STYLE + PADR(CUTPICK.CTKTLINENO,6)+'1','POSLN','POSLN')
            *  B609631,1 MMT 06/26/2011 Project Monitor should display 1 line for style color in Extended Size Scale companies[T20110620.0022][End]
            m.PRICE     = POSLN.NiCOST1
            m.TOTQTY    = CUTPICK.TOTQTY
            m.TOTWIP    = STYLE.TOTWIP
            m.Qty1 = CUTPICK.Qty1
            m.Qty2 = CUTPICK.Qty2
            m.Qty3 = CUTPICK.Qty3
            m.Qty4 = CUTPICK.Qty4
            m.Qty5 = CUTPICK.Qty5
            m.Qty6 = CUTPICK.Qty6
            m.Qty7 = CUTPICK.Qty7
            m.Qty8 = CUTPICK.Qty8
            m.CSTYGROUP = STYLE.CSTYGROUP
            m.Profile11 = ''
            m.Profile12 = ''
            m.Profile13 = ''
            IF gfSeek("SOO"+ordline.ORDER+STR(ordline.LINENO,6),"PROFVALU") AND (!EMPTY(lcSOProf1) OR !EMPTY(lcSOProf2) OR !EMPTY(lcSOProf3))
              SELECT PROFVALU
              lnPrfCnt = 1
              SCAN REST WHILE cpro_type+ckey+cpro_code = "SOO"+ordline.ORDER+STR(ordline.LINENO,6) FOR ;
                   INLIST(cpro_code,lcSOProf1,lcSOProf2,lcSOProf3)
                DO CASE
                  CASE cpro_code = lcSOProf1
                    m.Profile1 = PROFVALU.cpro_value
                  CASE cpro_code = lcSOProf2
                    m.Profile2 = PROFVALU.cpro_value
                  CASE cpro_code = lcSOProf3
                    m.Profile3 = PROFVALU.cpro_value
                ENDCASE
              ENDSCAN
            ENDIF
            IF lcRpPrjTp ='P' AND gfSeek("POPP"+CUTPICK.CTKTNO+CUTPICK.CTKTLINENO,"PROFVALU") AND;
               (!EMPTY(lcPOProf1) OR !EMPTY(lcPOProf2) OR !EMPTY(lcPOProf3))
              SELECT PROFVALU
              lnPrfCnt = 1
              SCAN REST WHILE cpro_type+ckey+cpro_code = "POPP"+CUTPICK.CTKTNO+CUTPICK.CTKTLINENO FOR ;
                  INLIST(cpro_code,lcPOProf1,lcPOProf2,lcPOProf3)
                DO CASE
                  CASE cpro_code = lcPOProf1
                    m.Profile11 = PROFVALU.cpro_value
                  CASE cpro_code = lcPOProf2
                    m.Profile12 = PROFVALU.cpro_value
                  CASE cpro_code = lcPOProf3
                    m.Profile13 = PROFVALU.cpro_value
                ENDCASE
              ENDSCAN
            ENDIF
            m.SEASON    = ordline.SEASON
            m.CDivision = OrdHdr.CDivision
            m.SHIPVIA   = OrdHdr.SHIPVIA
            m.CTERMCODE = OrdHdr.CTERMCODE
            m.SPCINST   = OrdHdr.SPCINST
            m.CPURCODE  = POSHDR.CPURCODE
            m.REP1      = OrdHdr.REP1
            m.REP2      = OrdHdr.REP2
            m.CWARECODE = OrdHdr.CWARECODE
            m.CCURRCODE = OrdHdr.CCURRCODE
            m.DEPT      = OrdHdr.DEPT
            llTaskFnd = .F.
            =gfSeek(ordline.STYLE,'STYLE')
            =gfSeek('S'+STYLE.SCALE,'SCALE')
            m.ScaleDsc  = SCALE.cscl_desc
         ENDSCAN
        ENDIF         
        FOR lnI =1 TO lnTaskCnt
          lcI = ALLTRIM(STR(lnI))
          m.cOprID&lcI.= ''
          m.cOpDsc&lcI.= ''
          m.COPCTG&lcI.= ''
          m.cTskSt&lcI.= ''
          m.dAcFnh&lcI.= {}
          m.DClFnh&lcI.= {}
          m.mOPCOM&lcI. = ""
        ENDFOR

        lfGetTasks()
        IF llTaskFnd
          INSERT INTO (.lcTempFile) FROM MEMVAR
          INSERT INTO (.lcTempFile2) FROM MEMVAR
          INSERT INTO (.lcTempFile3) FROM MEMVAR
        ENDIF
      ENDSCAN   
  ENDCASE


  *!*************************************************************
  *! Name      : lfGetTasks
  *! Developer : Mariam Mazhar
  *! Date      : 06/01/2009
  *! Purpose   : Get Tasks of Project
  *!*************************************************************
FUNCTION lfGetTasks

  SELECT PMPRJDT
  gfSetOrder('PMPRJDTS')
  
  *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [Start]
  *IF gfSeek(PmPrjHd.cprj_typ+PmPrjHd.cprj_id+PmPrjHd.cstyle,'PMPRJDT')
  IF gfSeek(PmPrjHd.cprj_typ+PmPrjHd.cprj_id+PmPrjHd.cstyle+STR(PmPrjHd.LINENO,6),'PMPRJDT')
  *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [End]
    SELECT PMPRJDT
    
    *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [Start]
    *!*    SCAN REST WHILE CPRJ_TYP+CPRJ_ID+CSTYLE+COPRT_CTG+COPRT_ID = PmPrjHd.cprj_typ+PmPrjHd.cprj_id+PmPrjHd.cstyle;
    *!*         FOR IIF(llTskSelected,SEEK(COPRT_CTG+'-'+COPRT_ID,lcTskSel),.T.) AND IIF(lcRpRprt = 'C',!lshw2cust,.T.)
    SCAN REST WHILE cprj_typ+cprj_id+cstyle+STR(LINENO,6)+COPRT_CTG+COPRT_ID = PmPrjHd.cprj_typ+PmPrjHd.cprj_id+PmPrjHd.cstyle+STR(PmPrjHd.LINENO,6);
        FOR IIF(llTskSelected,SEEK(COPRT_CTG+'-'+COPRT_ID,lcTskSel),.T.) AND IIF(lcRpRprt = 'C',!lshw2cust,.T.)
    *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [End]
      IF llNtfySelected
        *B609386,1 MMT 08/25/2010 When user select Notify user from OG it got not record to display[T20100817.0020][Start]
        *IF gfSeek(PMPRJDT.cprj_typ+PMPRJDT.cprj_id+PMPRJDT.cstyle+PMPRJDT.COPRT_CTG+PMPRJDT.COPRT_ID,'PMPRJNTF')        
        IF gfSeek(PMPRJDT.cprj_typ+PMPRJDT.cprj_id+PMPRJDT.cstyle+STR(PMPRJDT.LINENO,6)+PMPRJDT.COPRT_CTG+PMPRJDT.COPRT_ID,'PMPRJNTF')
        *B609386,1 MMT 08/25/2010 When user select Notify user from OG it got not record to display[T20100817.0020][End]        
          llUsrFound = .F.
          SELECT PMPRJNTF
          *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [Start]
          *!*          SCAN REST WHILE CPRJ_TYP+CPRJ_ID+CSTYLE+COPRT_CTG+COPRT_ID+CUSER_ID = ;
          *!*                          PMPRJDT.CPRJ_TYP+PMPRJDT.CPRJ_ID+PMPRJDT.CSTYLE+PMPRJDT.COPRT_CTG+PMPRJDT.COPRT_ID ;
          *!*                          FOR  SEEK(CUSER_ID ,lcNTFSel)
          SCAN REST WHILE cprj_typ+cprj_id+cstyle+STR(LINENO,6)+COPRT_CTG+COPRT_ID+cUser_Id = ;
              PMPRJDT.cprj_typ+PMPRJDT.cprj_id+PMPRJDT.cstyle+STR(PMPRJDT.LINENO,6)+PMPRJDT.COPRT_CTG+PMPRJDT.COPRT_ID ;
              FOR  SEEK(cUser_Id ,lcNTFSel)
          *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [End]
            llUsrFound = .T.
          ENDSCAN
          IF !llUsrFound
            SELECT PMPRJDT
            LOOP
          ENDIF
        ELSE
          LOOP
        ENDIF
      ENDIF
      SELECT SYSCHDUL
      
      *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [Start]
      *IF gfSEEK(SUBSTR(PMPRJDT.CPRJ_TYP,1,1) + SUBSTR(PMPRJDT.CPRJ_ID,1,6) + SUBSTR(PMPRJDT.CSTYLE,1,12) +PMPRJDT.cOprt_Ctg+PMPRJDT.cOprt_ID)
      IF gfSeek(SUBSTR(PMPRJDT.cprj_typ,1,1) + SUBSTR(PMPRJDT.cprj_id,1,6) + ;
          SUBSTR(PMPRJDT.cstyle,1,19)+STR(PMPRJDT.LINENO,6)+PMPRJDT.COPRT_CTG+PMPRJDT.COPRT_ID)
      *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [End]
        *! B609555,1 MMT 03/27/2011 Task List screen gives error while completing task[Start]
        IF COPERSTAT = 'C'
          LOCATE REST WHILE CCONTTYPE+CSEQNUMBER+CSTYLE+STR(LINENO,6)+CCONT_ID+COPERSTAT+CUSER_ID=SUBSTR(PMPRJDT.cprj_typ,1,1) + SUBSTR(PMPRJDT.cprj_id,1,6) + ;
            SUBSTR(PMPRJDT.cstyle,1,19)+STR(PMPRJDT.LINENO,6)+PMPRJDT.COPRT_CTG+PMPRJDT.COPRT_ID FOR COPERSTAT <> 'C'
          IF !FOUND()
            =gfSeek(SUBSTR(PMPRJDT.cprj_typ,1,1) + SUBSTR(PMPRJDT.cprj_id,1,6) + ;
                    SUBSTR(PMPRJDT.cstyle,1,19)+STR(PMPRJDT.LINENO,6)+PMPRJDT.COPRT_CTG+PMPRJDT.COPRT_ID)
          ENDIF  
        ENDIF 
        *! B609555,1 MMT 03/27/2011 Task List screen gives error while completing task[End]
        IF lcRpOprStt $ 'OC'
          IF SYSCHDUL.COPERSTAT <> lcRpOprStt
            SELECT PMPRJDT
            LOOP
          ENDIF
        ELSE
          IF lcRpOprStt = 'L'
            *! B609028,2 MMT 10/13/2009 Fix bug of Wrong Order of exporting Task notes to excel[Start]
            *!*            IF SYSCHDUL.COPERSTAT <> 'C' OR EMPTY(PMPRJDT.DCLC_FNSH) OR  EMPTY(PMPRJDT.DACT_FNSH);
            *!*                OR (PMPRJDT.DACT_FNSH =< PMPRJDT.DCLC_FNSH)
            IF EMPTY(PMPRJDT.DCLC_FNSH) OR (!EMPTY(PMPRJDT.dAct_Fnsh) AND (PMPRJDT.dAct_Fnsh =< PMPRJDT.DCLC_FNSH)) OR ;
                (SYSCHDUL.COPERSTAT <> 'C' AND EMPTY(PMPRJDT.dAct_Fnsh) AND (oAriaApplication.SystemDate =< PMPRJDT.DCLC_FNSH))
              *! B609028,2 MMT 10/13/2009 Fix bug of Wrong Order of exporting Task notes to excel[End]
              SELECT PMPRJDT
              LOOP
            ENDIF
          ENDIF
        ENDIF

        *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
        *IF lcRpOprStt = 'O' AND LNRPDYCMP <> 0 AND (EMPTY(PMPRJDT.DCLC_FNSH) OR (PMPRJDT.DCLC_FNSH <> oAriaApplication.SystemDate + LNRPDYCMP))
        IF lcRpOprStt = 'O' AND LNRPDYCMP <> 0 AND (EMPTY(PMPRJDT.DCLC_FNSH) OR (PMPRJDT.DCLC_FNSH > oAriaApplication.SystemDate + LNRPDYCMP))                
        *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
          LOOP
        ENDIF




        llTaskFnd = .T.
        IF lcRpPrjTp = 'O' AND llRpShWDy AND PMPRJDT.COPRT_CTG+"-"+PMPRJDT.COPRT_ID = lcRpTaskNom
          lnDateDiff = OrdHdr.COMPLETE - PMPRJDT.DCLC_FNSH
          m.nTaskStat = lnDateDiff
          *! B609028,1 MMT 10/07/2009 Fix bug of not exporting Task notes to excel[Start]
          lcNomDesc =  PMPRJDT.COPRT_DSC
          *! B609028,1 MMT 10/07/2009 Fix bug of not exporting Task notes to excel[End]
        ENDIF
        lcTaskNum = ALLTRIM(STR(lnTaskNum))
        IF !SEEK(PMPRJDT.COPRT_CTG+PMPRJDT.COPRT_ID,lcTaskRef)
          INSERT INTO (lcTaskRef)  VALUES (PMPRJDT.COPRT_CTG,PMPRJDT.COPRT_ID,lnTaskNum)
          lnTaskNum = lnTaskNum + 1
        ELSE
          lcTaskNum = ALLTRIM(STR(&lcTaskRef..nNum))
        ENDIF
        m.cOprID&lcTaskNum. = PMPRJDT.COPRT_ID
        m.cOpDsc&lcTaskNum. = PMPRJDT.COPRT_DSC
        m.COPCTG&lcTaskNum. = PMPRJDT.COPRT_CTG
        IF !EOF('SYSCHDUL')
          IF SYSCHDUL.COPERSTAT  = 'C'
            m.cTskSt&lcTaskNum. = 'Completed '+DTOC(PMPRJDT.dAct_Fnsh)
          ELSE
            IF !EOF('SYSCHDUL')
              m.cTskSt&lcTaskNum. = 'Due  '+DTOC(PMPRJDT.DCLC_FNSH)
            ELSE
              m.cTskSt&lcTaskNum. = ''
            ENDIF
          ENDIF
        ENDIF
        m.dAcFnh&lcTaskNum. = PMPRJDT.dAct_Fnsh
        m.DClFnh&lcTaskNum. = PMPRJDT.DCLC_FNSH
        m.mOPCOM&lcTaskNum. = PMPRJDT.mOprt_Com
      ELSE
        lcTaskNum = ALLTRIM(STR(lnTaskNum))
        m.cTskSt&lcTaskNum. = ""
      ENDIF
    ENDSCAN
  ENDIF
  SELECT PMPRJDT
  gfSetOrder('PMPRJDT')

DEFINE CLASS Project_Monitor AS CUSTOM

  *!*************************************************************
  *! Name      : lfEdtTask
  *! Developer : Mariam Mazhar
  *! Date      : 06/01/2009
  *! Purpose   : Edit Task Details
  *!*************************************************************
  FUNCTION lfEdtTask
  *  E302694,1 MMT 05/16/2010 Add Shortcut to Project Monitor to complete tasks[Start]
  LPARAMETERS llNowShow
  *  E302694,1 MMT 05/16/2010 Add Shortcut to Project Monitor to complete tasks[End]
    *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
    *TRY
    *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
      IF TYPE('GLOBALBROWSEWINDOW') = 'O'
        *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
        *lcTableBrw = loFormSet.lcTempFile
        lcTableBrw = IIF(UPPER(GLOBALBROWSEWINDOW.ALIAS)<>UPPeR(loFormSet.lcTempFile),GLOBALBROWSEWINDOW.ALIAS,loFormSet.lcTempFile)
        *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
        *! B608925,1 MMT 07/27/2009 Increase Number of tasks to be 85 instead of 25 [Start]
        lcTableBrw2 = loFormSet.lcTempFile2
        lcTableBrw3 = loFormSet.lcTempFile3
        *! B608925,1 MMT 07/27/2009 Increase Number of tasks to be 85 instead of 25 [End]
        lcCurForm = loFormSet

        *! B609028,1 MMT 10/07/2009 Fix bug of not exporting Task notes to excel[Start]
        *lcCurrentTask = GLOBALBROWSEWINDOW.ariagrid1.Columns(GLOBALBROWSEWINDOW.ariagrid1.ActiveColumn).ControlSource
        lcCurrentTask = lcTaskIdCnt
        *! B609028,1 MMT 10/07/2009 Fix bug of not exporting Task notes to excel[End]

        lcTaskID = EVALUATE(STRTRAN(lcCurrentTask ,'cTskSt','cOprID'))
        lcOpGt = EVALUATE(STRTRAN(lcCurrentTask ,'cTskSt','COPCTG'))

        IF !EMPTY(lcTaskID)
          lnTskColor = 0

          *! B609028,1 MMT 10/07/2009 Fix bug of not exporting Task notes to excel[Start]
          *lnTskColor = EVALUATE(GLOBALBROWSEWINDOW.ariagrid1.Columns(GLOBALBROWSEWINDOW.ariagrid1.ActiveColumn).DynamicForeColor)
          lnTskColor = lnBackClor
          *! B609028,1 MMT 10/07/2009 Fix bug of not exporting Task notes to excel[End]

          *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [Start]
          *DO FORM (oAriaApplication.ScreenHome+"\MFPRJTSK.SCX") WITH &lcTableBrw..CPRJ_TYP,&lcTableBrw..CPRJ_ID,&lcTableBrw..CSTYLE,lcOpGt ,lcTaskID,lnTskColor
          *  E302694,1 MMT 05/16/2010 Add Shortcut to Project Monitor to complete tasks[Start]
          IF llNowShow
            DO FORM (oAriaApplication.ScreenHome+"\MFPRJTSK.SCX") WITH &lcTableBrw..cprj_typ,&lcTableBrw..cprj_id,&lcTableBrw..cstyle,&lcTableBrw..LINENO,lcOpGt ,lcTaskID,lnTskColor,.T. NOSHOW           
          ELSE 
          *  E302694,1 MMT 05/16/2010 Add Shortcut to Project Monitor to complete tasks[End]
          DO FORM (oAriaApplication.ScreenHome+"\MFPRJTSK.SCX") WITH &lcTableBrw..cprj_typ,&lcTableBrw..cprj_id,&lcTableBrw..cstyle,&lcTableBrw..LINENO,lcOpGt ,lcTaskID,lnTskColor
          *  E302694,1 MMT 05/16/2010 Add Shortcut to Project Monitor to complete tasks[Start]
          ENDIF 
          *  E302694,1 MMT 05/16/2010 Add Shortcut to Project Monitor to complete tasks[End]
          *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [End]
    
          loFormSet = lcCurForm
          IF !USED('PMPRJDT_1')
            =gfOpenTable('PMPRJDT','PMPRJDT',"SH",'PMPRJDT_1')
          ENDIF
          lcType = &lcTableBrw..cprj_typ
          lcPrjID = &lcTableBrw..cprj_id
          lcStyleId  = &lcTableBrw..cstyle

          *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [Start]
          lnLineNoId = &lcTableBrw..LINENO
          *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [End]

          SELECT(lcTableBrw)
          lnRcNum = RECNO()

          *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [Start]
          *SCAN FOR CPRJ_TYP  = lcType  AND CPRJ_ID = lcPrjId  AND CSTYLE = lcStyleId
          SCAN FOR cprj_typ  = lcType  AND cprj_id = lcPrjID  AND cstyle = lcStyleId AND LINENO =  lnLineNoId
          *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [End]

            FOR lnG =1 TO lnTaskCnt
              lcG = ALLTRIM(STR(lnG))
              *! B608925,1 MMT 07/27/2009 Increase Number of tasks to be 85 instead of 25 [Start]
              *!*            lcOpGt = EVALUATE(lcTableBrw+'.COPCTG'+lcG)
              *!*            lcTaskID = EVALUATE(lcTableBrw+'.cOprID'+lcG)
              lcOpGt = EVALUATE(lcTableBrw2+'.COPCTG'+lcG)
              lcTaskID = EVALUATE(lcTableBrw2+'.cOprID'+lcG)
              *! B608925,1 MMT 07/27/2009 Increase Number of tasks to be 85 instead of 25 [End]
              lcTskUp = 'cTskSt&lcG.'
              
              *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [Start]
              *IF !gfSeek(&lcTableBrw..CPRJ_TYP+&lcTableBrw..CPRJ_ID+&lcTableBrw..CSTYLE+lcOpGt+lcTaskID,'PMPRJDT_1')
              IF !gfSeek(&lcTableBrw..cprj_typ+&lcTableBrw..cprj_id+&lcTableBrw..cstyle+STR(&lcTableBrw..LINENO,6)+lcOpGt+lcTaskID,'PMPRJDT_1')
              *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [End]
                LOOP
              ENDIF
              *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
              *SELECT SYSCHDUL
              *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
              *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [Start]
              *IF gfSEEK(SUBSTR(PMPRJDT_1.CPRJ_TYP,1,1) + SUBSTR(PMPRJDT_1.CPRJ_ID,1,6) + SUBSTR(PMPRJDT_1.CSTYLE,1,12) +PMPRJDT_1.cOprt_Ctg+PMPRJDT_1.cOprt_ID)
              *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
*!*                IF gfSeek(SUBSTR(PMPRJDT_1.cprj_typ,1,1) + SUBSTR(PMPRJDT_1.cprj_id,1,6) +;
*!*                    SUBSTR(PMPRJDT_1.cstyle,1,19) +STR(PMPRJDT_1.LINENO,6)+PMPRJDT_1.COPRT_CTG+PMPRJDT_1.COPRT_ID)
              IF gfSeek(SUBSTR(PMPRJDT_1.cprj_typ,1,1) + SUBSTR(PMPRJDT_1.cprj_id,1,6) +;
                  SUBSTR(PMPRJDT_1.cstyle,1,19) +STR(PMPRJDT_1.LINENO,6)+PMPRJDT_1.COPRT_CTG+PMPRJDT_1.COPRT_ID,'SYSCHDUL')
              *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
                *! B609555,1 MMT 03/27/2011 Task List screen gives error while completing task[Start]
                IF SYSCHDUL.COPERSTAT= 'C'
                  lnOldSel = SELECT()
                  SELECT SYSCHDUL
                  LOCATE REST WHILE CCONTTYPE+CSEQNUMBER+CSTYLE+STR(LINENO,6)+CCONT_ID+COPERSTAT+CUSER_ID=SUBSTR(PMPRJDT_1.cprj_typ,1,1) + SUBSTR(PMPRJDT_1.cprj_id,1,6) +;
                      SUBSTR(PMPRJDT_1.cstyle,1,19) +STR(PMPRJDT_1.LINENO,6)+PMPRJDT_1.COPRT_CTG+PMPRJDT_1.COPRT_ID FOR COPERSTAT <> 'C'
                  IF !FOUND()
                    =gfSeek(SUBSTR(PMPRJDT_1.cprj_typ,1,1) + SUBSTR(PMPRJDT_1.cprj_id,1,6) +;
                      SUBSTR(PMPRJDT_1.cstyle,1,19) +STR(PMPRJDT_1.LINENO,6)+PMPRJDT_1.COPRT_CTG+PMPRJDT_1.COPRT_ID,'SYSCHDUL')
                  ENDIF 
                  SELECT(lnOldSel) 
                ENDIF 
                *! B609555,1 MMT 03/27/2011 Task List screen gives error while completing task[End]              
              *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [End]
                IF SYSCHDUL.COPERSTAT  = 'C'
                  *! B608925,1 MMT 07/27/2009 Increase Number of tasks to be 85 instead of 25 [Start]
                  
                  *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
                  IF TYPE('&lcTableBrw2..&lcTskUp') <> 'U'
                  *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
                    REPLACE &lcTskUp  WITH 'Completed '+DTOC(PMPRJDT_1.dAct_Fnsh) IN  (lcTableBrw2)
                  *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
                  ENDIF
                  *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
                  *! B608925,1 MMT 07/27/2009 Increase Number of tasks to be 85 instead of 25 [End]
                ELSE
                  IF !EOF("SYSCHDUL")
                    *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
                    IF TYPE('&lcTableBrw2..&lcTskUp') <> 'U'
                    *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
                      REPLACE &lcTskUp  WITH 'Due  '+DTOC(PMPRJDT_1.DCLC_FNSH) IN (lcTableBrw2)
                    *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
                    ENDIF
                    *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
                  ENDIF
                ENDIF
                lcActFnsh  = 'dAcFnh&lcG.'
                lcClcFnsh  = 'DClFnh&lcG.'
                lcCommVr   = 'mOpCom&lcG.'
                *! B608925,1 MMT 07/27/2009 Increase Number of tasks to be 85 instead of 25 [Start]
                *!*              REPLACE &lcActFnsh WITH PMPRJDT_1.DAct_fnsh,;
                *!*                      &lcClcFnsh WITH PMPRJDT_1.Dclc_fnsh,;
                *!*                      &lcCommVr  WITH PMPRJDT_1.mOprt_Com IN (lcTableBrw)
                REPLACE &lcActFnsh WITH PMPRJDT_1.dAct_Fnsh,;
                  &lcClcFnsh WITH PMPRJDT_1.DCLC_FNSH IN (lcTableBrw3)

                REPLACE &lcCommVr  WITH PMPRJDT_1.mOprt_Com IN (lcTableBrw)
                *! B608925,1 MMT 07/27/2009 Increase Number of tasks to be 85 instead of 25 [End]
              ELSE
                *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
                IF TYPE('&lcTableBrw2..&lcTskUp') <> 'U'
                *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
                  REPLACE &lcTskUp  WITH "" IN (lcTableBrw2)
                *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
                ENDIF  
                *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
              ENDIF
            ENDFOR
          ENDSCAN
          
          
          *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
          IF TYPE('GLOBALBROWSEWINDOW') = 'O' AND UPPER(GLOBALBROWSEWINDOW.ALIAS)<>UPPER(loFormSet.lcTempFile)
            SELECT(loFormSet.lcTempFile)
            SCAN FOR cprj_typ  = lcType  AND cprj_id = lcPrjID  AND cstyle = lcStyleId AND LINENO =  lnLineNoId
            FOR lnG =1 TO lnTaskCnt
              lcG = ALLTRIM(STR(lnG))
              lcOpGt = EVALUATE(lcTableBrw2+'.COPCTG'+lcG)
              lcTaskID = EVALUATE(lcTableBrw2+'.cOprID'+lcG)
              lcTskUp = 'cTskSt&lcG.'
              IF !gfSeek(EVALUATE(loFormSet.lcTempFile+'.cprj_typ')+EVALUATE(loFormSet.lcTempFile+'.cprj_id')+EVALUATE(loFormSet.lcTempFile+'.cstyle')+STR(EVALUATE(loFormSet.lcTempFile+'.LINENO'),6)+lcOpGt+lcTaskID,'PMPRJDT_1')
                LOOP
              ENDIF
              IF gfSeek(SUBSTR(PMPRJDT_1.cprj_typ,1,1) + SUBSTR(PMPRJDT_1.cprj_id,1,6) +;
                  SUBSTR(PMPRJDT_1.cstyle,1,19) +STR(PMPRJDT_1.LINENO,6)+PMPRJDT_1.COPRT_CTG+PMPRJDT_1.COPRT_ID,'SYSCHDUL')
                  
                *! B609555,1 MMT 03/27/2011 Task List screen gives error while completing task[Start]
                IF SYSCHDUL.COPERSTAT= 'C'
                  lnOldSel = SELECT()
                  SELECT SYSCHDUL
                  LOCATE REST WHILE CCONTTYPE+CSEQNUMBER+CSTYLE+STR(LINENO,6)+CCONT_ID+COPERSTAT+CUSER_ID=SUBSTR(PMPRJDT_1.cprj_typ,1,1) + SUBSTR(PMPRJDT_1.cprj_id,1,6) +;
                  SUBSTR(PMPRJDT_1.cstyle,1,19) +STR(PMPRJDT_1.LINENO,6)+PMPRJDT_1.COPRT_CTG+PMPRJDT_1.COPRT_ID FOR COPERSTAT <> 'C'
                  IF !FOUND()
                    =gfSeek(SUBSTR(PMPRJDT_1.cprj_typ,1,1) + SUBSTR(PMPRJDT_1.cprj_id,1,6) +;
                      SUBSTR(PMPRJDT_1.cstyle,1,19) +STR(PMPRJDT_1.LINENO,6)+PMPRJDT_1.COPRT_CTG+PMPRJDT_1.COPRT_ID,'SYSCHDUL')
                  ENDIF 
                  SELECT(lnOldSel) 
                ENDIF 
                *! B609555,1 MMT 03/27/2011 Task List screen gives error while completing task[End]
                                  
                IF SYSCHDUL.COPERSTAT  = 'C'
                  IF TYPE('&lcTableBrw2..&lcTskUp') <> 'U'
                    REPLACE &lcTskUp  WITH 'Completed '+DTOC(PMPRJDT_1.dAct_Fnsh) IN  (lcTableBrw2)
                  ENDIF
                ELSE
                  IF !EOF("SYSCHDUL")
                    IF TYPE('&lcTableBrw2..&lcTskUp') <> 'U'
                       REPLACE &lcTskUp  WITH 'Due  '+DTOC(PMPRJDT_1.DCLC_FNSH) IN (lcTableBrw2)
                    ENDIF
                  ENDIF
                ENDIF
                lcActFnsh  = 'dAcFnh&lcG.'
                lcClcFnsh  = 'DClFnh&lcG.'
                lcCommVr   = 'mOpCom&lcG.'
                REPLACE &lcActFnsh WITH PMPRJDT_1.dAct_Fnsh,;
                  &lcClcFnsh WITH PMPRJDT_1.DCLC_FNSH IN (lcTableBrw3)
                REPLACE &lcCommVr  WITH PMPRJDT_1.mOprt_Com IN (lcTableBrw)
              ELSE
                IF TYPE('&lcTableBrw2..&lcTskUp') <> 'U'
                  REPLACE &lcTskUp  WITH "" IN (lcTableBrw2)
                 ENDIF
              ENDIF
            ENDFOR
          ENDSCAN
          ENDIF
          *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
          
          
          SELECT(lcTableBrw)
          IF BETWEEN(lnRcNum,1, RECCOUNT())
            GO RECORD lnRcNum
          ENDIF
          IF TYPE('GLOBALBROWSEWINDOW') = 'O'
            GLOBALBROWSEWINDOW.AriaGrid1.REFRESH
          ENDIF
        ENDIF
      ENDIF
    *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
    *CATCH
    *ENDTRY
    *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
*  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [Start]    
 *!*************************************************************
  *! Name      : lfEdtTTask
  *! Developer : Mariam Mazhar
  *! Date      : 06/01/2009
  *! Purpose   : Edit Task Details
  *!*************************************************************
  FUNCTION lfEdtTTask
  *  E302694,1 MMT 05/16/2010 Add Shortcut to Project Monitor to complete tasks[Start]
  LPARAMETERS llNowShow
  *  E302694,1 MMT 05/16/2010 Add Shortcut to Project Monitor to complete tasks[End]
    lcOldSel = SELECT(0)
    *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
    *TRY
    *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
      IF TYPE('GLOBALBROWSEWINDOW') = 'O'
        lcCurForm = loFormSet
        lcCurrentTask = lcTaskIdCnt
        IF EMPTY(EVALUATE(lcCurrentTask))
          RETURN  
        ENDIF 
       * lcTaskID = EVALUATE(STRTRAN(lcCurrentTask ,'cTskSt','cOprID'))
        lcTaskID = STRTRAN(UPPER(lcCurrentTask) ,UPPER('cTskSt'),'COPRID')
       * lcOpGt = EVALUATE(STRTRAN(lcCurrentTask ,'cTskSt','COPCTG'))
        lnDotPos = ATC('.', lcTaskID)
        lcTaskFld = ''
        IF lnDotPos > 0
          lcTaskFld = SUBSTR( lcTaskID ,lnDotPos+1)
        ENDIF   
        lcFlNm = ''
        lcFilName = ''
        IF !EMPTY(lcTaskFld)
          FOR lnC = 1 TO ALEN(loFormSet.LATEMPFL,1)
            lcFldName = loFormSet.LATEMPFL[lnC,1]+'.'+ALLTRIM(lcTaskFld)
            IF TYPE(lcFldName) = 'U'
              LOOP
            ELSE
              lcTaskFld = loFormSet.LATEMPFL[lnC,1]+"."+ALLTRIM(lcTaskFld)
              lcFilName = loFormSet.LATEMPFL[lnC,1]
              EXIT 
            ENDIF
          ENDFOR
        ENDIF 
        
        *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
        IF UPPER(lcFilName) = loFormSet.LATEMPFL[1,1]
          IF TYPE('GLOBALBROWSEWINDOW') = 'O' AND UPPER(GLOBALBROWSEWINDOW.ALIAS)<>UPPeR(loFormSet.LATEMPFL[1,1])
            lcTaskFld = STRTRAN(UPPER(lcTaskFld),UPPeR(loFormSet.LATEMPFL[1,1]),GLOBALBROWSEWINDOW.ALIAS)
            lcFilName  = GLOBALBROWSEWINDOW.ALIAS
          ENDIF
        ENDIF
        *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
        
        
        IF !EMPTY(lcTaskFld)
          lcTaskID  = EVALUATE(lcTaskFld)
          lcOpGt = EVALUATE(STRTRAN(UPPER(lcTaskFld),UPPER('cOprID'),'COPCTG'))
          lnTskColor = 0
          lnTskColor = lnBackClor
          *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
          *lcTableBrw = loFormSet.LATEMPFL[1,1]
          lcTableBrw = IIF(TYPE('GLOBALBROWSEWINDOW') = 'O' AND UPPER(GLOBALBROWSEWINDOW.ALIAS)<>UPPeR(loFormSet.LATEMPFL[1,1]),GLOBALBROWSEWINDOW.ALIAS,loFormSet.LATEMPFL[1,1])          
          *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
          *  E302694,1 MMT 05/16/2010 Add Shortcut to Project Monitor to complete tasks[Start]
          IF llNowShow
            DO FORM (oAriaApplication.ScreenHome+"\MFPRJTSK.SCX") WITH &lcTableBrw..cprj_typ,&lcTableBrw..cprj_id,&lcTableBrw..cstyle,&lcTableBrw..LINENO,lcOpGt ,lcTaskID,lnTskColor,.T. NOSHOW           
          ELSE 
          *  E302694,1 MMT 05/16/2010 Add Shortcut to Project Monitor to complete tasks[end]
            DO FORM (oAriaApplication.ScreenHome+"\MFPRJTSK.SCX") WITH &lcTableBrw..cprj_typ,&lcTableBrw..cprj_id,&lcTableBrw..cstyle,&lcTableBrw..LINENO,lcOpGt ,lcTaskID,lnTskColor
          *  E302694,1 MMT 05/16/2010 Add Shortcut to Project Monitor to complete tasks[Start]
          ENDIF
          *  E302694,1 MMT 05/16/2010 Add Shortcut to Project Monitor to complete tasks[End]
          loFormSet = lcCurForm
          IF !USED('PMPRJDT_1')
            =gfOpenTable('PMPRJDT','PMPRJDT',"SH",'PMPRJDT_1')
          ENDIF
          lcType = &lcTableBrw..cprj_typ
          lcPrjID = &lcTableBrw..cprj_id
          lcStyleId  = &lcTableBrw..cstyle
          lnLineNoId = &lcTableBrw..LINENO
          SELECT(lcTableBrw)
          lnRcNum = RECNO()
          SCAN FOR cprj_typ  = lcType  AND cprj_id = lcPrjID  AND cstyle = lcStyleId AND LINENO =  lnLineNoId
            FOR lnG =1 TO ALEN(loFormSet.LATEMPACT,1)
              lcG = ALLTRIM(STR(lnG))
              lcOpGt = SUBSTR(loFormSet.LATEMPACT[lnG,1],1,3)
              lcTaskID = RIGhT(loFormSet.LATEMPACT[lnG,1],5)
              lcTableBrw2 = lcFilName
              FOR lnC = 1 TO ALEN(loFormSet.LATEMPFL,1)
                lcFldName = loFormSet.LATEMPFL[lnC,1]+'.'+'COPCTG&lcG.'
                IF TYPE(lcFldName) = 'U'
                  LOOP
                ELSE
                  lcTableBrw2  = loFormSet.LATEMPFL[lnC,1]
                  EXIT 
                ENDIF
              ENDFOR
              lcTskUp = 'cTskSt&lcG.'
              IF !gfSeek(&lcTableBrw..cprj_typ+&lcTableBrw..cprj_id+&lcTableBrw..cstyle+STR(&lcTableBrw..LINENO,6)+lcOpGt+lcTaskID,'PMPRJDT_1')
                LOOP
              ENDIF
              *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
*!*                SELECT SYSCHDUL
*!*                IF gfSeek(SUBSTR(PMPRJDT_1.cprj_typ,1,1) + SUBSTR(PMPRJDT_1.cprj_id,1,6) +;
*!*                    SUBSTR(PMPRJDT_1.cstyle,1,19) +STR(PMPRJDT_1.LINENO,6)+PMPRJDT_1.COPRT_CTG+PMPRJDT_1.COPRT_ID)
              IF gfSeek(SUBSTR(PMPRJDT_1.cprj_typ,1,1) + SUBSTR(PMPRJDT_1.cprj_id,1,6) +;
                  SUBSTR(PMPRJDT_1.cstyle,1,19) +STR(PMPRJDT_1.LINENO,6)+PMPRJDT_1.COPRT_CTG+PMPRJDT_1.COPRT_ID,'SYSCHDUL')
              *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
              
                *! B609555,1 MMT 03/27/2011 Task List screen gives error while completing task[Start]
                IF SYSCHDUL.COPERSTAT= 'C'
                  lnOldSel = SELECT()
                  SELECT SYSCHDUL
                  LOCATE REST WHILE CCONTTYPE+CSEQNUMBER+CSTYLE+STR(LINENO,6)+CCONT_ID+COPERSTAT+CUSER_ID=SUBSTR(PMPRJDT_1.cprj_typ,1,1) + SUBSTR(PMPRJDT_1.cprj_id,1,6) +;
                  SUBSTR(PMPRJDT_1.cstyle,1,19) +STR(PMPRJDT_1.LINENO,6)+PMPRJDT_1.COPRT_CTG+PMPRJDT_1.COPRT_ID FOR COPERSTAT <> 'C'
                  IF !FOUND()
                    =gfSeek(SUBSTR(PMPRJDT_1.cprj_typ,1,1) + SUBSTR(PMPRJDT_1.cprj_id,1,6) +;
                  SUBSTR(PMPRJDT_1.cstyle,1,19) +STR(PMPRJDT_1.LINENO,6)+PMPRJDT_1.COPRT_CTG+PMPRJDT_1.COPRT_ID,'SYSCHDUL')
                  ENDIF 
                  SELECT(lnOldSel) 
                ENDIF 
                *! B609555,1 MMT 03/27/2011 Task List screen gives error while completing task[End]              
              
                IF SYSCHDUL.COPERSTAT  = 'C'
                  *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
                  IF TYPE('&lcTableBrw..&lcTskUp') <> 'U'
                  *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
                    REPLACE &lcTskUp  WITH 'Completed '+DTOC(PMPRJDT_1.dAct_Fnsh) IN  (lcTableBrw)
                  *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
                  ENDIF
                  *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
                ELSE
                  IF !EOF("SYSCHDUL")
                    *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
                    IF TYPE('&lcTableBrw..&lcTskUp') <> 'U'
                    *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
                      REPLACE &lcTskUp  WITH 'Due  '+DTOC(PMPRJDT_1.DCLC_FNSH) IN (lcTableBrw)
                    *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
                    ENDIF
                    *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
                  ENDIF
                ENDIF
                lcActFnsh  = 'dAcFnh&lcG.'
                lcClcFnsh  = 'DClFnh&lcG.'
                lcCommVr   = 'mOpCom&lcG.'
                REPLACE &lcActFnsh WITH PMPRJDT_1.dAct_Fnsh,;
                     &lcClcFnsh WITH PMPRJDT_1.DCLC_FNSH IN (lcTableBrw2)

                *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
                *REPLACE &lcCommVr  WITH &lcTskUp+CHR(13)+CHR(10)+PMPRJDT_1.mOprt_Com IN (lcTableBrw2)
                IF TYPE('&lcTableBrw..&lcTskUp') <> 'U'
                  REPLACE &lcCommVr  WITH EVALUATE(lcTableBrw+'.'+lcTskUp)+CHR(13)+CHR(10)+PMPRJDT_1.mOprt_Com IN (lcTableBrw2)
                ENDIF                
                *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
              ELSE
                *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
                IF TYPE('&lcTableBrw..&lcTskUp') <> 'U'
                *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
                  REPLACE &lcTskUp  WITH "" IN (lcTableBrw)
                *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
                ENDIF
                *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[END]
              ENDIF
            ENDFOR
          ENDSCAN
          
          *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
          IF TYPE('GLOBALBROWSEWINDOW') = 'O' AND UPPER(GLOBALBROWSEWINDOW.ALIAS)<>UPPeR(loFormSet.LATEMPFL[1,1])
            SELECT(loFormSet.LATEMPFL[1,1])
            SCAN FOR cprj_typ  = lcType  AND cprj_id = lcPrjID  AND cstyle = lcStyleId AND LINENO =  lnLineNoId
              FOR lnG =1 TO ALEN(loFormSet.LATEMPACT,1)
                lcG = ALLTRIM(STR(lnG))
                lcOpGt = SUBSTR(loFormSet.LATEMPACT[lnG,1],1,3)
                lcTaskID = RIGhT(loFormSet.LATEMPACT[lnG,1],5)
                lcTableBrw2 = lcFilName
                FOR lnC = 1 TO ALEN(loFormSet.LATEMPFL,1)
                  lcFldName = loFormSet.LATEMPFL[lnC,1]+'.'+'COPCTG&lcG.'
                  IF TYPE(lcFldName) = 'U'
                    LOOP
                  ELSE
                    lcTableBrw2  = loFormSet.LATEMPFL[lnC,1]
                    EXIT 
                  ENDIF
                ENDFOR
                lcTskUp = 'cTskSt&lcG.'
                IF !gfSeek(EVALUATE(loFormSet.LATEMPFL[1,1]+'.cprj_typ')+EVALUATE(loFormSet.LATEMPFL[1,1]+'.cprj_id')+EVALUATE(loFormSet.LATEMPFL[1,1]+'.cstyle')+STR(EVALUATE(loFormSet.LATEMPFL[1,1]+'.LINENO'),6)+lcOpGt+lcTaskID,'PMPRJDT_1')
                  LOOP
                ENDIF
                IF gfSeek(SUBSTR(PMPRJDT_1.cprj_typ,1,1) + SUBSTR(PMPRJDT_1.cprj_id,1,6) +;
                  SUBSTR(PMPRJDT_1.cstyle,1,19) +STR(PMPRJDT_1.LINENO,6)+PMPRJDT_1.COPRT_CTG+PMPRJDT_1.COPRT_ID,'SYSCHDUL')
                  
                *! B609555,1 MMT 03/27/2011 Task List screen gives error while completing task[Start]
                IF SYSCHDUL.COPERSTAT= 'C'
                  lnOldSel = SELECT()
                  SELECT SYSCHDUL
                  LOCATE REST WHILE CCONTTYPE+CSEQNUMBER+CSTYLE+STR(LINENO,6)+CCONT_ID+COPERSTAT+CUSER_ID=SUBSTR(PMPRJDT_1.cprj_typ,1,1) + SUBSTR(PMPRJDT_1.cprj_id,1,6) +;
                  SUBSTR(PMPRJDT_1.cstyle,1,19) +STR(PMPRJDT_1.LINENO,6)+PMPRJDT_1.COPRT_CTG+PMPRJDT_1.COPRT_ID FOR COPERSTAT <> 'C'
                  IF !FOUND()
                    =gfSeek(SUBSTR(PMPRJDT_1.cprj_typ,1,1) + SUBSTR(PMPRJDT_1.cprj_id,1,6) +;
                  SUBSTR(PMPRJDT_1.cstyle,1,19) +STR(PMPRJDT_1.LINENO,6)+PMPRJDT_1.COPRT_CTG+PMPRJDT_1.COPRT_ID,'SYSCHDUL')
                  ENDIF 
                  SELECT(lnOldSel) 
                ENDIF 
                *! B609555,1 MMT 03/27/2011 Task List screen gives error while completing task[End]
                  
                  
                  IF SYSCHDUL.COPERSTAT  = 'C'
                    IF TYPE('&lcTableBrw..&lcTskUp') <> 'U'
                      REPLACE &lcTskUp  WITH 'Completed '+DTOC(PMPRJDT_1.dAct_Fnsh) IN  (lcTableBrw)
                    ENDIF
                  ELSE
                    IF !EOF("SYSCHDUL")
                      IF TYPE('&lcTableBrw..&lcTskUp') <> 'U'
                        REPLACE &lcTskUp  WITH 'Due  '+DTOC(PMPRJDT_1.DCLC_FNSH) IN (lcTableBrw)
                      ENDIF
                    ENDIF
                  ENDIF
                  lcActFnsh  = 'dAcFnh&lcG.'
                  lcClcFnsh  = 'DClFnh&lcG.'
                  lcCommVr   = 'mOpCom&lcG.'
                  REPLACE &lcActFnsh WITH PMPRJDT_1.dAct_Fnsh,;
                          &lcClcFnsh WITH PMPRJDT_1.DCLC_FNSH IN (lcTableBrw2)
                  lcDumVr =loFormSet.LATEMPFL[1,1]  
                  IF TYPE('&lcDumVr..&lcTskUp') <> 'U'
                     REPLACE &lcCommVr  WITH EVALUATE(loFormSet.LATEMPFL[1,1]+'.'+lcTskUp)+CHR(13)+CHR(10)+PMPRJDT_1.mOprt_Com IN (lcTableBrw2)
                  ENDIF
                ELSE
                  IF TYPE('&lcTableBrw..&lcTskUp') <> 'U'
                    REPLACE &lcTskUp  WITH "" IN (lcTableBrw)
                  ENDIF
                ENDIF
              ENDFOR
            ENDSCAN
          ENDIF
          *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
          
          
          SELECT(lcTableBrw)
          IF BETWEEN(lnRcNum,1, RECCOUNT())
            GO RECORD lnRcNum
          ENDIF
          IF TYPE('GLOBALBROWSEWINDOW') = 'O'
            GLOBALBROWSEWINDOW.AriaGrid1.REFRESH
             SELECT (lcOldSel)
              FOR lnE = 1 TO GLOBALBROWSEWINDOW.AriaGrid1.ColumnCount
                lcContSource = GLOBALBROWSEWINDOW.AriaGrid1.Columns[lnE].ControlSource
                IF !EMPTY(lcContSource) AND (UPPER(ALIAS()) $ UPPER(lcContSource)) AND;
                   ('CTSKST' $ UPPER(lcContSource) OR ;
                   'PO'$ UPPER(lcContSource) OR ;
                   'ORDER'$ UPPER(lcContSource) OR ;
                   'STYLE'$ UPPER(lcContSource) OR ;
                   'ACCOUNT'$ UPPER(lcContSource) OR ;
                   'STORE'$ UPPER(lcContSource) OR ;
                   'VENDOR'$ UPPER(lcContSource) OR ;
                   'CPRJ_ID'$ UPPER(lcContSource) OR ;
                   'CVENDCODE'$ UPPER(lcContSource) )

                  IF TYPE('GLOBALBROWSEWINDOW.AriaGrid1.Columns[lnE].Controls(GLOBALBROWSEWINDOW.AriaGrid1.Columns[lnE].ControlCount).txtBox')  ='O'
                    GLOBALBROWSEWINDOW.AriaGrid1.Columns[lnE].Controls(GLOBALBROWSEWINDOW.AriaGrid1.Columns[lnE].ControlCount).txtBox.ControlSource = lcContSource 
                  ENDIF   
                  loParentForm.LATEMPFL[1,1] = ALIAS()
                  FOR lnT = 1 TO ALEN(loParentForm.LATEMPFL,1)
                    IF !EMPTY(loParentForm.LATEMPFL[lnT,1])
                      x= 'T'+ALLTRIM(STR(lnT))
                      &x. = loParentForm.LATEMPFL[lnT,1]
                      loParentForm.LATEMPFL[lnT,2] = x
                    ENDIF
                  ENDFOR
                ENDIF 
              ENDFOR
              SELECT (loParentForm.LATEMPFL[1,1]) 
              GLOBALBROWSEWINDOW.REFRESH
          ENDIF
        ENDIF
        
      ENDIF
  *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]    
  *CATCH
  *ENDTRY
  *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]

*  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [End]    
    *!*************************************************************
    *! Name      : lfTranFrm
    *! Developer : Mariam Mazhar
    *! Date      : 06/01/2009
    *! Purpose   : call transcation screens
    *!*************************************************************
  FUNCTION lfTranFrm

    *! B609028,1 MMT 10/07/2009 Fix bug of not exporting Task notes to excel[Start]
    *!*    lfGetProjID(lcRpPrjTp ,EVALUATE(loFormSet.lcTempFile+'.CPRJ_ID'),EVALUATE(loFormSet.lcTempFile+'.CStyle'))
    *!*    loTRParen= _Screen.ActiveForm.Parent
    *!*    IF TYPE('GLOBALBROWSEWINDOW') = 'O'
    *!*      GLOBALBROWSEWINDOW.hide()
    *!*      GLOBALBROWSEWINDOW.Show(0)
    *!*    ENDIF
    *!*    IF TYPE('loTRParen') = 'O'
    *!*     loTRParen.Show()
    *!*    ENDIF
    IF TYPE('GLOBALBROWSEWINDOW') = 'O'
      GLOBALBROWSEWINDOW.HIDE()
      GLOBALBROWSEWINDOW.SHOW(0)
    ENDIF
    lfGetProjID(lcRpPrjTp ,EVALUATE(loFormSet.lcTempFile+'.CPRJ_ID'),EVALUATE(loFormSet.lcTempFile+'.CStyle'))
    loTRParen= _SCREEN.ACTIVEFORM.PARENT
    loTRParen.HIDE()
    loTRParen.SHOW(1)
    *! B609028,1 MMT 10/07/2009 Fix bug of not exporting Task notes to excel[End]
    RETURN



    *!*************************************************************
    *! Name      : lfGetNot
    *! Developer : Mariam Mazhar
    *! Date      : 06/01/2009
    *! Purpose   : Tool tip note
    *!*************************************************************
  FUNCTION lfGetNot
    LPARAMETERS nButton, nShift, nXCoord, nYCoord
    *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
    *TRY
    *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
      *! B609028,2 MMT 10/13/2009 Fix bug of Wrong Order of exporting Task notes to excel[Start]
      *!*    lnCol = GLOBALBROWSEWINDOW.ariagrid1.ActiveColumn
      *!*    IF lnCol = 0
      *!*      RETURN
      *!*    ENDIF
      *!*  lcCurrentTask = GLOBALBROWSEWINDOW.ariagrid1.Columns(lnCol).ControlSource
      lcCurrentTask = lcTaskIdCnt
      lnCol = 0
      IF !('cTskSt' $ lcCurrentTask)
        RETURN
      ENDIF
      IF TYPE('GLOBALBROWSEWINDOW')    <> 'O'
        RETURN
      ELSE
        FOR lnW = 1 TO GLOBALBROWSEWINDOW.AriaGrid1.COLUMNCOUNT
          *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
          *IF  UPPER(lcCurrentTask) =  UPPER(GLOBALBROWSEWINDOW.AriaGrid1.COLUMNS(lnW).CONTROLSOURCE)
          IF  UPPER(lcCurrentTask) ==  UPPER(GLOBALBROWSEWINDOW.AriaGrid1.COLUMNS(lnW).CONTROLSOURCE)
          *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
            lnCol = lnW
          ENDIF
        ENDFOR
      ENDIF
      IF lnCol = 0
        RETURN
      ENDIF
      *! B609028,2 MMT 10/13/2009 Fix bug of Wrong Order of exporting Task notes to excel[End]

      *! B608925,1 MMT 07/27/2009 Increase Number of tasks to be 85 instead of 25 [Start]
      *    lcNotes = STRTRAN(lcCurrentTask ,'cTskSt','mOpCom')
      lnDotPos = ATC('.',lcCurrentTask)
      IF lnDotPos > 0
        lcNotes = STRTRAN(lcCurrentTask ,'cTskSt','mOpCom')
        lcNotes = lcTempFile + SUBSTR(lcNotes,lnDotPos)
      ENDIF
      *! B608925,1 MMT 07/27/2009 Increase Number of tasks to be 85 instead of 25 [End]

      GLOBALBROWSEWINDOW.AriaGrid1.COLUMNS(lnCol).CONTROLS(GLOBALBROWSEWINDOW.AriaGrid1.COLUMNS(lnCol).CONTROLCOUNT).TOOLTIPTEXT = EVALUATE(lcNotes)
      GLOBALBROWSEWINDOW.AriaGrid1.COLUMNS(lnCol).CONTROLS(GLOBALBROWSEWINDOW.AriaGrid1.COLUMNS(lnCol).CONTROLCOUNT).txtBox.TOOLTIPTEXT = EVALUATE(lcNotes)
      GLOBALBROWSEWINDOW.AriaGrid1.COLUMNS(lnCol).CONTROLS(GLOBALBROWSEWINDOW.AriaGrid1.COLUMNS(lnCol).CONTROLCOUNT).cmdButton.TOOLTIPTEXT = EVALUATE(lcNotes)
      GLOBALBROWSEWINDOW.AriaGrid1.COLUMNS(lnCol).CONTROLS(GLOBALBROWSEWINDOW.AriaGrid1.COLUMNS(lnCol).CONTROLCOUNT).REFRESH
    *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]  
    *CATCH
    *ENDTRY
    *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]

    *! B609028,2 MMT 10/13/2009 Fix bug of Wrong Order of exporting Task notes to excel[Start]
    *!*************************************************************
    *! Name      : lfReOrderBrow
    *! Developer : Mariam Mazhar
    *! Date      : 10/13/2009
    *! Purpose   : Function to reorder browse screen columns while exporting to Excel
    *!*************************************************************
  FUNCTION lfReOrderBrow
    IF TYPE('GLOBALBROWSEWINDOW')    = 'O'
      lnSetExct = SET("Exact")
      SET EXACT ON
      FOR lnC = 1 TO GLOBALBROWSEWINDOW.AriaGrid1.COLUMNCOUNT
        lcHeader =  GLOBALBROWSEWINDOW.AriaGrid1.COLUMNS(lnC).Header1.CAPTION

        IF '~' $ lcHeader OR ' Notes' $ lcHeader
          LOOP
        ENDIF
        lnOrder =  GLOBALBROWSEWINDOW.AriaGrid1.COLUMNS(lnC).COLUMNORDER
        FOR lnD =  1 TO GLOBALBROWSEWINDOW.AriaGrid1.COLUMNCOUNT
          IF ALLTRIM(lcHeader) $ GLOBALBROWSEWINDOW.AriaGrid1.COLUMNS(lnD).Header1.CAPTION  AND;
              ' Notes' $ GLOBALBROWSEWINDOW.AriaGrid1.COLUMNS(lnD).Header1.CAPTION

            FOR lnE = 1 TO GLOBALBROWSEWINDOW.AriaGrid1.COLUMNCOUNT
              IF GLOBALBROWSEWINDOW.AriaGrid1.COLUMNS(lnE).COLUMNORDER > lnOrder+1
                GLOBALBROWSEWINDOW.AriaGrid1.COLUMNS(lnE).COLUMNORDER = GLOBALBROWSEWINDOW.AriaGrid1.COLUMNS(lnE).COLUMNORDER+ 1
              ENDIF
            ENDFOR
            IF lnOrder+1 > GLOBALBROWSEWINDOW.AriaGrid1.COLUMNCOUNT
              lnStrtOrder =  GLOBALBROWSEWINDOW.AriaGrid1.COLUMNS(lnD).COLUMNORDER
              lnMaxOrd =  lnStrtOrder
              FOR lnS = 1 TO GLOBALBROWSEWINDOW.AriaGrid1.COLUMNCOUNT
                IF GLOBALBROWSEWINDOW.AriaGrid1.COLUMNS(lnS).COLUMNORDER > lnStrtOrder
                  IF lnMaxOrd < GLOBALBROWSEWINDOW.AriaGrid1.COLUMNS(lnS).COLUMNORDER
                    lnMaxOrd = GLOBALBROWSEWINDOW.AriaGrid1.COLUMNS(lnS).COLUMNORDER
                  ENDIF
                  GLOBALBROWSEWINDOW.AriaGrid1.COLUMNS(lnS).COLUMNORDER = GLOBALBROWSEWINDOW.AriaGrid1.COLUMNS(lnS).COLUMNORDER - 1
                ENDIF
              ENDFOR
              GLOBALBROWSEWINDOW.AriaGrid1.COLUMNS(lnD).COLUMNORDER = lnMaxOrd
            ELSE
              GLOBALBROWSEWINDOW.AriaGrid1.COLUMNS(lnD).COLUMNORDER = lnOrder + 1
            ENDIF
          ENDIF
        ENDFOR

      ENDFOR
      SET EXACT &lnSetExct
    ENDIF
    *!*************************************************************
    *! Name      : lfRestorePreference
    *! Developer : Mariam Mazhar
    *! Date      : 10/13/2009
    *! Purpose   : Function to Restore Pereferences of browse screen
    *!*************************************************************
  FUNCTION lfRestorePreference

    ConPrefSeperator = CHR(161)
    IF GLOBALBROWSEWINDOW.AriaGrid1.SavePreference AND !EMPTY(GLOBALBROWSEWINDOW.AriaGrid1.PreferenceName)
      *-- initialize variables needed by the method
      LOCAL lcXMLString,lnCount,laPrefInfo,laClmnInfo
      *-- if the preference file exist
      IF FILE(GLOBALBROWSEWINDOW.AriaGrid1.PreferenceName + ".XML")
        DIMENSION laPrefData[4,1]
        DIMENSION laPrefColors[1,2]
        DIMENSION laPrefInfo[3,1]
        *-- get the saved preferences from the file
        lcXMLString = FILETOSTR(GLOBALBROWSEWINDOW.AriaGrid1.PreferenceName + ".XML")
        =gfSubStr(lcXMLString,@laPrefData,ConPrefSeperator)
        lcXMLString = laPrefData[1,1]

        IF TYPE('laPrefData[7, 1]') = 'C'
          GLOBALBROWSEWINDOW.cLastSessionAlias = ALLTRIM(laPrefData[7, 1])

          IF !(UPPER(LEFT(GLOBALBROWSEWINDOW.ALIAS, 1)) = 'X' .AND. UPPER(LEFT(GLOBALBROWSEWINDOW.cLastSessionAlias, 1)) = 'X')

            LOCAL lcAlias,lcLastSessionAlias,lnPos1,lnPos2
            lcAlias = GLOBALBROWSEWINDOW.ALIAS
            lcLastSessionAlias = GLOBALBROWSEWINDOW.cLastSessionAlias
            lnPos1 = AT("_X",UPPER(lcAlias))
            lnPos2 = AT("_X",UPPER(lcLastSessionAlias))

            IF lnPos1 > 1 AND lnPos1 + 8 = LEN(lcAlias)
              lcAlias = SUBSTR(lcAlias,1,lnPos1-1)
            ENDIF

            IF lnPos2 > 1 AND lnPos2 + 8 = LEN(lcLastSessionAlias)
              lcLastSessionAlias = SUBSTR(lcLastSessionAlias,1,lnPos2-1)
            ENDIF

            IF !(UPPER(lcAlias) == UPPER(lcLastSessionAlias))
              RETURN
            ENDIF
          ENDIF
        ELSE
          GLOBALBROWSEWINDOW.cLastSessionAlias = .F.

          *-- In case if the cursor is temp and no last session return
          IF !(UPPER(oAriaApplication.DataDir) == SUBSTR(DBF(GLOBALBROWSEWINDOW.AriaGrid1.RECORDSOURCE), 1, LEN(oAriaApplication.DataDir)))
            RETURN
          ENDIF
        ENDIF



        *-- get the color schemes short cuts
        lcColors = laPrefData[3,1]
        =gfSubStr(lcColors,@laPrefColors,CHR(162)+"~")
        IF !EMPTY(laPrefColors[1,1])
          DIMENSION GLOBALBROWSEWINDOW.Colorschemes[ALEN(laPrefColors,1),ALEN(laPrefColors,2)]

          IF TYPE('GLOBALBROWSEWINDOW.cLastSessionAlias') = 'C'
            LOCAL lnIndex
            FOR lnIndex = 1 TO ALEN(laPrefColors, 1)
              laPrefColors[lnIndex, 2] = STRTRAN(laPrefColors[lnIndex, 2], ;
                ALLTRIM(GLOBALBROWSEWINDOW.cLastSessionAlias), ;
                GLOBALBROWSEWINDOW.AriaGrid1.RECORDSOURCE, ;
                -1, ;
                -1, ;
                1)
            ENDFOR
          ENDIF
          =ACOPY(laPrefColors,GLOBALBROWSEWINDOW.Colorschemes)

          ASORT(GLOBALBROWSEWINDOW.Colorschemes)
        ENDIF
        GLOBALBROWSEWINDOW.COLORSCHEME = ""
        lcClrScheme = ""
        *-- activate the selected color scheme
        IF TYPE('laPrefData[4,1]')='C'
          lcClrScheme = laPrefData[4,1]
          GLOBALBROWSEWINDOW.selectcolor(lcClrScheme)
        ENDIF

        IF TYPE('laPrefData[8,1]') = 'C' .AND. !EMPTY(laPrefData[8,1]) .AND. ;
            TYPE('laPrefData[9,1]') = 'C' .AND. VAL(laPrefData[9,1]) > 0 .AND. ;
            TYPE('laPrefData[10,1]') = 'C' .AND. !EMPTY(laPrefData[10,1])

          GLOBALBROWSEWINDOW.cFontName  = laPrefData[8,1]
          GLOBALBROWSEWINDOW.nFontSize  = VAL(laPrefData[9,1])
          GLOBALBROWSEWINDOW.cFontStyle = laPrefData[10,1]

          GLOBALBROWSEWINDOW.AriaGrid1.FONTNAME   = GLOBALBROWSEWINDOW.cFontName
          GLOBALBROWSEWINDOW.AriaGrid1.FONTSIZE   = GLOBALBROWSEWINDOW.nFontSize
          GLOBALBROWSEWINDOW.AriaGrid1.FONTBOLD   = AT('B', GLOBALBROWSEWINDOW.cFontStyle) > 0
          GLOBALBROWSEWINDOW.AriaGrid1.FONTITALIC = AT('I', GLOBALBROWSEWINDOW.cFontStyle) > 0
        ENDIF
        *-- restore the preference settings in array
        FOR lnCount = 1 TO GLOBALBROWSEWINDOW.AriaGrid1.COLUMNCOUNT
          *-- Is current column mulit select column?
          IF TYPE("GLOBALBROWSEWINDOW.AriaGrid1.MultiSelect") = 'L' .AND. GLOBALBROWSEWINDOW.AriaGrid1.MULTISELECT .AND. ;
              UPPER(ALLTRIM(GLOBALBROWSEWINDOW.AriaGrid1.COLUMNS[lnCount].NAME)) == UPPER("chkMultiSelect")
            LOOP
          ENDIF

          *-- get the column controlsource,columnorder and width
          GLOBALBROWSEWINDOW.AriaGrid1.COLUMNS[lnCount].TAG = GLOBALBROWSEWINDOW.AriaGrid1.COLUMNS[lnCount].NAME
          lcClmnName = IIF(EMPTY(GLOBALBROWSEWINDOW.AriaGrid1.COLUMNS[lnCount].CONTROLSOURCE),GLOBALBROWSEWINDOW.AriaGrid1.COLUMNS[lnCount].NAME,GLOBALBROWSEWINDOW.AriaGrid1.COLUMNS[lnCount].CONTROLSOURCE)


          IF TYPE('GLOBALBROWSEWINDOW.cLastSessionAlias') = 'C' .AND. UPPER(LEFT(GLOBALBROWSEWINDOW.cLastSessionAlias, 1)) = 'X'
            lcClmnName = STRTRAN(lcClmnName, GLOBALBROWSEWINDOW.AriaGrid1.RECORDSOURCE, ALLTRIM(GLOBALBROWSEWINDOW.cLastSessionAlias), -1, -1, 1)
          ENDIF
          lnDotPos = ATC('.',lcClmnName)
           *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [Start]
           x=lcClmnName
          IF "(" $ lcClmnName
            lnEqPos = ATC('=',lcClmnName)
            lnComPos = ATC(',',lcClmnName)
            lnBrPos =  ATC('(',lcClmnName)
            *  E302650,2 MMT 12/24/2009 make project monitor work based on report templated selected [Start]
            *IF lnEqPos < lnComPos 
            IF lnEqPos < lnComPos AND lnEqPos <> 0
             *  E302650,2 MMT 12/24/2009 make project monitor work based on report templated selected [End]
              lcClmnName = SUBSTR(lcClmnName, lnDotPos +1 ,lnEqPos -lnDotPos-1)
            ELSE
              
              lcClmnName = SUBSTR(lcClmnName, lnDotPos +1 ,lnComPos -lnDotPos-1)
            ENDIF   
         ELSE
         *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [End]
           lcClmnName  = SUBSTR(lcClmnName  ,lnDotPos +1)
          *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [Start]
          ENDIF
          *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [End]

          GLOBALBROWSEWINDOW.AriaGrid1.COLUMNS[lnCount].NAME = lcClmnName
        ENDFOR

        =gfSubStr(lcXMLString,@laPrefInfo,"~")
        *-- restore the grid headerheight
        GLOBALBROWSEWINDOW.AriaGrid1.HEADERHEIGHT = VAL(laPrefInfo[1,1])
        *-- restore the grid rowheight
        GLOBALBROWSEWINDOW.AriaGrid1.ROWHEIGHT = VAL(laPrefInfo[2,1])
        lcXMLString = laPrefInfo[3,1]
        DIMENSION laPrefInfo[1,1]
        *-- get the grid columns
        =gfSubStr(lcXMLString,@laPrefInfo,"|")
        FOR lnCount = 1 TO ALEN(laPrefInfo,1)
          DIMENSION laClmnInfo[1,1]
          *-- get the column preference settings
          =gfSubStr(laPrefInfo[lnCount,1],@laClmnInfo)
          *-- if the columns name is an object in the grid
          lnDashPos = ATC('_',laClmnInfo[1,1])
          lcColumnName  = SUBSTR(laClmnInfo[1,1] ,lnDashPos+1)

          IF TYPE("GLOBALBROWSEWINDOW.AriaGrid1."+lcColumnName) = "O" AND TYPE("GLOBALBROWSEWINDOW.AriaGrid1."  + lcColumnName  + ".columnorder") # "U"
            *-- get the column name to be used
            lcXMLString = "GLOBALBROWSEWINDOW.AriaGrid1."+lcColumnName
            *-- restore the column order
            *-- Is global browse in mulit select mode?
            IF TYPE("GLOBALBROWSEWINDOW.MultiSelect") = 'L' .AND. GLOBALBROWSEWINDOW.MULTISELECT
              *-- Is column order not equil mulit select column order
              IF TYPE("VAL(laClmnInfo[2,1])") = 'N' .AND. !VAL(laClmnInfo[2,1]) = 1
                &lcXMLString..COLUMNORDER = VAL(laClmnInfo[2,1])
              ENDIF
            ELSE
              &lcXMLString..COLUMNORDER = VAL(laClmnInfo[2,1])
            ENDIF

            *-- restore the column width
            &lcXMLString..WIDTH = VAL(laClmnInfo[3,1])
          ENDIF
        ENDFOR
        FOR lnCount = 1 TO GLOBALBROWSEWINDOW.AriaGrid1.COLUMNCOUNT
          *-- Is current column mulit select column?
          IF TYPE("GLOBALBROWSEWINDOW.MultiSelect") = 'L' .AND. GLOBALBROWSEWINDOW.MULTISELECT .AND. ;
              UPPER(ALLTRIM(GLOBALBROWSEWINDOW.AriaGrid1.COLUMNS[lnCount].NAME)) == UPPER("chkMultiSelect")
            LOOP
          ENDIF
          *-- get the column name,columnorder and width
          GLOBALBROWSEWINDOW.AriaGrid1.COLUMNS[lnCount].NAME = GLOBALBROWSEWINDOW.AriaGrid1.COLUMNS[lnCount].TAG
        ENDFOR
        IF TYPE('laPrefData[5,1]')='C'
          GLOBALBROWSEWINDOW.Restoreformposition(laPrefData[5,1])
        ENDIF

        *               It can be applied only in case existance of object link.
        *-- IF TYPE('laPrefData[6,1]')='C'
        IF TYPE('laPrefData[6,1]')='C' .AND. ;
            TYPE("GLOBALBROWSEWINDOW.cPictType") = 'C' .AND. !EMPTY(GLOBALBROWSEWINDOW.cPictType) .AND. ;
            TYPE("GLOBALBROWSEWINDOW.cNoteType") = 'C' .AND. !EMPTY(GLOBALBROWSEWINDOW.cNoteType) .AND. ;
            TYPE("GLOBALBROWSEWINDOW.cPictKey") = 'C' .AND.  !EMPTY(GLOBALBROWSEWINDOW.cPictKey)

          LOCAL lcCommand
          lcCommand = ALLTRIM(laPrefData[6,1])

          lcCommand = STRTRAN(UPPER(lcCommand), UPPER('GLOBALBROWSEWINDOW.cntProperties.Visible'), 'GLOBALBROWSEWINDOW.lPropertiesStatus')


          TRY
            &lcCommand.
          CATCH
          ENDTRY
        ENDIF
      ENDIF
    ENDIF
    GLOBALBROWSEWINDOW.AriaGrid1.SETALL('Size', .T. , 'ariagridmemocolumn')
    *! B609028,2 MMT 10/13/2009 Fix bug of Wrong Order of exporting Task notes to excel[End]
    
*  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [Start]
FUNCTION lfOpnTr
*B609143,2 MMT 03/03/2010 Fix problem of PO Screen Toolbar is no working when called from project Monitor[Start]
*TRY 
*B609143,2 MMT 03/03/2010 Fix problem of PO Screen Toolbar is no working when called from project Monitor[End]
IF TYPE('loParentForm.LATEMPFL[1,1]') <>'U' AND !EMPTY(loParentForm.LATEMPFL[1,1]) AND USED(loParentForm.LATEMPFL[1,1])
  lcPrjTyp = EVALUATE(loParentForm.LATEMPFL[1,1]+'.CPRJ_TYP')
  lcValue = EVALUATE(lcTaskIdCnt)
  lnDotPos = ATC('.',lcTaskIdCnt)
  lcStyleId  =   EVALUATE(loParentForm.LATEMPFL[1,1]+'.CSTYLE')
  lnLineNo =    EVALUATE(loParentForm.LATEMPFL[1,1]+'.LINENO')
  lcFldName = SUBSTR(lcTaskIdCnt,lnDotPos +1)
  *  E302650,2 MMT 12/24/2009 make project monitor work based on report templated selected [Start]
  IF EMPTY(lcValue )
    RETURN 
  ENDIF 
  IF !INLIST(padr(lcFldName ,10),PADR('CPRJ_ID',10), PADR('ORDER',10)  ,PADR('PO',10),PADR('STYLE',10),;
                                               PADR('VENDOR',10) , PADR('ACCOUNT',10),PADR('STORE',10),PADR('CVENDCODE',10)) 
    IF ASCAN(loFormSet.LAFLDMAP,PADR(lcFldName,10) ,1,0,1)>0
       lnFldArPos  = ASUBSCRIPT(loFormSet.LAFLDMAP,ASCAN(loFormSet.LAFLDMAP,PADR(lcFldName,10) ,1,0,1),1)
       lcFldOrgName  = loFormSet.LAFLDMAP[lnFldArPos  ,2]
       lnDotPOrg = ATC('.',lcFldOrgName  )
       lcFldName = SUBSTR(lcFldOrgName  ,lnDotPOrg +1)
    ENDIF                                            
  ENDIF 
  *  E302650,2 MMT 12/24/2009 make project monitor work based on report templated selected [End]
  *  E302650,3 MMT 12/29/2009 make project monitor work based on report templated selected [Start]
  IF TYPE('GLOBALBROWSEWINDOW') = 'O'
    GLOBALBROWSEWINDOW.HIDE()
    GLOBALBROWSEWINDOW.SHOW(0)
  ENDIF
  *  E302650,3 MMT 12/29/2009 make project monitor work based on report templated selected [End]
  DO CASE 
    CASE UPPER(ALLTRIM(lcFldName)) = 'ACCOUNT' OR UPPER(ALLTRIM(lcFldName)) = 'STORE'
      IF UPPER(ALLTRIM(lcFldName)) = 'ACCOUNT' 
        oAriaApplication.DoProgram("AWRARCUST",'"'+lcValue+'"',.F.,'')
      ELSE
        lcAccount = EVALUATE(loParentForm.LATEMPFL[1,1]+'.ACCOUNT')
        oAriaApplication.DoProgram("AWRARCUST",'"'+PADR(lcAccount,5) +'","'+PADR(lcValue,8)+'"',.F.,'')
      ENDIF   
      *  E302650,3 MMT 12/29/2009 make project monitor work based on report templated selected [Start]
      loTRParen= _SCREEN.ACTIVEFORM.PARENT
      loTRParen.HIDE()
      loTRParen.SHOW(1)
      *  E302650,3 MMT 12/29/2009 make project monitor work based on report templated selected [End]

    CASE UPPER(ALLTRIM(lcFldName)) = 'STYLE'
       IF lcPrjTyp  = 'M'
         *Call Material Screen   
         PRIVATE lcICInvType
         lcICInvType = '0002'
         =oAriaApplication.DoProgram('AWRICITEM','"0002","'+lcValue+"'",.F.,.F.)
       ELSE && Call Style Screen
         *  B609631,1 MMT 06/26/2011 Project Monitor should display 1 line for style color in Extended Size Scale companies[START]          
         IF llExtendedScale AND SUBSTR(CSTYLE,1,lnmajorlen) = REPLICATE('*',lnmajorlen)
           =gfSeek(ALLTRIM(lcValue),'STYLE','STYLE')
           lcValue = STYLE.STYlE
         ENDIF
         *  B609631,1 MMT 06/26/2011 Project Monitor should display 1 line for style color in Extended Size Scale companies[END]          
         oAriaApplication.DoProgram("AWRICSTYLE",'"'+lcValue+'"',.F.,'IC')
       ENDIF    
      *  E302650,3 MMT 12/29/2009 make project monitor work based on report templated selected [Start]
      loTRParen= _SCREEN.ACTIVEFORM.PARENT
      loTRParen.HIDE()
      loTRParen.SHOW(1)
      *  E302650,3 MMT 12/29/2009 make project monitor work based on report templated selected [End]
      
    CASE UPPER(ALLTRIM(lcFldName)) = 'PO'
      
      IF lcPrjTyp  <> 'O'
        *  E302650,2 MMT 12/24/2009 make project monitor work based on report templated selected [Start]
        IF lcPrjTyp  $ 'PN'
          oAriaApplication.ActiveModuleID  = 'PO'
        ELSE
          oAriaApplication.ActiveModuleID  = 'MF'
        ENDIF 
        *  E302650,2 MMT 12/24/2009 make project monitor work based on report templated selected [End]
        lfGetProjID(lcPrjTyp ,lcValue)
        *B609220,1 MMT 04/26/2010 PO and SO screens toolbar is not working when called from project Monitor[Start]        
        loTRParen= _SCREEN.ACTIVEFORM.PARENT
        loTRParen.HIDE()
        loTRParen.SHOW(1)
        *B609220,1 MMT 04/26/2010 PO and SO screens toolbar is not working when called from project Monitor[End]                
      ELSE
        SELECT CUTPICK
        =gfSetOrder('CUTORD')    
        llFound = .F.
        lcOrderId = EVALUATE(loParentForm.LATEMPFL[1,1]+'.CPRJ_ID')
        IF gfSeek('1'+lcOrderId ) OR gfSeek('2'+lcOrderId) 
          IF gfSeek('1'+lcOrderId ) 
            LOCATE REST WHILE TRANCD+ORDER+CORDLINE = '1'+lcOrderId FOR CTKTNO= lcValue
            IF FOUND()
              llFound = .T.
              oAriaApplication.ActiveModuleID  = 'MF'
              lfGetProjID('C' ,lcValue) 
              *B609220,1 MMT 04/26/2010 PO and SO screens toolbar is not working when called from project Monitor[Start]        
              loTRParen= _SCREEN.ACTIVEFORM.PARENT
              loTRParen.HIDE()
              loTRParen.SHOW(1)
              *B609220,1 MMT 04/26/2010 PO and SO screens toolbar is not working when called from project Monitor[End]                
                   
            ELSE
              =gfSeek('2'+lcOrderId) 
              LOCATE REST WHILE TRANCD+ORDER+CORDLINE = '2'+lcOrderId FOR CTKTNO= lcValue
              IF FOUND()
                llFound = .T.
                oAriaApplication.ActiveModuleID  = 'PO'
                lfGetProjID('P' ,lcValue)      
                *B609220,1 MMT 04/26/2010 PO and SO screens toolbar is not working when called from project Monitor[Start]        
                loTRParen= _SCREEN.ACTIVEFORM.PARENT
                loTRParen.HIDE()
                loTRParen.SHOW(1)
                *B609220,1 MMT 04/26/2010 PO and SO screens toolbar is not working when called from project Monitor[End]                
                
              ENDIF 
            ENDIF 
          ELSE
            =gfSeek('2'+lcOrderId) 
            LOCATE REST WHILE TRANCD+ORDER+CORDLINE = '2'+lcOrderId FOR CTKTNO= lcValue
            IF FOUND()
              llFound = .T.
              oAriaApplication.ActiveModuleID  = 'PO'
              lfGetProjID('P' ,lcValue)      
              *B609220,1 MMT 04/26/2010 PO and SO screens toolbar is not working when called from project Monitor[Start]        
              loTRParen= _SCREEN.ACTIVEFORM.PARENT
              loTRParen.HIDE()
              loTRParen.SHOW(1)
              *B609220,1 MMT 04/26/2010 PO and SO screens toolbar is not working when called from project Monitor[End]                
              
            ENDIF 
          ENDIF   
        ENDIF 
      ENDIF   
     
    CASE UPPER(ALLTRIM(lcFldName)) = 'ORDER'
       oAriaApplication.ActiveModuleID  = 'SO'
       lfGetProjID('O',lcValue)    
       *B609220,1 MMT 04/26/2010 PO and SO screens toolbar is not working when called from project Monitor[Start]        
       loTRParen= _SCREEN.ACTIVEFORM.PARENT
       loTRParen.HIDE()
       loTRParen.SHOW(1)
       *B609220,1 MMT 04/26/2010 PO and SO screens toolbar is not working when called from project Monitor[End]                
      
    CASE UPPER(ALLTRIM(lcFldName)) = 'VENDOR' OR UPPER(ALLTRIM(lcFldName)) = 'CVENDCODE'
       *  E302650,2 MMT 12/24/2009 make project monitor work based on report templated selected [Start]
       * OPen location scree in case interlocation PO and field is vendor
       IF lcPrjTyp = 'N' AND UPPER(ALLTRIM(lcFldName)) = 'VENDOR'
         
       ELSE 
       *  E302650,2 MMT 12/24/2009 make project monitor work based on report templated selected [End]
         DO FORM (oAriaApplication.ScreenHome+"APVENDR.SCX") WITH lcValue NAME loVendForm
       *  E302650,2 MMT 12/24/2009 make project monitor work based on report templated selected [Start]
       ENDIF 
       *  E302650,2 MMT 12/24/2009 make project monitor work based on report templated selected [End]
      *  E302650,3 MMT 12/29/2009 make project monitor work based on report templated selected [Start]
      loTRParen= _SCREEN.ACTIVEFORM.PARENT
      loTRParen.HIDE()
      loTRParen.SHOW(1)
     *  E302650,3 MMT 12/29/2009 make project monitor work based on report templated selected [End]

    CASE UPPER(ALLTRIM(lcFldName)) = 'CPRJ_ID'
      
      DO CASE 
        CASE lcPrjTyp  $ 'PNR'
          oAriaApplication.ActiveModuleID  = 'PO'
        CASE lcPrjTyp  $ 'CAD'
          oAriaApplication.ActiveModuleID  = 'MF'
        CASE lcPrjTyp  $ 'OT'
          oAriaApplication.ActiveModuleID  = 'SO'
        CASE lcPrjTyp  $ 'SH'
          oAriaApplication.ActiveModuleID  = 'IC'
        OTHERWISE 
          oAriaApplication.ActiveModuleID  = 'MA'
      ENDCASE   
      
      =oAriaApplication.DoProgram('AWRMFPROJ',"'"+lcPrjTyp+"','"+lcValue+"','"+lcStyleId+"',VAL('"+STR(lnLineNo ,6)+"'),'',.F.,.F.,.T.",.F.,.F.)
      *  E302650,3 MMT 12/29/2009 make project monitor work based on report templated selected [Start]
      loTRParen= _SCREEN.ACTIVEFORM.PARENT
      loTRParen.HIDE()
      loTRParen.SHOW(1)
      *  E302650,3 MMT 12/29/2009 make project monitor work based on report templated selected [End]
  ENDCASE   
*  E302650,3 MMT 12/29/2009 make project monitor work based on report templated selected [Start]
*!*    loTRParen= _SCREEN.ACTIVEFORM.PARENT
*!*    loTRParen.HIDE()
*!*    loTRParen.SHOW(1)
*  E302650,3 MMT 12/29/2009 make project monitor work based on report templated selected [End]
ENDIF 
*B609143,2 MMT 03/03/2010 Fix problem of PO Screen Toolbar is no working when called from project Monitor[Start]
*CATCH
*ENDTRY 
*B609143,2 MMT 03/03/2010 Fix problem of PO Screen Toolbar is no working when called from project Monitor[End]
 
RETURN

FUNCTION lfOrdBrowExp
IF TYPE('GLOBALBROWSEWINDOW')    = 'O'
  FOR lnA = 1 TO GLOBALBROWSEWINDOW.AriaGrid1.COLUMNCOUNT
    lcTask =  GLOBALBROWSEWINDOW.AriaGrid1.COLUMNS(lnA).ControlSource
    IF !('CTSKST' $ UPPER(lcTask))
      LOOP
    ENDIF
    lcNotesField  = STRTRAN(UPPER(lcTask),'CTSKST','MOPCOM')
    lnDotPos = ATC('.',lcNotesField)
    lcNoteFld = ''
    IF lnDotPos  > 0
      lcNoteFld = substR(lcNotesField  ,lnDotPos+1)
    ENDIF 
    FOR lnC = 1 TO ALEN(loFormSet.LATEMPFL,1)
      lcFldName = loFormSet.LATEMPFL[lnC,1]+'.'+ALLTRIM(lcNoteFld)
      IF TYPE(lcFldName) = 'U'
        LOOP
      ELSE
        lcNotesField  = loFormSet.LATEMPFL[lnC,1]+'.'+ALLTRIM(lcNoteFld)
        EXIT 
      ENDIF
    ENDFOR
    GLOBALBROWSEWINDOW.AriaGrid1.COLUMNS(lnA).ControlSource = lcNotesField 
  ENDFOR
ENDIF
    
FUNCTION lfReOrdBrowExp    
IF TYPE('GLOBALBROWSEWINDOW')    = 'O'
  FOR LNC = 1 TO GLOBALBROWSEWINDOW.ARIAGRID1.COLUMNCOUNT
    LCTASK =  GLOBALBROWSEWINDOW.ARIAGRID1.COLUMNS(LNC).CONTROLSOURCE
    IF !('MOPCOM' $ UPPER(LCTASK))
      LOOP
    ENDIF
    LCNOTESFIELD  = STRTRAN(UPPER(LCTASK),'MOPCOM','CTSKST')
    LNDOTPOS = ATC('.',LCNOTESFIELD)
    LCNOTEFLD = ''
    IF LNDOTPOS  > 0
      LCNOTEFLD = SUBSTR(LCNOTESFIELD  ,LNDOTPOS+1)
    ENDIF 
    LCNOTESFIELD = GLOBALBROWSEWINDOW.ALIAS+'.'+LCNOTEFLD 
    GLOBALBROWSEWINDOW.ARIAGRID1.COLUMNS(LNC).CONTROLSOURCE = LCNOTESFIELD 
    IF TYPE('GLOBALBROWSEWINDOW.ARIAGRID1.COLUMNS[LNE].CONTROLS(GLOBALBROWSEWINDOW.ARIAGRID1.COLUMNS[LNC].CONTROLCOUNT).TXTBOX')  ='O'
      GLOBALBROWSEWINDOW.ARIAGRID1.COLUMNS[LNC ].CONTROLS(GLOBALBROWSEWINDOW.ARIAGRID1.COLUMNS[LNC ].CONTROLCOUNT).TXTBOX.CONTROLSOURCE = LCNOTESFIELD 
    ENDIF   
  ENDFOR
ENDIF


FUNCTION lfColumnDown
loIndCol = SYS(1270)
lcContSrc = loIndCol.ControlSource
lnDotPos = ATC('.',lcContSrc)
lcCursName = SUBSTR(lcContSrc ,1,lnDotPos -1)
lcColumnID = SUBSTR(lcContSrc ,lnDotPos+1)
lcFldType = TYPE(lcContSrc)

*!*  IF !EMPTY(loParentForm.lcLastConts) AND UPPER(ALLTRIM(loParentForm.lcLastConts)) <> UPPER(ALLTRIM(lcContSrc)) AND lcFldType $ 'LCND'
*!*    lcFldName = SUBSTR(loParentForm.lcLastConts,ATC('.',loParentForm.lcLastConts)+1)
*!*    lcTypCol = TYPE(loParentForm.lcLastConts)
*!*    lnPosIndArr = 0
*!*    DO CASE 
*!*      CASE lcTypCol = 'C'
*!*        lnPosIndArr = ASCAN(GLOBALBROWSEWINDOW.laUDFSortColumns,'CCharacter',1,0,1)
*!*      CASE lcTypCol = 'N'  
*!*        lnPosIndArr = ASCAN(GLOBALBROWSEWINDOW.laUDFSortColumns,'NNumric',1,0,1)    
*!*      CASE lcTypCol = 'L'
*!*        lnPosIndArr = ASCAN(GLOBALBROWSEWINDOW.laUDFSortColumns, 'LLOGICAL',1,0,1)
*!*      CASE lcTypCol = 'D'
*!*        lnPosIndArr = ASCAN(GLOBALBROWSEWINDOW.laUDFSortColumns,'DDATEFLD',1,0,1)    
*!*    ENDCASE   
*!*    
*!*    IF lnPosIndArr > 0 AND !EMPTY(lcFldName)
*!*      lnPosIndArr = ASUBSCRIPT(GLOBALBROWSEWINDOW.laUDFSortColumns,lnPosIndArr ,1)
*!*      GLOBALBROWSEWINDOW.laUDFSortColumns[lnPosIndArr ,1] = lcFldName 
*!*    ENDIF 
*!*   
*!*  ENDIF 
*!*  IF UPPER(ALLTRIM(lcCursName)) <> UPPER(ALLTRIM(loParentForm.LATEMPFL[1,1])) AND lcFldType $ 'LCND'
*!*    SELECT (loParentForm.LATEMPFL[1,1])
*!*    lcNewFldName = ''
*!*    DO CASE 
*!*      CASE lcFldType  = 'C'
*!*        REPLACE ALL CCharacter WITH &lcContSrc.
*!*        REPLACE ALL CCharacter WITH &lcContSrc. IN (GLOBALBROWSEWINDOW.lcOldAlias)
*!*        lcNewFldName = 'CCharacter'
*!*      CASE lcFldType  = 'N'
*!*        REPLACE ALL NNumric WITH &lcContSrc.
*!*        REPLACE ALL NNumric WITH &lcContSrc.  IN (GLOBALBROWSEWINDOW.lcOldAlias)
*!*        lcNewFldName = 'NNumric'
*!*      CASE lcFldType  = 'D'
*!*        REPLACE ALL DDATEFLD WITH &lcContSrc.
*!*        REPLACE ALL DDATEFLD WITH &lcContSrc.      IN (GLOBALBROWSEWINDOW.lcOldAlias) 
*!*        lcNewFldName = 'DDATEFLD'
*!*      CASE lcFldType  = 'L'
*!*        REPLACE ALL LLOGICAL WITH &lcContSrc.
*!*        REPLACE ALL LLOGICAL WITH &lcContSrc.       IN (GLOBALBROWSEWINDOW.lcOldAlias)
*!*        lcNewFldName = 'LLOGICAL'
*!*    ENDCASE   
*!*    IF !EMPTY(lcNewFldName) AND lcFldType $ 'CLND'
*!*      loParentForm.lcLastConts =lcContSrc 
*!*      loIndCol.ControlSource =loParentForm.LATEMPFL[1,1]+'.'+ lcNewFldName 
*!*      IF  ASCAN(GLOBALBROWSEWINDOW.aFileFields,lcContSrc,1,0,2) > 0
*!*        lnColPos = ASUBSCRIPT(GLOBALBROWSEWINDOW.aFileFields,ASCAN(GLOBALBROWSEWINDOW.aFileFields,lcContSrc,1,0,2),1)
*!*        GLOBALBROWSEWINDOW.aFileFields[lnColPos , 2]= loIndCol.ControlSource
*!*      ENDIF   
*!*    ENDIF   
*!*  ENDIF 


FUNCTION  lfReColumndown
*!*  loIndCol = SYS(1270)
TRY 
*!*    IF !EMPTY(loParentForm.lcLastConts) AND ( 'CCHARACTER' $ UPPER(loIndCol.ControlSource) OR ;
*!*                                              'NNUMRIC' $ UPPER(loIndCol.ControlSource) OR ; 
*!*                                              'DDATEFLD' $ UPPER(loIndCol.ControlSource) OR ;
*!*                                              'LLOGICAL'$ UPPER(loIndCol.ControlSource))
*!*      loIndCol.ControlSource = loParentForm.lcLastConts
*!*      GLOBALBROWSEWINDOW.lnCustomSortColumnCount = 1
*!*    ENDIF 

  FOR lnE = 1 TO GLOBALBROWSEWINDOW.AriaGrid1.ColumnCount
    lcContSource = GLOBALBROWSEWINDOW.AriaGrid1.Columns[lnE].ControlSource
    IF !EMPTY(lcContSource) AND (UPPER(ALIAS()) $ UPPER(lcContSource)) AND;
       ('CTSKST' $ UPPER(lcContSource) OR ;
       'PO'$ UPPER(lcContSource) OR ;
       'ORDER'$ UPPER(lcContSource) OR ;
       'STYLE'$ UPPER(lcContSource) OR ;
       'ACCOUNT'$ UPPER(lcContSource) OR ;
       'STORE'$ UPPER(lcContSource) OR ;
       'VENDOR'$ UPPER(lcContSource) OR ;
       'CPRJ_ID'$ UPPER(lcContSource) OR ;
       'CVENDCODE'$ UPPER(lcContSource) )

      IF TYPE('GLOBALBROWSEWINDOW.AriaGrid1.Columns[lnE].Controls(GLOBALBROWSEWINDOW.AriaGrid1.Columns[lnE].ControlCount).txtBox')  ='O'
        GLOBALBROWSEWINDOW.AriaGrid1.Columns[lnE].Controls(GLOBALBROWSEWINDOW.AriaGrid1.Columns[lnE].ControlCount).txtBox.ControlSource = lcContSource 
      ENDIF   
      loParentForm.LATEMPFL[1,1] = ALIAS()
      FOR lnT = 1 TO ALEN(loParentForm.LATEMPFL,1)
        IF !EMPTY(loParentForm.LATEMPFL[lnT,1])
          x= 'T'+ALLTRIM(STR(lnT))
          &x. = loParentForm.LATEMPFL[lnT,1]
          loParentForm.LATEMPFL[lnT,2] = x
        ENDIF
      ENDFOR
    ENDIF 
  ENDFOR
  GLOBALBROWSEWINDOW.Refresh()
  SELECT (loParentForm.LATEMPFL[1,1]) 
CATCH
ENDTRY 

FUNCTION lfgetfilefields
Local lnCount
DECLARE GLOBALBROWSEWINDOW.laAvForUDFSort[1, 1]
STORE .F. TO GLOBALBROWSEWINDOW.laAvForUDFSort

DIMENSION GLOBALBROWSEWINDOW.aFileFields[1, 4]
STORE .F. TO GLOBALBROWSEWINDOW.aFileFields
lnCount = 0

LOCAL lnCount1
LOCAL lnCount2
FOR lnCount2 = 1 TO GLOBALBROWSEWINDOW.Ariagrid1.ColumnCount
  
  FOR lnCount1 = 1 TO GLOBALBROWSEWINDOW.Ariagrid1.ColumnCount
    IF GLOBALBROWSEWINDOW.Ariagrid1.Columns(lnCount1).ColumnOrder  = lnCount2
      EXIT
    ENDIF
  ENDFOR

  IF TYPE("GLOBALBROWSEWINDOW.MultiSelect") = 'L' .AND. GLOBALBROWSEWINDOW.MultiSelect .AND. ;
     UPPER(ALLTRIM(GLOBALBROWSEWINDOW.Ariagrid1.Columns[lnCount1].Name)) == UPPER("chkMultiSelect")
     LOOP
  ENDIF
  IF TYPE(GLOBALBROWSEWINDOW.Ariagrid1.Columns[lnCount1].ControlSource) = 'M'
    LOOP 
  ENDIF 
  lnCount = lnCount + 1
  DIMENSION GLOBALBROWSEWINDOW.aFileFields[lnCount, 4]
  
  IF EMPTY(GLOBALBROWSEWINDOW.Ariagrid1.Columns[lnCount1].Header1.Caption)
    GLOBALBROWSEWINDOW.aFileFields[lnCount, 1] = STRTRAN(GLOBALBROWSEWINDOW.Ariagrid1.Columns[lnCount1].ControlSource, ;
                                           GLOBALBROWSEWINDOW.Alias + '.', ;
                                           '', -1, -1, 1)
  ELSE
    GLOBALBROWSEWINDOW.aFileFields[lnCount, 1] = GLOBALBROWSEWINDOW.Ariagrid1.Columns[lnCount1].Header1.Caption
  ENDIF
  GLOBALBROWSEWINDOW.aFileFields[lnCount, 2] = GLOBALBROWSEWINDOW.Ariagrid1.Columns[lnCount1].ControlSource
  
  GLOBALBROWSEWINDOW.aFileFields[lnCount, 3] = TYPE(EVAL('GLOBALBROWSEWINDOW.aFileFields[lnCount,2]'))
  IF GLOBALBROWSEWINDOW.IsExpression(GLOBALBROWSEWINDOW.aFileFields[lnCount,2]) THEN
    GLOBALBROWSEWINDOW.aFileFields[lnCount,4] = 'E'
    lcComFld = GLOBALBROWSEWINDOW.aFileFields[lnCount,2]
    TRY 
      X = &lcComFld.
    CATCH
    ENDTRY
    GLOBALBROWSEWINDOW.aFileFields[lnCount,3] = TYPE('X')    
    
  ELSE
    GLOBALBROWSEWINDOW.aFileFields[lnCount,4] = 'F'
  ENDIF
ENDFOR

GLOBALBROWSEWINDOW.nCustomColumnCount = 0

IF GLOBALBROWSEWINDOW.Container1.chkColumn.Enabled
  LOCAL lnSelected
  lnSelected = SELECT()
  
  LOCAL lcTempName, lnResult
  lcTempName = gfTempName()
  *E303030,1 BEGIN
*!*	  lnResult = oAriaApplication.RemoteSystemData.SQLRun( ;
*!*	             'SELECT SYDFIELD.CFLD_NAME, SYDFIELD.CFLD_HEAD, SYDFIELD.CDATA_TYP FROM SYDFIELD,SYDFLFLD ' + ;
*!*	             "WHERE SYDFIELD.cfld_name = SYDFLFLD.cfld_name AND SYDFLFLD.cfile_nam = '" + ;
*!*	             PADR(GLOBALBROWSEWINDOW.lcDbfName, 8) + "' ORDER BY SYDFIELD.CFLD_HEAD", ;
*!*	             lcTempName, ;
*!*	             '', ;
*!*	             oAriaApplication.SystemConnectionString, ;
*!*	             3, ;
*!*	             'SAVE', ;
*!*	             GLOBALBROWSEWINDOW.DataSessionId)
  lnResult = oAriaApplication.RemoteSystemData.SQLRun( ;
             'SELECT SYDFIELD.CFLD_NAME, SYDFIELD.CFLD_HEAD, SYDFIELD.CDATA_TYP FROM SYDFIELD,SYDFLFLD ' + ;
             "WHERE SYDFIELD.cfld_name = SYDFLFLD.cfld_name AND SYDFLFLD.cfile_nam = '" + ;
             PADR(GLOBALBROWSEWINDOW.lcDbfName, oAriaApplication.FileW) + "' ORDER BY SYDFIELD.CFLD_HEAD", ;
             lcTempName, ;
             '', ;
             oAriaApplication.SystemConnectionString, ;
             3, ;
             'SAVE', ;
             GLOBALBROWSEWINDOW.DataSessionId)
  *E303030,1 END
  
  DELETE FOR ASCAN(GLOBALBROWSEWINDOW.aFileFields, GLOBALBROWSEWINDOW.Alias + '.' + ALLTRIM(CFLD_NAME), -1, -1, 2, 7) > 0
  DELETE FOR ASCAN(GLOBALBROWSEWINDOW.aFileFields, ALLTRIM(CFLD_NAME), -1, -1, 2, 7) > 0
  
  IF lnResult = 1
    GLOBALBROWSEWINDOW.nCustomColumnCount = 0
    
    SCAN
      LOCAL lnRowCount
      lnRowCount = ALEN(GLOBALBROWSEWINDOW.aFileFields, 1) + 1
      DIMENSION GLOBALBROWSEWINDOW.aFileFields[lnRowCount, 4]
      
      GLOBALBROWSEWINDOW.aFileFields[lnRowCount, 1] = CFLD_HEAD
      GLOBALBROWSEWINDOW.aFileFields[lnRowCount, 2] = GLOBALBROWSEWINDOW.Alias + '.' + ALLTRIM(CFLD_NAME)
      GLOBALBROWSEWINDOW.aFileFields[lnRowCount, 3] = CDATA_TYP
      GLOBALBROWSEWINDOW.aFileFields[lnRowCount, 4] = 'F'
      
      GLOBALBROWSEWINDOW.nCustomColumnCount = GLOBALBROWSEWINDOW.nCustomColumnCount + 1
    ENDSCAN
     
    USE IN (lcTempName)
     
    SELECT (lnSelected)
  ELSE
    oAriaApplication.RemoteSystemData.CheckRetResult('SQLRun', lnResult, .T.)
    
    SELECT (lnSelected)
    
    RETURN .F.
  ENDIF
ENDIF 
RETURN .T.

FUNCTION lfGetTNot
  LPARAMETERS nButton, nShift, nXCoord, nYCoord
  *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
  *TRY
  *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
    lcCurrentTask = lcTaskIdCnt
    lnCol = 0
    IF !('CTSKST' $ UPPER(lcCurrentTask))
      RETURN
    ENDIF
    IF TYPE('GLOBALBROWSEWINDOW')    <> 'O'
      RETURN
    ELSE
      FOR lnW = 1 TO GLOBALBROWSEWINDOW.AriaGrid1.COLUMNCOUNT
        *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
        *IF UPPER(lcCurrentTask) =  UPPER(GLOBALBROWSEWINDOW.AriaGrid1.COLUMNS(lnW).CONTROLSOURCE)
        IF UPPER(lcCurrentTask) ==  UPPER(GLOBALBROWSEWINDOW.AriaGrid1.COLUMNS(lnW).CONTROLSOURCE)
        *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
          lnCol = lnW
          EXIT 
        ENDIF
      ENDFOR
    ENDIF
    IF lnCol = 0
      RETURN
    ENDIF
    
    lcNotesField  = STRTRAN(UPPER(lcCurrentTask),'CTSKST','MOPCOM')
    lnDotPos = ATC('.',lcNotesField)
    lcNoteFld = ''
    IF lnDotPos  > 0
      lcNoteFld = substR(lcNotesField  ,lnDotPos+1)
    ENDIF 
    FOR lnC = 1 TO ALEN(loFormSet.LATEMPFL,1)
      lcFldName = loFormSet.LATEMPFL[lnC,1]+'.'+ALLTRIM(lcNoteFld)
      IF TYPE(lcFldName) = 'U'
        LOOP
      ELSE
        lcNotesField  = loFormSet.LATEMPFL[lnC,1]+'.'+ALLTRIM(lcNoteFld)
        EXIT 
      ENDIF
    ENDFOR
    lcNots = EVALUATE(lcNotesField)
    IF ATC(CHR(10),lcNots) > 0
      lcNotes = SUBSTR(lcNots,ATC(CHR(10),lcNots)+1)
    ELSE
      lcNotes = lcNots 
    ENDIF 
    IF !EMPTY(lcNotes)
      GLOBALBROWSEWINDOW.AriaGrid1.COLUMNS(lnCol).CONTROLS(GLOBALBROWSEWINDOW.AriaGrid1.COLUMNS(lnCol).CONTROLCOUNT).TOOLTIPTEXT = lcNotes 
      GLOBALBROWSEWINDOW.AriaGrid1.COLUMNS(lnCol).CONTROLS(GLOBALBROWSEWINDOW.AriaGrid1.COLUMNS(lnCol).CONTROLCOUNT).txtBox.TOOLTIPTEXT = lcNotes 
      GLOBALBROWSEWINDOW.AriaGrid1.COLUMNS(lnCol).CONTROLS(GLOBALBROWSEWINDOW.AriaGrid1.COLUMNS(lnCol).CONTROLCOUNT).cmdButton.TOOLTIPTEXT = lcNotes 
    ELSE
      GLOBALBROWSEWINDOW.AriaGrid1.COLUMNS(lnCol).CONTROLS(GLOBALBROWSEWINDOW.AriaGrid1.COLUMNS(lnCol).CONTROLCOUNT).TOOLTIPTEXT = ''
      GLOBALBROWSEWINDOW.AriaGrid1.COLUMNS(lnCol).CONTROLS(GLOBALBROWSEWINDOW.AriaGrid1.COLUMNS(lnCol).CONTROLCOUNT).txtBox.TOOLTIPTEXT = ''
      GLOBALBROWSEWINDOW.AriaGrid1.COLUMNS(lnCol).CONTROLS(GLOBALBROWSEWINDOW.AriaGrid1.COLUMNS(lnCol).CONTROLCOUNT).cmdButton.TOOLTIPTEXT = ''
    ENDIF   
      GLOBALBROWSEWINDOW.AriaGrid1.COLUMNS(lnCol).CONTROLS(GLOBALBROWSEWINDOW.AriaGrid1.COLUMNS(lnCol).CONTROLCOUNT).REFRESH
  *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]    
  *CATCH
  *ENDTRY
  *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
*E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [End]

ENDDEFINE

*!*************************************************************
*! Name      : lfTranBut
*! Developer : Mariam Mazhar
*! Date      : 06/01/2009
*! Purpose   : the button that will call transcation screens
*!*************************************************************
FUNCTION lfTranBut
  RETURN  '<VFP:CommandButton Delegate = "lfTranFrm" SourceControl = "cProjType" HandlerObject= "loParentForm"  Caption="..."'

  *!*************************************************************
  *! Name      : lfVldPrdc
  *: Developer : Mariam Mazhar[MMT]
  *: Date      : 06/01/2009
  *! Purpose   : Checks a predecessor whether it would create a
  *!             cyclic path or not.
  *!*************************************************************
  *! Called    : lfvMSource() in MAINPROC, from ARIAMOVR.SR
  *!*************************************************************
  *! Calls     : None.
  *!*************************************************************
  *! Passed Parameters  :
  *!*************************************************************
  *! Returns            : .T. if the current operation may be
  *!                      selected as a predecessor, .f. otherwise.
  *!*************************************************************
  *! Example            :  IF lfVldPrd(..)....
  *!*************************************************************
FUNCTION lfVldPrdc
  PARAMETERS lnSrcBtn,lnLstIndx

  IF lnSrcBtn = 3 OR lnSrcBtn = 4
    RETURN .T.
  ENDIF
  llMoveAll  = (lnSrcBtn = 2)
  lcArrCtg  = SUBSTR(IIF(lnLstIndx = 0,laSource[1],laSource[lnLstIndx]), 1, 3)
  lcArrOprt = SUBSTR(IIF(lnLstIndx = 0,laSource[1],laSource[lnLstIndx]), 5, 5)
  m.COPRT_DSC = &lc_PMPrjDt..COPRT_DSC
  SET PROCEDURE TO &lcSetProc
  lnRetVal = IIF(ASCAN(laPathElems, lcArrCtg + lcArrOprt) = 0, .T.,;
    IIF(gfModalGen("TRM38214B00000","DIALOG",ALLTRIM(lcArrCtg)+'\'+;
    ALLTRIM(lcArrOprt)) = 1, ;
    .F., .T.))
  IF !EMPTY(laTarget)
    lcPrevOrd = ORDER(lc_PMPrjRl)
    IF llMoveAll
      SET ORDER TO PMPRJRL IN (lc_PMPrjRl)
      
      *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [Start]
      *!*        IF lnRetVal AND (SEEK(lcPrj_typ+lcPrj_ID+SUBSTR(lcStyle,1,LEN(&lc_PMPrjRl..cStyle))+lcOprt_ctg+lcOprt_ID,lc_PMPrjRl) AND;
      *!*                         SEEK(lcPrj_typ+lcPrj_ID+SUBSTR(lcStyle,1,LEN(&lc_PMPrjRl..cStyle))+&lc_PMPrjRl..Cprd_Ctg+&lc_PMPrjRl..Cprd_ID,lc_PMPrjRl)) AND ;
      *!*                         (lcArrCtg = &lc_PMPrjRl..Cprd_Ctg AND lcArrOprt = &lc_PMPrjRl..Cprd_ID)
      IF lnRetVal AND (SEEK(lcPrj_Typ+lcPrj_ID+SUBSTR(lcStyle,1,LEN(&lc_PMPrjRl..cstyle))+STR(lnLineNo,6)+lcOprt_ctg+lcOprt_ID,lc_PMPrjRl) AND;
          SEEK(lcPrj_Typ+lcPrj_ID+SUBSTR(lcStyle,1,LEN(&lc_PMPrjRl..cstyle))+STR(lnLineNo,6)+&lc_PMPrjRl..Cprd_Ctg+&lc_PMPrjRl..Cprd_ID,lc_PMPrjRl)) AND ;
          (lcArrCtg = &lc_PMPrjRl..Cprd_Ctg AND lcArrOprt = &lc_PMPrjRl..Cprd_ID)
      *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [End]
        =gfModalGen("TRM38241B00000","DIALOG",;
          lcArrCtg+"|"+lcArrOprt+"|"+lcOprt_ctg+"|"+lcOprt_ID+"|"+ALLTRIM(m.COPRT_DSC))
        lnRetVal = .F.
      ELSE
        SET ORDER TO PMPRJRLP IN (lc_PMPrjRl)
        *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [Start]
        *!*          IF lnRetVal AND SEEK(lcPrj_typ+lcPrj_ID+SUBSTR(lcStyle,1,LEN(&lc_PMPrjRl..cStyle))+lcArrCtg+lcArrOprt,lc_PMPrjRl) AND ASCAN(laTarget,&lc_PMPrjRl..COprt_Ctg+'\'+&lc_PMPrjRl..COprt_ID) <> 0)
        IF lnRetVal AND SEEK(lcPrj_Typ+lcPrj_ID+SUBSTR(lcStyle,1,LEN(&lc_PMPrjRl..cstyle))+STR(lnLineNo,6)+lcArrCtg+lcArrOprt,lc_PMPrjRl) AND ASCAN(laTarget,&lc_PMPrjRl..COPRT_CTG+'\'+&lc_PMPrjRl..COPRT_ID) <> 0)
        *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [End]
          =gfModalGen("TRM38241B00000","DIALOG",;
            lcArrCtg+"|"+lcArrOprt+"|"+lcOprt_ctg+"|"+lcOprt_ID+"|"+ALLTRIM(m.COPRT_DSC))
          lnRetVal = .F.
        ELSE
          SET ORDER TO PMPRJRL IN (lc_PMPrjRl)
          *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [Start]
          *IF lnRetVal AND SEEK(lcPrj_typ+lcPrj_ID+SUBSTR(lcStyle,1,LEN(&lc_PMPrjRl..cStyle))+lcArrCtg+lcArrOprt,lc_PMPrjRl) AND ASCAN(laTarget,&lc_PMPrjRl..CPrd_Ctg+'\'+&lc_PMPrjRl..CPrd_ID) <> 0)
          IF lnRetVal AND SEEK(lcPrj_Typ+lcPrj_ID+SUBSTR(lcStyle,1,LEN(&lc_PMPrjRl..cstyle))+;
              STR(lnLineNo,6)+lcArrCtg+lcArrOprt,lc_PMPrjRl);
              AND ASCAN(laTarget,&lc_PMPrjRl..Cprd_Ctg+'\'+&lc_PMPrjRl..Cprd_ID) <> 0)
          *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [End]
            =gfModalGen("TRM38242B00000","DIALOG",;
              lcOprt_ctg+"|"+lcOprt_ID+"|"+ALLTRIM(m.COPRT_DSC)+"|"+lcArrCtg+"|"+lcArrOprt)
            lnRetVal = .F.
          ENDIF
        ENDIF
      ENDIF
    ELSE
      SELECT  (lc_PMPrjRl)
      gfSetOrder('PMPRJRL')
      
      *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [Start]
      *!*        IF lnRetVal AND (SEEK(lcPrj_typ+lcPrj_ID+SUBSTR(lcStyle,1,LEN(&lc_PMPrjRl..cStyle))+lcOprt_ctg+lcOprt_ID,lc_PMPrjRl) AND;
      *!*                         SEEK(lcPrj_typ+lcPrj_ID+SUBSTR(lcStyle,1,LEN(&lc_PMPrjRl..cStyle))+&lc_PMPrjRl..Cprd_Ctg+&lc_PMPrjRl..Cprd_ID,lc_PMPrjRl)) AND ;
      *!*                         (lcArrCtg = &lc_PMPrjRl..Cprd_Ctg AND lcArrOprt = &lc_PMPrjRl..Cprd_ID)
      IF lnRetVal AND (SEEK(lcPrj_Typ+lcPrj_ID+SUBSTR(lcStyle,1,LEN(&lc_PMPrjRl..cstyle))+STR(lnLineNo,6)+lcOprt_ctg+lcOprt_ID,lc_PMPrjRl) AND;
          SEEK(lcPrj_Typ+lcPrj_ID+SUBSTR(lcStyle,1,LEN(&lc_PMPrjRl..cstyle))+STR(lnLineNo,6)+&lc_PMPrjRl..Cprd_Ctg+&lc_PMPrjRl..Cprd_ID,lc_PMPrjRl)) AND ;
          (lcArrCtg = &lc_PMPrjRl..Cprd_Ctg AND lcArrOprt = &lc_PMPrjRl..Cprd_ID)
      *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [End]
        =gfModalGen("TRM38243B00000","DIALOG",;
          lcOprt_ctg+"|"+lcOprt_ID+"|"+ALLTRIM(m.COPRT_DSC))
        lnRetVal = .F.
      ELSE
        SET ORDER TO PMPRJRLP IN (lc_PMPrjRl)
        *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [Start]
        *!*          IF lnRetVal AND SEEK(lcPrj_typ+lcPrj_ID+SUBSTR(lcStyle,1,LEN(&lc_PMPrjRl..cStyle))+lcArrCtg+lcArrOprt,lc_PMPrjRl) AND;
        *!*                          ASCAN(laTarget,&lc_PMPrjRl..COprt_Ctg+'\'+&lc_PMPrjRl..COprt_ID) <> 0
        IF lnRetVal AND SEEK(lcPrj_Typ+lcPrj_ID+SUBSTR(lcStyle,1,LEN(&lc_PMPrjRl..cstyle))+STR(lnLineNo,6)+;
            lcArrCtg+lcArrOprt,lc_PMPrjRl) AND;
            ASCAN(laTarget,&lc_PMPrjRl..COPRT_CTG+'\'+&lc_PMPrjRl..COPRT_ID) <> 0
        *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [End]
          =gfModalGen("TRM38243B00000","DIALOG",;
            lcOprt_ctg+"|"+lcOprt_ID+"|"+ALLTRIM(m.COPRT_DSC))
          lnRetVal = .F.
        ELSE
          SELECT (lc_PMPrjRl)
          gfSetOrder('PMPRJRL')
          *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [Start]
          *!*            IF lnRetVal AND SEEK(lcPrj_typ+lcPrj_ID+SUBSTR(lcStyle,1,LEN(&lc_PMPrjRl..cStyle))+lcArrCtg+lcArrOprt,lc_PMPrjRl) AND;
          *!*                        ASCAN(laTarget,&lc_PMPrjRl..CPrd_Ctg+'\'+&lc_PMPrjRl..CPrd_ID) <> 0
          IF lnRetVal AND SEEK(lcPrj_Typ+lcPrj_ID+SUBSTR(lcStyle,1,LEN(&lc_PMPrjRl..cstyle))+STR(lnLineNo,6)+;
              lcArrCtg+lcArrOprt,lc_PMPrjRl) AND;
              ASCAN(laTarget,&lc_PMPrjRl..Cprd_Ctg+'\'+&lc_PMPrjRl..Cprd_ID) <> 0
          *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [End]

            =gfModalGen("TRM38244B00000","DIALOG",;
              lcOprt_ctg+"|"+lcOprt_ID+"|"+ALLTRIM(m.COPRT_DSC))
            lnRetVal = .F.
          ENDIF
        ENDIF
      ENDIF
    ENDIF
    SELECT (lc_PMPrjRl)
    gfSetOrder(lcPrevOrd)

  ENDIF
  SET PROCEDURE TO
  RETURN lnRetVal

  *!*************************************************************
  *! Name      : LFWHENFN
  *! Developer : Mariam Mazhar
  *! Date      : 06/01/2009
  *! Purpose   : when function of option grid
  *!*************************************************************
FUNCTION LFWHENFN
  DIMENSION laPrjDesc[2],laPrjVals[2]
  STORE '' TO laPrjDesc,laPrjVals
  laPrjDesc[1] = LANG_MFPROJMON_STYLE
  laPrjVals[1] = 'S'
  laPrjDesc[2] = LANG_MFPROJMON_OTHER
  laPrjVals[2] = 'H'
  lnValCnt = 3
  IF 'MF' $ oAriaApplication.CompanyInstalledModules
    DIMENSION laPrjDesc[ALEN(laPrjDesc)+3],laPrjVals[ALEN(laPrjVals)+3]
    laPrjDesc[lnValCnt] = LANG_MFPROJMON_CTKTKT
    laPrjVals[lnValCnt] = 'C'
    lnValCnt = lnValCnt + 1
    laPrjDesc[lnValCnt] = LANG_MFPROJMON_ADORORDER
    laPrjVals[lnValCnt] = 'A'
    lnValCnt = lnValCnt + 1
    laPrjDesc[lnValCnt] = LANG_MFPROJMON_DYEORDER
    laPrjVals[lnValCnt] = 'D'
    lnValCnt = lnValCnt + 1
  ENDIF

  IF 'PO' $ oAriaApplication.CompanyInstalledModules
    DIMENSION laPrjDesc[ALEN(laPrjDesc)+3],laPrjVals[ALEN(laPrjVals)+3]
    laPrjDesc[lnValCnt] = LANG_MFPROJMON_PO
    laPrjVals[lnValCnt] = 'P'
    lnValCnt = lnValCnt + 1
    laPrjDesc[lnValCnt] = LANG_MFPROJMON_INTRPO
    laPrjVals[lnValCnt] = 'N'
    lnValCnt = lnValCnt + 1
    laPrjDesc[lnValCnt] = LANG_MFPROJMON_RETPO
    laPrjVals[lnValCnt] = 'R'
    lnValCnt = lnValCnt + 1
  ENDIF

  IF 'SO' $ oAriaApplication.CompanyInstalledModules
    DIMENSION laPrjDesc[ALEN(laPrjDesc)+2],laPrjVals[ALEN(laPrjVals)+2]
    laPrjDesc[lnValCnt] = LANG_MFPROJMON_SALORD
    laPrjVals[lnValCnt] = 'O'
    lnValCnt = lnValCnt + 1
    laPrjDesc[lnValCnt] =  LANG_MFPROJMON_TMPEDI
    laPrjVals[lnValCnt] = 'T'
    lnValCnt = lnValCnt + 1
  ENDIF


  *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [Start]
  IF 'MA' $ oAriaApplication.CompanyInstalledModules
    DIMENSION laPrjDesc[ALEN(laPrjDesc)+1],laPrjVals[ALEN(laPrjVals)+1]
    laPrjDesc[lnValCnt] = LANG_MFPROJMON_MATERIAL
    laPrjVals[lnValCnt] = 'M'
  ENDIF
  *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [End]

  lcRpPrjTyp = lcPrj_Typ
  ldRpSchDt = dSch_Date
  lcTempPrj = loogScroll.gfTempName()
  
  *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [Start]
  *CREATE CURSOR (lcTempPrj ) (KeyExp C(19))
  CREATE CURSOR (lcTempPrj ) (KeyExp C(33))
  *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [End]
  
  SELECT (lcTempPrj )
  INDEX ON KeyExp TAG (lcTempPrj)
  
  *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [Start]
  *INSERT INTO (lcTempPrj ) VALUES (PADR(lcPrj_ID,6)+'-'+SUBSTR(lcStyle,1,12))
  INSERT INTO (lcTempPrj ) VALUES (PADR(lcPrj_ID,6)+'+'+SUBSTR(lcStyle,1,19)+"+"+STR(lnLineNo,6))
  *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [End]
      
  lnPrjId= ASCAN(loogScroll.laogFxFlt,"PMPRJHD.CPRJ_ID")
  IF lnPrjId > 0
    lnPrjId = ASUBSCRIPT(loogScroll.laogFxFlt,lnPrjId,1)
    loogScroll.laogFxFlt[lnPrjId,6] = lcTempPrj
  ENDIF



  IF llEditAddScr
    llEditAdd = .T.
    lcTmpPrjRl = lcRelFile
    lcTmpPrjDt = lcDetFile
    lcPrjHder = lcHdFile
    lnFrmDtSe = lnCurDtSes
  ENDIF
  =loogScroll.RefreshScroll()
  loogScroll.SetFilterExpr()
  lcXmlStr = loogScroll.convertvariablestoxml()
  STRTOFILE(lcXmlStr,oAriaApplication.OutputHome+lcXmlFlName+'.xml')
  RETURN .F.


  *!*************************************************************
  *! Name      : lfNtfInit
  *! Developer : Mariam Mazhar
  *! Date      : 06/01/2009
  *! Purpose   : init function of Notification screen
  *!*************************************************************
FUNCTION lfNtfInit
  PARAMETERS loNtFrmSet,lcPathID, lcCatgID, lcTaskID, lcMode

  WITH loNtFrmSet
    .lcMode   = lcMode
    .lcTaskID = lcTaskID
    .lccatg   = lcCatgID
    .lcPathID = lcPathID
  ENDWITH

  WITH loNtFrmSet.AriaForm1.Ariacontainer2
    .txtEmail.INPUTMASK = REPLICATE('X',60)
    .UserKey.Keytextbox.INPUTMASK = REPLICATE('!',10)
    .txtNBC.VALUE = 0
    .txtNOSD.VALUE = 0
    .txtNOCD.VALUE = 0
    .txtNBS.VALUE = 0
  ENDWITH

  loNtFrmSet.lcScreentable = lcPMPRJNTF
  IF !loNtFrmSet.llifok
    WITH loNtFrmSet.AriaForm1.Ariacontainer1.grdUsers
      .RECORDSOURCE = lcPMPRJNTF
      .Column1.CONTROLSOURCE = 'cuser_id'
      .Column2.CONTROLSOURCE = 'nbfrstrtdy'
      .Column3.CONTROLSOURCE = 'lonstrt'
      .Column4.CONTROLSOURCE = 'nstrtdelay'
      .Column5.CONTROLSOURCE = 'lonredrct'
      .Column6.CONTROLSOURCE = 'nbfrcmpldy'
      .Column7.CONTROLSOURCE = 'loncmplt'
      .Column8.CONTROLSOURCE = 'ncmpldelay'
      loNtFrmSet.llifok = .T.
    ENDWITH
  ENDIF

  IF !EOF(lcPMPRJNTF)
    loNtFrmSet.AriaForm1.Ariacontainer1.grdUsers.REFRESH()
    loNtFrmSet.AriaForm1.Ariacontainer1.grdUsers.AFTERROWCOLCHANGE
  ELSE
    WITH loNtFrmSet.AriaForm1.Ariacontainer2
      .UserKey.ENABLED = .F.
      .txtEmail.ENABLED = .F.
      .cbnotbefst.ENABLED = .F.
      .cbnotbefcom.ENABLED = .F.
      .cbnor.ENABLED = .F.
      .cbnoc.ENABLED = .F.
      .cbnos.ENABLED = .F.
      .txtNBC.ENABLED = .F.
      .txtNBS.ENABLED = .F.
      .txtNOSD.ENABLED =.F.
      .txtNOCD.ENABLED = .F.
      .chkCmpDely.ENABLED = .F.
      .chkStrtDely.ENABLED = .F.

    ENDWITH
  ENDIF

  loNtFrmSet.AriaForm1.Ariacontainer1.grdUsers.REFRESH()
  loNtFrmSet.changemode(lcMode)

  IF EOF(lcPMPRJNTF)
    WITH loNtFrmSet.AriaForm1.Ariacontainer2
      .UserKey.ENABLED = .F.
      .txtEmail.ENABLED = .F.
      .cbnotbefst.ENABLED = .F.
      .cbnotbefcom.ENABLED = .F.
      .cbnor.ENABLED = .F.
      .cbnoc.ENABLED = .F.
      .cbnos.ENABLED = .F.
      .txtNBC.ENABLED = .F.
      .txtNBS.ENABLED = .F.
      .txtNOSD.ENABLED =.F.
      .txtNOCD.ENABLED = .F.
      .chkCmpDely.ENABLED = .F.
      .chkStrtDely.ENABLED = .F.

    ENDWITH
  ENDIF


  *!*************************************************************
  *! Name      : lfChngMode
  *! Developer : Mariam Mazhar
  *! Date      : 06/01/2009
  *! Purpose   : Change mode function of Notification screen
  *!*************************************************************
FUNCTION lfChngMode
  PARAMETERS loNtFrmSet

  *!*************************************************************
  *! Name      : lfAftrRowCol
  *! Developer : Mariam Mazhar
  *! Date      : 06/01/2009
  *! Purpose   : grid after row clo. chnaged function of Notification screen
  *!*************************************************************
FUNCTION lfAftrRowCol
  PARAMETERS loNtFrmSet
  lcScreentable = loNtFrmSet.lcScreentable
  IF !EOF()
    WITH loNtFrmSet.AriaForm1.Ariacontainer2
      .UserKey.Keytextbox.VALUE = &lcScreentable..cUser_Id
      .txtEmail.VALUE = &lcScreentable..cemail_add
      .cbnotbefst.VALUE = &lcScreentable..lbfrstrt
      .cbnotbefcom.VALUE = &lcScreentable..lbfrcmplt
      .cbnor.VALUE = &lcScreentable..lonredrct
      .cbnoc.VALUE = &lcScreentable..loncmplt
      .cbnos.VALUE = &lcScreentable..lonstrt
      .txtNBC.VALUE = &lcScreentable..nbfrcmpldy
      .txtNBS.VALUE = &lcScreentable..nbfrstrtdy
      .txtNOSD.VALUE = &lcScreentable..nstrtdelay
      .txtNOCD.VALUE = &lcScreentable..ncmpldelay
      .chkCmpDely.VALUE  = &lcScreentable..LCMPLDELAY
      .chkStrtDely.VALUE  = &lcScreentable..LSTRTDELAY

    ENDWITH

    IF &lcScreentable..lbfrstrt
      loNtFrmSet.AriaForm1.Ariacontainer2.txtNBS.ENABLED = !(loNtFrmSet.ActiveMode = 'V')
    ELSE
      loNtFrmSet.AriaForm1.Ariacontainer2.txtNBS.ENABLED = .F.
    ENDIF

    IF &lcScreentable..lbfrcmplt
      loNtFrmSet.AriaForm1.Ariacontainer2.txtNBC.ENABLED = !(loNtFrmSet.ActiveMode = 'V')
    ELSE
      loNtFrmSet.AriaForm1.Ariacontainer2.txtNBC.ENABLED = .F.
    ENDIF
    loNtFrmSet.AriaForm1.Ariacontainer3.btnRemove.ENABLED = !(loNtFrmSet.ActiveMode = 'V')

    WITH loNtFrmSet.AriaForm1.Ariacontainer2
      .UserKey.ENABLED = !(loNtFrmSet.ActiveMode = 'V')
      .txtEmail.ENABLED = !(loNtFrmSet.ActiveMode = 'V')
      .cbnotbefst.ENABLED = !(loNtFrmSet.ActiveMode = 'V')
      .cbnotbefcom.ENABLED = !(loNtFrmSet.ActiveMode = 'V')
      .cbnor.ENABLED = !(loNtFrmSet.ActiveMode = 'V')
      .cbnoc.ENABLED = !(loNtFrmSet.ActiveMode = 'V')
      .cbnos.ENABLED = !(loNtFrmSet.ActiveMode = 'V')
      .txtNBC.ENABLED = !(loNtFrmSet.ActiveMode = 'V')
      .txtNBS.ENABLED = !(loNtFrmSet.ActiveMode = 'V')
      .txtNOSD.ENABLED =!(loNtFrmSet.ActiveMode = 'V')
      .txtNOCD.ENABLED = !(loNtFrmSet.ActiveMode = 'V')
      .chkCmpDely.ENABLED = !(loNtFrmSet.ActiveMode = 'V')
      .chkStrtDely.ENABLED = !(loNtFrmSet.ActiveMode = 'V')

    ENDWITH
  ENDIF

  *!*************************************************************
  *! Name      : lfvUsrId
  *! Developer : Mariam Mazhar
  *! Date      : 06/01/2009
  *! Purpose   : validate user id function of Notification screen
  *!*************************************************************
FUNCTION lfvUsrId
  PARAMETERS loNtFrmSet
  lcPMPRJNTF = loNtFrmSet.lcScreentable
  WITH loNtFrmSet.AriaForm1.Ariacontainer2.UserKey
    lcOldVal   = .Keytextbox.OldValue
    m.cUser_Id = .Keytextbox.VALUE
    llBrowse   = .selectedfrombrowse
  ENDWITH

  IF (PADR(m.cUser_Id,10) = PADR(lcOldVal,10)) .AND. !llBrowse
    RETURN .T.
  ENDIF

  PRIVATE lnCurAlias
  lnCurAlias = SELECT(0)
  SELECT SYUUSER
  lcBrFields = [cUser_Id : H = ']+LANG_MFPROJMON_USRID +[', cUsr_Name : H = ']+LANG_MFPROJMON_USRNAME+[']
  IF (!EMPTY(m.cUser_Id) AND !gfSeek(PADR(m.cUser_Id,10),'SYUUSER')) .OR. llBrowse
    llBrowse = .F.
    DIMENSION latemp[2]
    IF ARIABROW('',LANG_MFPROJMON_USERS,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,.F.,.F.,'cUser_Id,cEmail_Add','laTemp')
      m.cUser_Id   = latemp[1]
      m.cemail_add = latemp[2]
    ELSE
      m.cUser_Id   = lcOldVal
    ENDIF
    loNtFrmSet.AriaForm1.Ariacontainer2.UserKey.Keytextbox.VALUE = m.cUser_Id
    lcOldVal = loNtFrmSet.AriaForm1.Ariacontainer2.UserKey.Keytextbox.OldValue
    IF !EMPTY(m.cUser_Id) &&AND m.cUser_ID <> lcOldVal
      lnTmpRecNo = RECNO(lcPMPRJNTF)
      *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [Start]
      *IF !SEEK(PADR(lcPrj_Typ,1)+PADR(lcPrj_ID,6)+PADR(lcStyle,12)+lcOprt_Ctg+lcoprt_id+m.cUser_ID,lcPMPRJNTF)
      IF !SEEK(PADR(lcPrj_Typ,1)+PADR(lcPrj_ID,6)+PADR(lcStyle,19)+STR(lnLineNo,6)+lcOprt_ctg+lcOprt_ID+m.cUser_Id,lcPMPRJNTF)
      *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [End]
        m.cemail_add = SYUUSER.cemail_add
      ELSE
        *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
        *WAIT WINDOW "User Exist"
        WAIT WINDOW LANG_MFPROJMOUSREXIST
        *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
        m.cUser_Id = lcOldVal
      ENDIF
      IF BETWEEN(lnTmpRecNo,1,RECCOUNT(lcPMPRJNTF))
        GO lnTmpRecNo IN (lcPMPRJNTF)
      ENDIF
    ENDIF

  ELSE
    IF !EMPTY(m.cUser_Id)
      lnTmpRecNo = RECNO(lcPMPRJNTF)
      *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [Start]
      *IF !SEEK(PADR(lcPrj_Typ,1)+PADR(lcPrj_ID,6)+PADR(lcStyle,12)+lcOprt_Ctg+lcoprt_id+m.cUser_ID,lcPMPRJNTF)
      IF !SEEK(PADR(lcPrj_Typ,1)+PADR(lcPrj_ID,6)+PADR(lcStyle,19)+STR(lnLineNo,6)+lcOprt_ctg+lcOprt_ID+m.cUser_Id,lcPMPRJNTF)
      *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [End]
        m.cemail_add = SYUUSER.cemail_add
      ELSE
        WAIT WINDOW "User Exist"
        m.cUser_Id = lcOldVal
      ENDIF
      IF BETWEEN(lnTmpRecNo,1,RECCOUNT(lcPMPRJNTF))
        GO lnTmpRecNo IN (lcPMPRJNTF)
      ENDIF
    ENDIF
  ENDIF

  SELECT (lcPMPRJNTF)
  gfREPLACe('cUser_ID   WITH m.cUser_ID,cEmail_Add WITH m.cEmail_Add')


  SELECT (lnCurAlias)
  loNtFrmSet.AriaForm1.Ariacontainer1.grdUsers.AFTERROWCOLCHANGE

  *!*************************************************************
  *! Name      : lfShow
  *! Developer : Mariam Mazhar
  *! Date      : 06/01/2009
  *! Purpose   : refresh function of Notification screen
  *!*************************************************************
FUNCTION lfShow
  PARAMETERS loNtFrmSet
  WITH loNtFrmSet.AriaForm1.Ariacontainer2
    .UserKey.ENABLED = .T.
    .txtEmail.ENABLED = .T.
    .cbnotbefst.ENABLED = .T.
    .cbnotbefcom.ENABLED = .T.
    .cbnor.ENABLED = .T.
    .cbnoc.ENABLED = .T.
    .cbnos.ENABLED = .T.
    .txtNBC.ENABLED = .T.
    .txtNBS.ENABLED = .T.
    .txtNOSD.ENABLED = .T.
    .txtNOCD.ENABLED = .T.
    .chkCmpDely.ENABLED = .T.
    .chkStrtDely.ENABLED = .T.

  ENDWITH

  WITH loNtFrmSet.AriaForm1.Ariacontainer3
    .btnRemove.ENABLED = .T.
    .btnNew.ENABLED = .T.
    .btnClose.ENABLED = .T.
  ENDWITH

  *!*************************************************************
  *! Name      : lfvEmail
  *! Developer : Mariam Mazhar
  *! Date      : 06/01/2009
  *! Purpose   : Emial validation function of Notification screen
  *!*************************************************************
FUNCTION lfvEmail
  PARAMETERS loNtFrmSet
  lcScreentable = loNtFrmSet.lcScreentable
  lcValueMail = loNtFrmSet.AriaForm1.Ariacontainer2.txtEmail.VALUE
  SELECT (lcScreentable )
  gfREPLACe("cemail_add WITH lcValueMail")
  loNtFrmSet.AriaForm1.Ariacontainer1.grdUsers.REFRESH



  *!*************************************************************
  *! Name      : lfvNtb4Strt
  *! Developer : Mariam Mazhar
  *! Date      : 06/01/2009
  *! Purpose   : notify before start validation function of Notification screen
  *!*************************************************************
FUNCTION lfvNtb4Strt
  PARAMETERS loNtFrmSet
  lcScreentable = loNtFrmSet.lcScreentable
  WITH loNtFrmSet.AriaForm1.Ariacontainer2

    SELECT(lcScreentable )
    llBfrStrt  = .cbnotbefst.VALUE
    gfREPLACe("lBfrStrt WITH llBfrStrt")


    IF .cbnotbefst.VALUE
      .txtNBS.ENABLED = .T.
      .txtNBS.VALUE = 1

      gfREPLACe("nBfrStrtDy WITH 1")
    ELSE
      .txtNBS.ENABLED = .F.
      .txtNBS.VALUE = 0
      gfREPLACe("nBfrStrtDy WITH 0")
    ENDIF
  ENDWITH

  loNtFrmSet.AriaForm1.Ariacontainer1.grdUsers.REFRESH

  *!*************************************************************
  *! Name      : lfvNtfb4StrtDays
  *! Developer : Mariam Mazhar
  *! Date      : 06/01/2009
  *! Purpose   : notify before start days validation function of Notification screen
  *!*************************************************************
FUNCTION lfvNtfb4StrtDays
  PARAMETERS loNtFrmSet
  lcScreentable = loNtFrmSet.lcScreentable
  WITH loNtFrmSet.AriaForm1
    SELECT(lcScreentable )
    lnBfrStrtDy   = .Ariacontainer2.txtNBS.VALUE
    gfREPLACe("nBfrStrtDy  WITH lnBfrStrtDy")

    .Ariacontainer1.grdUsers.REFRESH
  ENDWITH
  *!*************************************************************
  *! Name      : lfvNtfb4Comp
  *! Developer : Mariam Mazhar
  *! Date      : 06/01/2009
  *! Purpose   : notify before Complete validation function of Notification screen
  *!*************************************************************
FUNCTION lfvNtfb4Comp
  PARAMETERS loNtFrmSet
  lcScreentable = loNtFrmSet.lcScreentable
  WITH loNtFrmSet.AriaForm1.Ariacontainer2
    SELECT(lcScreentable )
    llBfrCmplt  = .cbnotbefcom.VALUE
    gfREPLACe("lBfrCmplt WITH llBfrCmplt")

    IF .cbnotbefcom.VALUE  = .F.
      .txtNBC.ENABLED = .F.
      .txtNBC.VALUE = 0
    ELSE
      .txtNBC.ENABLED = .T.
      .txtNBC.VALUE = 1
    ENDIF
    lnBfrCmplDy  = .txtNBC.VALUE
    gfREPLACe("nBfrCmplDy WITH lnBfrCmplDy")
  ENDWITH

  loNtFrmSet.AriaForm1.Ariacontainer1.grdUsers.REFRESH

  *!*************************************************************
  *! Name      : lfVNtfB4Cmpl
  *! Developer : Mariam Mazhar
  *! Date      : 06/01/2009
  *! Purpose   : notify before Complete days validation function of Notification screen
  *!*************************************************************
FUNCTION lfVNtfB4Cmpl
  PARAMETERS loNtFrmSet
  lcScreentable = loNtFrmSet.lcScreentable
  WITH loNtFrmSet.AriaForm1
    lnBfrCmplDy  = .Ariacontainer2.txtNBC.VALUE
    SELECT(lcScreentable )
    gfREPLACe("nBfrCmplDy WITH lnBfrCmplDy ")
    .Ariacontainer1.grdUsers.REFRESH
  ENDWITH
  *!*************************************************************
  *! Name      : lfvNtfStrtDl
  *! Developer : Mariam Mazhar
  *! Date      : 06/01/2009
  *! Purpose   : notify before start delay validation function of Notification screen
  *!*************************************************************
FUNCTION lfvNtfStrtDl
  PARAMETERS loNtFrmSet
  lcScreentable = loNtFrmSet.lcScreentable
  WITH loNtFrmSet.AriaForm1
    lnStrtDelay = .Ariacontainer2.txtNOSD.VALUE
    SELECT(lcScreentable )
    gfREPLACe("nStrtDelay WITH lnStrtDelay")
    .Ariacontainer1.grdUsers.REFRESH
  ENDWITH
  *!*************************************************************
  *! Name      : lfvNtfCD
  *! Developer : Mariam Mazhar
  *! Date      : 06/01/2009
  *! Purpose   : notify before Complete delay validation function of Notification screen
  *!*************************************************************
FUNCTION lfvNtfCD
  PARAMETERS loNtFrmSet
  lcScreentable = loNtFrmSet.lcScreentable
  WITH loNtFrmSet.AriaForm1
    SELECT (lcScreentable )
    lnCmplDelay  = .Ariacontainer2.txtNOCD.VALUE
    gfREPLACe("nCmplDelay WITH lnCmplDelay")
    .Ariacontainer1.grdUsers.REFRESH
  ENDWITH

  *!*************************************************************
  *! Name      : lfvNtfOnStrt
  *! Developer : Mariam Mazhar
  *! Date      : 06/01/2009
  *! Purpose   : notify on Start validation function of Notification screen
  *!*************************************************************
FUNCTION lfvNtfOnStrt
  PARAMETERS loNtFrmSet
  lcScreentable = loNtFrmSet.lcScreentable
  WITH loNtFrmSet.AriaForm1
    llOnStrt = .Ariacontainer2.cbnos.VALUE
    SELECT(lcScreentable )
    gfREPLACe("lOnStrt WITH llOnStrt")
    .Ariacontainer1.grdUsers.REFRESH
  ENDWITH
  *!*************************************************************
  *! Name      : lfvNtfOnComp
  *! Developer : Mariam Mazhar
  *! Date      : 06/01/2009
  *! Purpose   : notify on Complete validation function of Notification screen
  *!*************************************************************
FUNCTION lfvNtfOnComp
  PARAMETERS loNtFrmSet
  lcScreentable = loNtFrmSet.lcScreentable
  WITH loNtFrmSet.AriaForm1
    llOnCmplt  = .Ariacontainer2.cbnoc.VALUE
    SELECT (lcScreentable )
    gfREPLACe("lOnCmplt WITH llOnCmplt")
    .Ariacontainer1.grdUsers.REFRESH
  ENDWITH
  *!*************************************************************
  *! Name      : lfVNtfonRedir
  *! Developer : Mariam Mazhar
  *! Date      :06/01/2009
  *! Purpose   : notify on redirect validation function of Notification screen
  *!*************************************************************
FUNCTION lfVNtfonRedir
  PARAMETERS loNtFrmSet
  WITH loNtFrmSet.AriaForm1
    lcScreentable = loNtFrmSet.lcScreentable
    SELECT  (lcScreentable )
    llOnRedrct  =  .Ariacontainer2.cbnor.VALUE
    gfREPLACe("lOnRedrct WITH llOnRedrct  ")
    .Ariacontainer1.grdUsers.REFRESH
  ENDWITH
  *!*************************************************************
  *! Name      : lfvRmvUser
  *! Developer : Mariam Mazhar
  *! Date      : 06/01/2009
  *! Purpose   : Remove user validation function of Notification screen
  *!*************************************************************
FUNCTION lfvRmvUser
  PARAMETERS loNtFrmSet
  lcScreentable = loNtFrmSet.lcScreentable
  lnCurAlias = SELECT(0)
  IF gfModalGen("TRM38208B38006","DIALOG") = 1
    SELECT (lcScreentable )
    gfDELETE()
    loNtFrmSet.lfclear()
    SELECT (lcScreentable )
    LOCATE
    IF EOF()
      loNtFrmSet.AriaForm1.Ariacontainer1.grdUsers.REFRESH()
      loNtFrmSet.AriaForm1.Ariacontainer3.btnRemove.ENABLED = .F.
      WITH loNtFrmSet.AriaForm1.Ariacontainer2
        .UserKey.ENABLED = .F.
        .txtEmail.ENABLED = .F.
        .cbnotbefst.ENABLED = .F.
        .cbnotbefcom.ENABLED = .F.
        .cbnor.ENABLED = .F.
        .cbnoc.ENABLED = .F.
        .cbnos.ENABLED = .F.
        .txtNBC.ENABLED = .F.
        .txtNBS.ENABLED = .F.
        .txtNOSD.ENABLED = .F.
        .txtNOCD.ENABLED = .F.
      ENDWITH
    ENDIF
    glUpdated   = .T.
    loNtFrmSet.AriaForm1.Ariacontainer1.grdUsers.REFRESH()
  ENDIF
  SELECT (lnCurAlias)

  *!*************************************************************
  *! Name      : lfvClosNtf
  *! Developer : Mariam Mazhar
  *! Date      : 06/01/2009
  *! Purpose   : Close button function of Notification screen
  *!*************************************************************
FUNCTION lfvClosNtf
  PARAMETERS loNtFrmSet
  lcScreentable = loNtFrmSet.lcScreentable
  SELECT (lcScreentable )
  SCAN FOR EMPTY(cUser_Id)
    =gfDELETE()
  ENDSCAN

  *!*************************************************************
  *! Name      : lfvNewUsr
  *! Developer : Mariam Mazhar
  *! Date      : 06/01/2009
  *! Purpose   : New button function of Notification screen
  *!*************************************************************
FUNCTION lfvNewUsr
  PARAMETERS loNtFrmSet
  lcScreentable = loNtFrmSet.lcScreentable
  SELECT (lcScreentable)

  *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [Start]
  *IF !SEEK(PADR(lcPrj_Typ,1)+PADR(lcPrj_ID,6)+PADR(lcStyle,12)+lcOprt_Ctg+lcOprt_id+SPACE(10))
  IF !SEEK(PADR(lcPrj_Typ,1)+PADR(lcPrj_ID,6)+PADR(lcStyle,19)+STR(lnLineNo,6)+lcOprt_ctg+lcOprt_ID+SPACE(10))
  *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [End]
    gfAPPEND()
  ENDIF

  gfREPLACe("COprt_Ctg WITH lcOprt_Ctg,"+;
    "COprt_ID  WITH lcOprt_id")

  *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [Start]
  *=gfreplace("cPrj_Typ  WITH PADR(lcPrj_Typ,1),cPrj_ID   WITH PADR(lcPrj_ID,6),cStyle    WITH PADR(lcStyle,12),COprt_Ctg WITH lcOprt_Ctg,COprt_ID  WITH lcOprt_id")
  =gfREPLACe("cPrj_Typ  WITH PADR(lcPrj_Typ,1),cPrj_ID   WITH PADR(lcPrj_ID,6),;
      cStyle    WITH PADR(lcStyle,19),;
      LineNo    With lnLineNo,;
      COprt_Ctg WITH lcOprt_Ctg,COprt_ID  WITH lcOprt_id")
  *  E302650,1 MMT 12/14/2009 make project monitor work based on report templated selected [End]

  =gfAdd_Info(lcPMPRJNTF)
  gfREPLACe("")
  TABLEUPDATE(.T.)

  WITH loNtFrmSet.AriaForm1.Ariacontainer2
    .UserKey.ENABLED = .T.
    .txtEmail.ENABLED = .F.
    .cbnotbefst.ENABLED = .F.
    .cbnotbefcom.ENABLED = .F.
    .cbnor.ENABLED = .F.
    .cbnoc.ENABLED = .F.
    .cbnos.ENABLED = .F.
    .txtNBC.ENABLED = .F.
    .txtNBS.ENABLED = .F.
    .txtNOSD.ENABLED = .F.
    .txtNOCD.ENABLED = .F.
  ENDWITH
  loNtFrmSet.AriaForm1.Ariacontainer1.grdUsers.REFRESH()

  *!***************************************************************************
  *! Name      : lfvStrtDely
  *! Developer : Mariam Mazhar(MMT)
  *! Date      : 06/01/2009
  *! Purpose   : Validation function for Nofiy before Start delay Option
  *!***************************************************************************
FUNCTION lfvStrtDely
  PARAMETERS loNtFrmSet
  lcScreentable = loNtFrmSet.lcScreentable
  WITH loNtFrmSet.AriaForm1.Ariacontainer2
    SELECT(lcScreentable)
    llValueUp = .chkStrtDely.VALUE
    gfREPLACe("LSTRTDELAY WITH llValueUp")

    IF .chkStrtDely.VALUE
      .txtNOSD.ENABLED = .T.
      .txtNOSD.VALUE = 1
      gfREPLACe("nstrtdelay WITH 1")
    ELSE
      .txtNOSD.ENABLED = .F.
      .txtNOSD.VALUE = 0
      gfREPLACe("nstrtdelay WITH 0 ")
    ENDIF
  ENDWITH
  loNtFrmSet.AriaForm1.Ariacontainer1.grdUsers.REFRESH



  *!***************************************************************************
  *! Name      : lfvCompDly
  *! Developer : Mariam Mazhar(MMT)
  *! Date      : 06/01/2009
  *! Purpose   : Validation function for Nofiy before Complete delay Option
  *!***************************************************************************
FUNCTION lfvCompDly
  PARAMETERS loNtFrmSet
  lcScreentable = loNtFrmSet.lcScreentable
  SELECT(lcScreentable )

  WITH loNtFrmSet.AriaForm1.Ariacontainer2
    llValuUp = .chkCmpDely.VALUE
    gfREPLACe("LCMPLDELAY WITH llValuUp")


    IF .chkCmpDely.VALUE
      .txtNOCD.ENABLED = .T.
      .txtNOCD.VALUE = 1
      gfREPLACe("ncmpldelay WITH 1")
    ELSE
      .txtNOCD.ENABLED = .F.
      .txtNOCD.VALUE = 0
      gfREPLACe("ncmpldelay WITH 0 ")
    ENDIF
  ENDWITH
  loNtFrmSet.AriaForm1.Ariacontainer1.grdUsers.REFRESH
  *! N000632,1 MMT 06/01/2009 Modify Project Monitor Screen [End]


  *!*************************************************************
  *! Name      : lfFiltrTrrn
  *! Developer : Mariam Mazhar
  *! Date      : 06/01/2009
  *! Purpose   : Get Details of Transcation
  *!*************************************************************
FUNCTION lfFiltrTnTmp
llGetPoSum = .F.

  IF (lcRpPrjTp $ 'PADNRC')
    DO CASE
      CASE lcRpPrjTp  = 'P'
        =gfSeek('PP'+PmPrjHd.cprj_id,'POSHDR')
      CASE lcRpPrjTp  = 'A'
        =gfSeek('PA'+PmPrjHd.cprj_id,'POSHDR')
      CASE lcRpPrjTp  = 'D'
        =gfSeek('PD'+PmPrjHd.cprj_id,'POSHDR')
      CASE lcRpPrjTp  = 'N'
        =gfSeek('NN'+PmPrjHd.cprj_id,'POSHDR')
      CASE lcRpPrjTp  = 'R'
        =gfSeek('RP'+PmPrjHd.cprj_id,'POSHDR')
      CASE lcRpPrjTp  = 'C'
        =gfSeek('PU'+PmPrjHd.cprj_id,'POSHDR')
    ENDCASE

    IF (lcRpPrjTp $ 'PADNR')
      IF llVendSelected AND !SEEK(POSHDR.VENDOR,lcSelVend)
        RETURN .F.
      ENDIF
      IF lcRpPrjTp  = 'P' AND !EMPTY(lcPCmpDate)
        DO CASE
          CASE ATC("|",lcPCmpDate) = 1
            IF !(POSHDR.COMPLETE=<CTOD(SUBSTR(lcPCmpDate,2)))
              RETURN .F.
            ENDIF
          OTHERWISE
            IF !BETWEEN(POSHDR.COMPLETE,CTOD(SUBSTR(lcPCmpDate,1,10)),CTOD(SUBSTR(lcPCmpDate,12,21)))
              RETURN .F.
            ENDIF
        ENDCASE
      ENDIF
    ENDIF
    m.VENDOR =  POSHDR.VENDOR
    IF ASCAN(loFormSet.LATABLEFLD,'APVENDOR',1,0,1)> 0
      lnPOs = ASUBSCRIPT(loFormSet.LATABLEFLD,ASCAN(loFormSet.LATABLEFLD,'APVENDOR',1,0,1),1)
      lcFldScat = loFormSet.LATABLEFLD[lnPOs ,2]
      =gfSeek(POSHDR.VENDOR ,'APVENDOR')
      SELECT APVENDOR
      SCATTER FIELDS LIKE &lcFldScat. MEMO MEMVAR
      IF ASCAN(loFormSet.LAFLDMAP,'APVENDOR',1,0,2) > 0
        FOR lnE = 1 TO ALEN(loFormSet.LAFLDMAP,1)
         IF 'APVENDOR' $ loFormSet.LAFLDMAP[lnE,2]
           lcMemFld = 'm.'+loFormSet.LAFLDMAP[lnE,1]
           &lcMemFld  = EVALUATE(loFormSet.LAFLDMAP[lnE,2])
         ELSE
           LOOP 
         ENDIF   
        ENDFOR  
      ENDIF 
    ENDIF
  ENDIF
  DO CASE
    CASE lcRpPrjTp $ 'TO' &&   Sales  Order
      =gfSeek(lcRpPrjTp+PmPrjHd.cprj_id,'Ordhdr','Ordhdr')
      m.ACCOUNT = OrdHdr.ACCOUNT

      IF ASCAN(loFormSet.LATABLEFLD,'ORDHDR',1,0,1)> 0
        lnPOs = ASUBSCRIPT(loFormSet.LATABLEFLD,ASCAN(loFormSet.LATABLEFLD,'ORDHDR',1,0,1),1)
        lcFldScat = loFormSet.LATABLEFLD[lnPOs ,2]
        SELECT OrdHdr
        IF 'MUSRCOMMSO' $ lcFldScat
          SCATTER FIELDS LIKE &lcFldScat. EXCEPT MUSRCOMMSO MEMO MEMVAR
          =gfSeek('B'+ORDER,'NOTEPAD','NOTEPAD')
          m.MUSRCOMMSO = NOTEPAD.Mnotes
        ELSE
          SCATTER FIELDS LIKE &lcFldScat. MEMO MEMVAR
        ENDIF
        IF ASCAN(loFormSet.LAFLDMAP,'ORDHDR',1,0,2) > 0
          FOR lnE = 1 TO ALEN(loFormSet.LAFLDMAP,1)
           IF 'ORDHDR' $ loFormSet.LAFLDMAP[lnE,2]
             lcMemFld = 'm.'+loFormSet.LAFLDMAP[lnE,1]
             &lcMemFld  = EVALUATE(loFormSet.LAFLDMAP[lnE,2])
           ELSE
             LOOP 
           ENDIF   
          ENDFOR  
        ENDIF 
      ENDIF

      IF OrdHdr.MULTI = 'N' AND ASCAN(loFormSet.LATABLEFLD,'CUSTOMER',1,0,1)> 0
        lnPOs = ASUBSCRIPT(loFormSet.LATABLEFLD,ASCAN(loFormSet.LATABLEFLD,'CUSTOMER',1,0,1),1)
        lcFldScat = loFormSet.LATABLEFLD[lnPOs ,2]
        =gfSeek(IIF(EMPTY(OrdHdr.STORE),'M'+OrdHdr.ACCOUNT,'S'+OrdHdr.ACCOUNT+OrdHdr.STORE),'CUSTOMER')
        SELECT Customer
        SCATTER FIELDS LIKE &lcFldScat. MEMO MEMVAR
        IF ASCAN(loFormSet.LAFLDMAP,'CUSTOMER',1,0,2) > 0
          FOR lnE = 1 TO ALEN(loFormSet.LAFLDMAP,1)
            IF 'CUSTOMER' $ loFormSet.LAFLDMAP[lnE,2]
              lcMemFld = 'm.'+loFormSet.LAFLDMAP[lnE,1]
              &lcMemFld  = EVALUATE(loFormSet.LAFLDMAP[lnE,2])
            ELSE
              LOOP 
            ENDIF   
          ENDFOR  
         ENDIF         
       ENDIF
      *  B609778,1 MMT 12/25/2011 fix bug of not Displaying the Cancelled projects[Start]
*!*        IF OrdHdr.STATUS = 'X'
*!*          RETURN .F.
*!*        ENDIF
      *  B609778,1 MMT 12/25/2011 fix bug of not Displaying the Cancelled projects[END]
      IF llAccSelected AND !SEEK(OrdHdr.ACCOUNT,lcSelAcc)
        RETURN .F.
      ENDIF
      IF !EMPTY(lcCmpDate)
        DO CASE
          CASE ATC("|",lcCmpDate) = 1
            IF !(OrdHdr.COMPLETE =< CTOD(SUBSTR(lcCmpDate,2)))
              RETURN .F.
            ENDIF

          OTHERWISE
            IF !EMPTY(lcCmpDate) AND !BETWEEN(OrdHdr.COMPLETE,CTOD(SUBSTR(lcCmpDate,1,10)),CTOD(SUBSTR(lcCmpDate,12,21)))
              RETURN .F.
            ENDIF
        ENDCASE
      ENDIF

      SELECT CUTPICK
      =gfSetOrder('CUTORD')

      IF lcRpPrjTp = 'O' AND !EMPTY(loFormSet.LATABLEPROF[1,1])
        FOR lnT = 1 TO ALEN(loFormSet.LATABLEPROF,1)
          IF  loFormSet.LATABLEPROF[lnT,2] <> 'CS'
            LOOP
          ELSE
            IF gfSeek("SO"+PADR(OrdHdr.CORDTYPE+OrdHdr.ORDER,130)+loFormSet.LATABLEPROF[lnT,1],"PROFVALU")
              lcPrFFld = 'P'+loFormSet.LATABLEPROF[lnT,1]
              m.&lcPrFFld. = PROFVALU.cpro_value
            ENDIF
          ENDIF
        ENDFOR
      ENDIF

      SELECT ordline
      =gfSeek(OrdHdr.CORDTYPE+OrdHdr.ORDER)
      *B609286,1 MMT 06/07/2010 SO - project monitor screen not working for style filter [Start]
*!*        SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6)= OrdHdr.CORDTYPE+OrdHdr.ORDER  FOR ;
*!*            IIF(SUBSTR(PmPrjHd.cstyle,1,lnmajorlen) = REPLICATE('*',lnmajorlen),.T.,IIF(PmPrjHd.LINENO= 0,STYLE = ALLTRIM(PmPrjHd.cstyle),STYLE = PmPrjHd.cstyle)) AND ;
*!*            IIF(PmPrjHd.LINENO <> 0,LINENO = PmPrjHd.LINENO,.T.)
      SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6)= OrdHdr.CORDTYPE+OrdHdr.ORDER  FOR ;
          IIF(SUBSTR(PmPrjHd.cstyle,1,lnmajorlen) = REPLICATE('*',lnmajorlen),.T.,IIF(PmPrjHd.LINENO= 0,STYLE = ALLTRIM(PmPrjHd.cstyle),STYLE = PmPrjHd.cstyle)) AND ;
          IIF(PmPrjHd.LINENO <> 0,LINENO = PmPrjHd.LINENO,.T.) AND IIF(llStyleSelect ,SEEK(SUBSTR(Style,1,lnmajorlen),lcSelSty),.T.)
      *B609286,1 MMT 06/07/2010 SO - project monitor screen not working for style filter [End]
        IF ASCAN(loFormSet.LATABLEFLD,'ORDLINE',1,0,1)> 0
          lnPOs = ASUBSCRIPT(loFormSet.LATABLEFLD,ASCAN(loFormSet.LATABLEFLD,'ORDLINE',1,0,1),1)
          lcFldScat = loFormSet.LATABLEFLD[lnPOs ,2]
          SELECT ordline
          SCATTER FIELDS LIKE  &lcFldScat. MEMO MEMVAR
          IF ASCAN(loFormSet.LAFLDMAP,'ORDLINE',1,0,2) > 0
            FOR lnE = 1 TO ALEN(loFormSet.LAFLDMAP,1)
              IF 'ORDLINE' $ loFormSet.LAFLDMAP[lnE,2]
                lcMemFld = 'm.'+loFormSet.LAFLDMAP[lnE,1]
                &lcMemFld  = EVALUATE(loFormSet.LAFLDMAP[lnE,2])
              ELSE
                LOOP 
              ENDIF   
            ENDFOR  
          ENDIF         

          
        ENDIF
        m.CPOSUM = ''
        m.CLCTOTOPN = 0
        m.CLCTOTREC= 0
        m.CLCTOTINT= 0
        *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
        m.DPORCDAT = {}
        *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
        FOR lnA = 1 TO 8
          lcA =STR(lnA,1)
          m.CLCOPN&lcA. = 0
          m.CLCREC&lcA. = 0
          m.CLCINT&lcA. = 0
        ENDFOR   
                                 
        IF lcRpPrjTp = 'O' AND gfSeek('2'+PmPrjHd.cprj_id,'CUTPICK','CUTORD')
          SELECT CUTPICK
          lcCTType  = '2'
          lcSeekExp = trancd+ORDER
          SCAN REST WHILE trancd+ORDER+cordline =lcSeekExp FOR  cordline =STR(ordline.LINENO,6)
            IF ASCAN(loFormSet.LATABLEFLD,'CUTPICK',1,0,1)> 0
              lnPOs = ASUBSCRIPT(loFormSet.LATABLEFLD,ASCAN(loFormSet.LATABLEFLD,'CUTPICK',1,0,1),1)
              lcFldScat = loFormSet.LATABLEFLD[lnPOs ,2]
              SELECT CUTPICK
              SCATTER FIELDS LIKE &lcFldScat. MEMO MEMVAR
              IF ASCAN(loFormSet.LAFLDMAP,'CUTPICK',1,0,2) > 0
              FOR lnE = 1 TO ALEN(loFormSet.LAFLDMAP,1)
                IF 'CUTPICK' $ loFormSet.LAFLDMAP[lnE,2]
                  lcMemFld = 'm.'+loFormSet.LAFLDMAP[lnE,1]
                  &lcMemFld  = EVALUATE(loFormSet.LAFLDMAP[lnE,2])
                ELSE
                  LOOP 
                ENDIF   
              ENDFOR  
            ENDIF                       
            ENDIF
            =gfSeek(IIF(lcCTType  = '2','PP','PU')+CUTPICK.CTKTNO,'POSHDR')
            IF ASCAN(loFormSet.LATABLEFLD,'POSHDR',1,0,1)> 0
              lnPOs = ASUBSCRIPT(loFormSet.LATABLEFLD,ASCAN(loFormSet.LATABLEFLD,'POSHDR',1,0,1),1)
              lcFldScat = loFormSet.LATABLEFLD[lnPOs ,2]
              SELECT POSHDR
              IF 'MUSRCOMMPO' $ lcFldScat OR  'CPOSUM' $ lcFldScat
                SCATTER FIELDS LIKE &lcFldScat. EXCEPT MUSRCOMMPO,CPOSUM MEMO MEMVAR
                IF 'MUSRCOMMPO' $ lcFldScat 
                  *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
                  *=gfSeek(POSHDR.Cstytype+POSHDR.PO,'NOTEPAD','NOTEPAD')
                  =gfSeek(IIF(POSHDR.Cstytype = 'U','I',POSHDR.Cstytype)+POSHDR.PO,'NOTEPAD','NOTEPAD')                  
                  *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
                  m.MUSRCOMMPO = NOTEPAD.Mnotes
                ENDIF 
                IF 'CPOSUM' $ lcFldScat 
                  llGetPoSum = .T.
                ENDIF 
              ELSE
                SCATTER FIELDS LIKE  &lcFldScat. MEMO MEMVAR
              ENDIF
               IF ASCAN(loFormSet.LAFLDMAP,'POSHDR',1,0,2) > 0
              FOR lnE = 1 TO ALEN(loFormSet.LAFLDMAP,1)
                IF 'POSHDR' $ loFormSet.LAFLDMAP[lnE,2]
                  lcMemFld = 'm.'+loFormSet.LAFLDMAP[lnE,1]
                  &lcMemFld  = EVALUATE(loFormSet.LAFLDMAP[lnE,2])
                ELSE
                  LOOP 
                ENDIF   
              ENDFOR  
            ENDIF      
            ENDIF
            IF ASCAN(loFormSet.LATABLEFLD,'APVENDOR',1,0,1)> 0
              lnPOs = ASUBSCRIPT(loFormSet.LATABLEFLD,ASCAN(loFormSet.LATABLEFLD,'APVENDOR',1,0,1),1)
              lcFldScat = loFormSet.LATABLEFLD[lnPOs ,2]
              =gfSeek(POSHDR.VENDOR ,'APVENDOR')
              SELECT APVENDOR
              SCATTER FIELDS LIKE &lcFldScat. MEMO MEMVAR
              IF ASCAN(loFormSet.LAFLDMAP,'APVENDOR',1,0,2) > 0
              FOR lnE = 1 TO ALEN(loFormSet.LAFLDMAP,1)
                IF 'APVENDOR' $ loFormSet.LAFLDMAP[lnE,2]
                  lcMemFld = 'm.'+loFormSet.LAFLDMAP[lnE,1]
                  &lcMemFld  = EVALUATE(loFormSet.LAFLDMAP[lnE,2])
                ELSE
                  LOOP 
                ENDIF   
              ENDFOR  
            ENDIF              
            ENDIF
            IF !EMPTY(lcPCmpDate)
              DO CASE
                CASE ATC("|",lcPCmpDate) = 1
                  IF !(POSHDR.COMPLETE =< CTOD(SUBSTR(lcPCmpDate,2)))
                    LOOP
                  ENDIF
                OTHERWISE
                  IF !EMPTY(lcPCmpDate)AND !BETWEEN(POSHDR.COMPLETE,CTOD(SUBSTR(lcPCmpDate,1,10)),CTOD(SUBSTR(lcPCmpDate,12,21)))
                    LOOP
                  ENDIF
              ENDCASE
            ENDIF
            
            =gfSeek(IIF(lcCTType  = '2','PP','PU')+CUTPICK.CTKTNO+'0001' + CUTPICK.STYLE + PADR(CUTPICK.CTKTLINENO,6)+'1','POSLN','POSLN')
            IF ASCAN(loFormSet.LATABLEFLD,'POSLN',1,0,1)> 0
              lnPOs = ASUBSCRIPT(loFormSet.LATABLEFLD,ASCAN(loFormSet.LATABLEFLD,'POSLN',1,0,1),1)
              lcFldScat = loFormSet.LATABLEFLD[lnPOs ,2]
              SELECT POSLN
              *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
*!*                IF 'CLCOPN' $ lcFldScat OR 'CLCREC' $ lcFldScat OR ;
*!*                   'CLCINT' $ lcFldScat OR 'CLCTOTOPN' $ lcFldScat OR ;                
*!*                   'CLCTOTREC' $ lcFldScat OR 'CLCTOTINT' $ lcFldScat
*!*                   
*!*                  SCATTER FIELDS LIKE &lcFldScat. ;
*!*                  EXCEPT CLCOPN1,CLCOPN2,CLCOPN3,CLCOPN4,CLCOPN5,CLCOPN6,CLCOPN7,CLCOPN8,CLCTOTOPN,;
*!*                         CLCREC1,CLCREC2,CLCREC3,CLCREC4,CLCREC5,CLCREC6,CLCREC7,CLCREC8,CLCTOTREC,;
*!*                         CLCINT1,CLCINT2,CLCINT3,CLCINT4,CLCINT5,CLCINT6,CLCINT7,CLCINT8,CLCTOTINT   MEMO MEMVAR 
              IF 'CLCOPN' $ lcFldScat OR 'CLCREC' $ lcFldScat OR ;
                 'CLCINT' $ lcFldScat OR 'CLCTOTOPN' $ lcFldScat OR ;                
                 'CLCTOTREC' $ lcFldScat OR 'CLCTOTINT' $ lcFldScat OR 'DPORCDAT' $ lcFldScat 
                 
                SCATTER FIELDS LIKE &lcFldScat. ;
                EXCEPT CLCOPN1,CLCOPN2,CLCOPN3,CLCOPN4,CLCOPN5,CLCOPN6,CLCOPN7,CLCOPN8,CLCTOTOPN,;
                       CLCREC1,CLCREC2,CLCREC3,CLCREC4,CLCREC5,CLCREC6,CLCREC7,CLCREC8,CLCTOTREC,;
                       CLCINT1,CLCINT2,CLCINT3,CLCINT4,CLCINT5,CLCINT6,CLCINT7,CLCINT8,CLCTOTINT,DPORCDAT   MEMO MEMVAR 

        *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
                *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
                *IF 'CLCOPN' $ lcFldScat
                IF 'CLCOPN' $ lcFldScat OR 'CLCTOTOPN' $ lcFldScat
                *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[eND]
                  =gfSeek(IIF(lcCTType  = '2','PP','PU')+CUTPICK.CTKTNO+'0001' + CUTPICK.STYLE + PADR(CUTPICK.CTKTLINENO,6),'POSLN','POSLN')
                  SELECT POSLN
                  SCAN REST WHILE CBUSDOCU+CSTYTYPE+PO+CINVTYPE+STYLE+STR(LINENO,6)+TRANCD = ;
                      IIF(lcCTType  = '2','PP','PU')+CUTPICK.CTKTNO+'0001' + CUTPICK.STYLE + PADR(CUTPICK.CTKTLINENO,6) FOR trancd <> '3'
                    FOR lnA = 1 TO 8
                      lcA =STR(lnA,1)
                      m.CLCOPN&lcA. = m.CLCOPN&lcA. + IIF(TRANCD$'245',-1*POSLN.Qty&lcA.,POSLN.Qty&lcA.)
                      m.CLCTOTOPN = m.CLCTOTOPN +  IIF(TRANCD$'245',-1*POSLN.Qty&lcA.,POSLN.Qty&lcA.)
                    ENDFOR 
                  ENDSCAN 
                ENDIF 
                *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
                *IF 'CLCINT' $ lcFldScat
                IF 'CLCINT' $ lcFldScat OR 'CLCTOTINT' $ lcFldScat
                *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[eND]
                  =gfSeek(IIF(lcCTType  = '2','PP','PU')+CUTPICK.CTKTNO+'0001' + CUTPICK.STYLE + PADR(CUTPICK.CTKTLINENO,6)+'3','POSLN','POSLN')                    
                  SELECT POSLN
                  SCAN REST WHILE CBUSDOCU+CSTYTYPE+PO+CINVTYPE+STYLE+STR(LINENO,6)+TRANCD = IIF(lcCTType  = '2','PP','PU')+CUTPICK.CTKTNO+'0001' + CUTPICK.STYLE + PADR(CUTPICK.CTKTLINENO,6)+'3'
                    FOR lnA = 1 TO 8
                      lcA =STR(lnA,1)
                      m.CLCINT&lcA. = m.CLCINT&lcA. +  POSLN.Qty&lcA.
                      m.CLCTOTINT= m.CLCTOTINT+ POSLN.Qty&lcA.
                    ENDFOR 
                  ENDSCAN                                                                
                ENDIF   
                *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
                *IF 'CLCREC' $ lcFldScat
                IF 'CLCREC' $ lcFldScat OR 'CLCTOTREC' $ lcFldScat OR 'DPORCDAT' $ lcFldScat
                *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
                  =gfSeek(IIF(lcCTType  = '2','PP','PU')+CUTPICK.CTKTNO+'0001' + CUTPICK.STYLE + PADR(CUTPICK.CTKTLINENO,6)+'2','POSLN','POSLN')                    
                  SELECT POSLN
                  SCAN REST WHILE CBUSDOCU+CSTYTYPE+PO+CINVTYPE+STYLE+STR(LINENO,6)+TRANCD = IIF(lcCTType  = '2','PP','PU')+CUTPICK.CTKTNO+'0001' + CUTPICK.STYLE + PADR(CUTPICK.CTKTLINENO,6)+'2'
                    FOR lnA = 1 TO 8
                      lcA =STR(lnA,1)
                      m.CLCREC&lcA. = m.CLCREC&lcA. +  POSLN.Qty&lcA.
                      m.CLCTOTREC= m.CLCTOTREC+ POSLN.Qty&lcA.
                    ENDFOR 
                    *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
                    IF EMPTY(m.DPORCDAT) OR POSLN.Date > m.DPORCDAT
                      m.DPORCDAT = POSLN.Date 
                    ENDIF
                    *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
                  ENDSCAN                                                                
                ENDIF   
                
              ELSE
                SCATTER FIELDS LIKE &lcFldScat. MEMO MEMVAR
              ENDIF 
              IF ASCAN(loFormSet.LAFLDMAP,'POSLN',1,0,2) > 0
              FOR lnE = 1 TO ALEN(loFormSet.LAFLDMAP,1)
                IF 'POSLN' $ loFormSet.LAFLDMAP[lnE,2]
                  lcMemFld = 'm.'+loFormSet.LAFLDMAP[lnE,1]
                  &lcMemFld  = EVALUATE(loFormSet.LAFLDMAP[lnE,2])
                ELSE
                  LOOP 
                ENDIF   
              ENDFOR  
            ENDIF       
            ENDIF
            =gfSeek(IIF(lcCTType  = '2','PP','PU')+CUTPICK.CTKTNO+'0001' + CUTPICK.STYLE + PADR(CUTPICK.CTKTLINENO,6)+'1','POSLN','POSLN')
            IF llGetPoSum 
              m.CPOSUM = m.CPOSUM + + IIF(EMPTY(m.CPOSUM),'',CHR(13)+CHR(10))+IIF(lcCTType= '2','PO:','CT:')+POSLN.PO+' Complete:'+DTOC(POSLN.COMPLETE)+' TOTQTY:'+ALLTRIM(STR(CUTPICK.TOTQTY,10))
            ENDIF 


            IF !EMPTY(loFormSet.LATABLEPROF[1,1])
              FOR lnT = 1 TO ALEN(loFormSet.LATABLEPROF,1)
                DO CASE
                  CASE  loFormSet.LATABLEPROF[lnT,2] = 'ST' && GET PROFILE VALUE FOR SALES Order Header
                  *  E302650,3 MMT 12/29/2009 make project monitor work based on report templated selected [Start]
                  *  IF gfSeek(IIF(lcCTType  = '2',"POPP",'CTPU')+PADR(CUTPICK.CTKTNO+CUTPICK.CTKTLINENO,130)+loFormSet.LATABLEPROF[lnT,1],"PROFVALU")
                    IF gfSeek(IIF(lcCTType  = '2',"POPP",'CTPU')+PADR(CUTPICK.CTKTNO+CUTPICK.CTKTLINENO,128)+loFormSet.LATABLEPROF[lnT,1],"PROFVALU")
                  *  E302650,3 MMT 12/29/2009 make project monitor work based on report templated selected [End]
                      lcPrFFld = 'P'+loFormSet.LATABLEPROF[lnT,1]
                      m.&lcPrFFld. = PROFVALU.cpro_value
                    ENDIF
                  CASE  loFormSet.LATABLEPROF[lnT,2] = 'VN' AND lcCTType  = '2'&& GET PROFILE VALUE FOR PO Order Header
                  *  E302650,3 MMT 12/29/2009 make project monitor work based on report templated selected [Start]
                  *  IF gfSeek("POPP"+PADR(CUTPICK.CTKTNO,130)+loFormSet.LATABLEPROF[lnT,1],"PROFVALU")
                      IF gfSeek("POPP"+PADR(CUTPICK.CTKTNO,128)+loFormSet.LATABLEPROF[lnT,1],"PROFVALU")
                  *  E302650,3 MMT 12/29/2009 make project monitor work based on report templated selected [End]
                      lcPrFFld = 'P'+loFormSet.LATABLEPROF[lnT,1]
                      m.&lcPrFFld. = PROFVALU.cpro_value
                    ENDIF
                ENDCASE
              ENDFOR
            ENDIF
          ENDSCAN
        ENDIF

        IF lcRpPrjTp = 'O' AND gfSeek('1'+PmPrjHd.cprj_id,'CUTPICK','CUTORD')
          SELECT CUTPICK
          lcCTType  = '1'
          lcSeekExp = trancd+ORDER
          SCAN REST WHILE trancd+ORDER+cordline =lcSeekExp FOR  cordline =STR(ordline.LINENO,6)
            IF ASCAN(loFormSet.LATABLEFLD,'CUTPICK',1,0,1)> 0
              lnPOs = ASUBSCRIPT(loFormSet.LATABLEFLD,ASCAN(loFormSet.LATABLEFLD,'CUTPICK',1,0,1),1)
              lcFldScat = loFormSet.LATABLEFLD[lnPOs ,2]
              SELECT CUTPICK
              SCATTER FIELDS LIKE &lcFldScat. MEMO MEMVAR
              IF ASCAN(loFormSet.LAFLDMAP,'CUTPICK',1,0,2) > 0
              FOR lnE = 1 TO ALEN(loFormSet.LAFLDMAP,1)
                IF 'CUTPICK' $ loFormSet.LAFLDMAP[lnE,2]
                  lcMemFld = 'm.'+loFormSet.LAFLDMAP[lnE,1]
                  &lcMemFld  = EVALUATE(loFormSet.LAFLDMAP[lnE,2])
                ELSE
                  LOOP 
                ENDIF   
              ENDFOR  
            ENDIF                     
            ENDIF
            =gfSeek(IIF(lcCTType  = '2','PP','PU')+CUTPICK.CTKTNO,'POSHDR')
            IF ASCAN(loFormSet.LATABLEFLD,'POSHDR',1,0,1)> 0
              lnPOs = ASUBSCRIPT(loFormSet.LATABLEFLD,ASCAN(loFormSet.LATABLEFLD,'POSHDR',1,0,1),1)
              lcFldScat = loFormSet.LATABLEFLD[lnPOs ,2]
              SELECT POSHDR
              IF 'MUSRCOMMPO' $ lcFldScat OR 'CPOSUM' $ lcFldScat
                SCATTER FIELDS LIKE &lcFldScat. EXCEPT MUSRCOMMPO,CPOSUM MEMO MEMVAR
                IF 'MUSRCOMMPO' $ lcFldScat 
                  *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
                  *=gfSeek(POSHDR.Cstytype+POSHDR.PO,'NOTEPAD','NOTEPAD')
                  =gfSeek(IIF(POSHDR.Cstytype = 'U','I',POSHDR.Cstytype)+POSHDR.PO,'NOTEPAD','NOTEPAD')                  
                  *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
                  m.MUSRCOMMPO = NOTEPAD.Mnotes
                ENDIF 
                IF 'CPOSUM' $ lcFldScat 
                  llGetPoSum = .T.
                ENDIF   
              ELSE
                SCATTER FIELDS LIKE &lcFldScat. MEMO MEMVAR  
              ENDIF
              IF ASCAN(loFormSet.LAFLDMAP,'POSHDR',1,0,2) > 0
              FOR lnE = 1 TO ALEN(loFormSet.LAFLDMAP,1)
                IF 'POSHDR' $ loFormSet.LAFLDMAP[lnE,2]
                  lcMemFld = 'm.'+loFormSet.LAFLDMAP[lnE,1]
                  &lcMemFld  = EVALUATE(loFormSet.LAFLDMAP[lnE,2])
                ELSE
                  LOOP 
                ENDIF   
              ENDFOR  
            ENDIF       
            ENDIF
            IF ASCAN(loFormSet.LATABLEFLD,'APVENDOR')> 0
              lnPOs = ASUBSCRIPT(loFormSet.LATABLEFLD,ASCAN(loFormSet.LATABLEFLD,'APVENDOR',1,0,1),1)
              lcFldScat = loFormSet.LATABLEFLD[lnPOs ,2]
              =gfSeek(POSHDR.VENDOR ,'APVENDOR')
              SELECT APVENDOR
              SCATTER FIELDS LIKE &lcFldScat. MEMO MEMVAR
               IF ASCAN(loFormSet.LAFLDMAP,'APVENDOR',1,0,2) > 0
              FOR lnE = 1 TO ALEN(loFormSet.LAFLDMAP,1)
                IF 'APVENDOR' $ loFormSet.LAFLDMAP[lnE,2]
                  lcMemFld = 'm.'+loFormSet.LAFLDMAP[lnE,1]
                  &lcMemFld  = EVALUATE(loFormSet.LAFLDMAP[lnE,2])
                ELSE
                  LOOP 
                ENDIF   
              ENDFOR  
            ENDIF       
            ENDIF
            IF !EMPTY(lcPCmpDate)
              DO CASE
                CASE ATC("|",lcPCmpDate) = 1
                  IF !(POSHDR.COMPLETE =< CTOD(SUBSTR(lcPCmpDate,2)))
                    LOOP
                  ENDIF

                OTHERWISE
                  IF !EMPTY(lcPCmpDate)AND !BETWEEN(POSHDR.COMPLETE,CTOD(SUBSTR(lcPCmpDate,1,10)),CTOD(SUBSTR(lcPCmpDate,12,21)))
                    LOOP
                  ENDIF
              ENDCASE
            ENDIF
            =gfSeek(IIF(lcCTType  = '2','PP','PU')+CUTPICK.CTKTNO+'0001' + CUTPICK.STYLE + PADR(CUTPICK.CTKTLINENO,6)+'1','POSLN','POSLN')
            IF ASCAN(loFormSet.LATABLEFLD,'POSLN',1,0,1)> 0
              lnPOs = ASUBSCRIPT(loFormSet.LATABLEFLD,ASCAN(loFormSet.LATABLEFLD,'POSLN',1,0,1),1)
              lcFldScat = loFormSet.LATABLEFLD[lnPOs ,2]
              SELECT POSLN
              *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
*!*                IF 'CLCOPN' $ lcFldScat OR 'CLCREC' $ lcFldScat OR ;
*!*                   'CLCINT' $ lcFldScat OR 'CLCTOTOPN' $ lcFldScat OR ;                
*!*                   'CLCTOTREC' $ lcFldScat OR 'CLCTOTINT' $ lcFldScat
*!*                   
*!*                  SCATTER FIELDS LIKE &lcFldScat. ;
*!*                  EXCEPT CLCOPN1,CLCOPN2,CLCOPN3,CLCOPN4,CLCOPN5,CLCOPN6,CLCOPN7,CLCOPN8,CLCTOTOPN,;
*!*                         CLCREC1,CLCREC2,CLCREC3,CLCREC4,CLCREC5,CLCREC6,CLCREC7,CLCREC8,CLCTOTREC,;
*!*                         CLCINT1,CLCINT2,CLCINT3,CLCINT4,CLCINT5,CLCINT6,CLCINT7,CLCINT8,CLCTOTINT   MEMO MEMVAR 
              IF 'CLCOPN' $ lcFldScat OR 'CLCREC' $ lcFldScat OR ;
                 'CLCINT' $ lcFldScat OR 'CLCTOTOPN' $ lcFldScat OR ;                
                 'CLCTOTREC' $ lcFldScat OR 'CLCTOTINT' $ lcFldScat OR 'DPORCDAT' $ lcFldScat
                 
                SCATTER FIELDS LIKE &lcFldScat. ;
                EXCEPT CLCOPN1,CLCOPN2,CLCOPN3,CLCOPN4,CLCOPN5,CLCOPN6,CLCOPN7,CLCOPN8,CLCTOTOPN,;
                       CLCREC1,CLCREC2,CLCREC3,CLCREC4,CLCREC5,CLCREC6,CLCREC7,CLCREC8,CLCTOTREC,;
                       CLCINT1,CLCINT2,CLCINT3,CLCINT4,CLCINT5,CLCINT6,CLCINT7,CLCINT8,CLCTOTINT,DPORCDAT   MEMO MEMVAR 

          *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
                *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
                *IF 'CLCOPN' $ lcFldScat
                IF 'CLCOPN' $ lcFldScat OR 'CLCTOTOPN' $ lcFldScat 
                *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
                 =gfSeek(IIF(lcCTType  = '2','PP','PU')+CUTPICK.CTKTNO+'0001' + CUTPICK.STYLE + PADR(CUTPICK.CTKTLINENO,6),'POSLN','POSLN')
                  SELECT POSLN
                  SCAN REST WHILE CBUSDOCU+CSTYTYPE+PO+CINVTYPE+STYLE+STR(LINENO,6)+TRANCD = ;
                      IIF(lcCTType  = '2','PP','PU')+CUTPICK.CTKTNO+'0001' + CUTPICK.STYLE + PADR(CUTPICK.CTKTLINENO,6) FOR trancd <> '3'
                    FOR lnA = 1 TO 8
                      lcA =STR(lnA,1)
                      m.CLCOPN&lcA. = m.CLCOPN&lcA. +  IIF(TRANCD$'245',-1*POSLN.Qty&lcA.,POSLN.Qty&lcA.)
                      m.CLCTOTOPN = m.CLCTOTOPN +IIF(TRANCD$'245',-1*POSLN.Qty&lcA.,POSLN.Qty&lcA.)
                    ENDFOR 
                  ENDSCAN  
                ENDIF 
                *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
                *IF 'CLCINT' $ lcFldScat                
                IF 'CLCINT' $ lcFldScat OR 'CLCTOTINT' $ lcFldScat
                *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
                  =gfSeek(IIF(lcCTType  = '2','PP','PU')+CUTPICK.CTKTNO+'0001' + CUTPICK.STYLE + PADR(CUTPICK.CTKTLINENO,6)+'3','POSLN','POSLN')                    
                  SELECT POSLN
                  SCAN REST WHILE CBUSDOCU+CSTYTYPE+PO+CINVTYPE+STYLE+STR(LINENO,6)+TRANCD = IIF(lcCTType  = '2','PP','PU')+CUTPICK.CTKTNO+'0001' + CUTPICK.STYLE + PADR(CUTPICK.CTKTLINENO,6)+'3'
                    FOR lnA = 1 TO 8
                      lcA =STR(lnA,1)
                      m.CLCINT&lcA. = m.CLCINT&lcA. +  POSLN.Qty&lcA.
                      m.CLCTOTINT= m.CLCTOTINT+ POSLN.Qty&lcA.
                    ENDFOR 
                  ENDSCAN                                                                
                ENDIF   
                *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
                *IF 'CLCREC' $ lcFldScat
                IF 'CLCREC' $ lcFldScat OR 'CLCTOTREC' $ lcFldScat OR 'DPORCDAT' $ lcFldScat
                *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
                  =gfSeek(IIF(lcCTType  = '2','PP','PU')+CUTPICK.CTKTNO+'0001' + CUTPICK.STYLE + PADR(CUTPICK.CTKTLINENO,6)+'2','POSLN','POSLN')                    
                  SELECT POSLN
                  SCAN REST WHILE CBUSDOCU+CSTYTYPE+PO+CINVTYPE+STYLE+STR(LINENO,6)+TRANCD = IIF(lcCTType  = '2','PP','PU')+CUTPICK.CTKTNO+'0001' + CUTPICK.STYLE + PADR(CUTPICK.CTKTLINENO,6)+'2'
                    FOR lnA = 1 TO 8
                      lcA =STR(lnA,1)
                      m.CLCREC&lcA. = m.CLCREC&lcA. +  POSLN.Qty&lcA.
                      m.CLCTOTREC= m.CLCTOTREC + POSLN.Qty&lcA.
                    ENDFOR
                    *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
                    IF EMPTY(m.DPORCDAT) OR POSLN.Date > m.DPORCDAT
                      m.DPORCDAT = POSLN.Date 
                    ENDIF
                    *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
                     
                  ENDSCAN                                                                
                ENDIF   
              ELSE
                SCATTER FIELDS LIKE  &lcFldScat. MEMO MEMVAR
              ENDIF
               IF ASCAN(loFormSet.LAFLDMAP,'POSLN',1,0,2) > 0
              FOR lnE = 1 TO ALEN(loFormSet.LAFLDMAP,1)
                IF 'POSLN' $ loFormSet.LAFLDMAP[lnE,2]
                  lcMemFld = 'm.'+loFormSet.LAFLDMAP[lnE,1]
                  &lcMemFld  = EVALUATE(loFormSet.LAFLDMAP[lnE,2])
                ELSE
                  LOOP 
                ENDIF   
              ENDFOR  
            ENDIF         
            ENDIF
            =gfSeek(IIF(lcCTType  = '2','PP','PU')+CUTPICK.CTKTNO+'0001' + CUTPICK.STYLE + PADR(CUTPICK.CTKTLINENO,6)+'1','POSLN','POSLN')
            IF llGetPoSum 
              m.CPOSUM = m.CPOSUM + + IIF(EMPTY(m.CPOSUM),'',CHR(13)+CHR(10))+IIF(lcCTType= '2','PO:','CT:')+POSLN.PO+' Complete:'+DTOC(POSLN.COMPLETE)+' TOTQTY:'+ALLTRIM(STR(CUTPICK.TOTQTY,10))
            ENDIF 
            IF !EMPTY(loFormSet.LATABLEPROF[1,1])
              FOR lnT = 1 TO ALEN(loFormSet.LATABLEPROF,1)
                DO CASE
                  CASE  loFormSet.LATABLEPROF[lnT,2] = 'ST' && GET PROFILE VALUE FOR SALES Order Header
                   *  E302650,3 MMT 12/29/2009 make project monitor work based on report templated selected [Start]
                   * IF gfSeek(IIF(lcCTType  = '2',"POPP",'CTPU')+PADR(CUTPICK.CTKTNO+CUTPICK.CTKTLINENO,130)+loFormSet.LATABLEPROF[lnT,1],"PROFVALU")
                      IF gfSeek(IIF(lcCTType  = '2',"POPP",'CTPU')+PADR(CUTPICK.CTKTNO+CUTPICK.CTKTLINENO,128)+loFormSet.LATABLEPROF[lnT,1],"PROFVALU")
                  *  E302650,3 MMT 12/29/2009 make project monitor work based on report templated selected [End]
                      lcPrFFld = 'P'+loFormSet.LATABLEPROF[lnT,1]
                      m.&lcPrFFld. = PROFVALU.cpro_value
                    ENDIF
                  CASE  loFormSet.LATABLEPROF[lnT,2] = 'VN' AND lcCTType  = '2'&& GET PROFILE VALUE FOR PO Order Header
                    *  E302650,3 MMT 12/29/2009 make project monitor work based on report templated selected [Start]
                   * IF gfSeek("POPP"+PADR(CUTPICK.CTKTNO,130)+loFormSet.LATABLEPROF[lnT,1],"PROFVALU")
                    IF gfSeek("POPP"+PADR(CUTPICK.CTKTNO,128)+loFormSet.LATABLEPROF[lnT,1],"PROFVALU")
                    *  E302650,3 MMT 12/29/2009 make project monitor work based on report templated selected [End]
                      lcPrFFld = 'P'+loFormSet.LATABLEPROF[lnT,1]
                      m.&lcPrFFld. = PROFVALU.cpro_value
                    ENDIF
                ENDCASE
              ENDFOR
            ENDIF

          ENDSCAN
        ENDIF


        IF lcRpPrjTp = 'O' AND !EMPTY(loFormSet.LATABLEPROF[1,1])
          FOR lnT = 1 TO ALEN(loFormSet.LATABLEPROF,1)
            IF loFormSet.LATABLEPROF[lnT,2] <> 'ST' && GET PROFILE VALUE FOR SALES Order Header
              LOOP
            ELSE
              IF gfSeek("SO"+OrdHdr.CORDTYPE+OrdHdr.ORDER,"PROFVALU")
                SELECT PROFVALU
                LOCATE REST WHILE cpro_type+ckey+cpro_code ="SO"+OrdHdr.CORDTYPE+OrdHdr.ORDER FOR cpro_code  = loFormSet.LATABLEPROF[lnT,1] AND ;
                  SUBSTR(ckey,14,6) = STR(ordline.LINENO,6)
                IF FOUND()
                  lcPrFFld = 'P'+loFormSet.LATABLEPROF[lnT,1]
                  m.&lcPrFFld. = PROFVALU.cpro_value
                ENDIF
              ENDIF
            ENDIF
          ENDFOR
        ENDIF

        =gfSeek(ordline.STYLE,'STYLE')
        =gfSeek('S'+STYLE.SCALE,'SCALE')
        IF ASCAN(loFormSet.LATABLEFLD,'STYLE',1,0,1)> 0
          lnPOs = ASUBSCRIPT(loFormSet.LATABLEFLD,ASCAN(loFormSet.LATABLEFLD,'STYLE',1,0,1),1)
          lcFldScat = loFormSet.LATABLEFLD[lnPOs ,2]
          SELECT STYLE
          SCATTER FIELDS LIKE &lcFldScat. MEMO MEMVAR
           IF ASCAN(loFormSet.LAFLDMAP,'STYLE',1,0,2) > 0
              FOR lnE = 1 TO ALEN(loFormSet.LAFLDMAP,1)
                IF 'STYLE' $ loFormSet.LAFLDMAP[lnE,2]
                  lcMemFld = 'm.'+loFormSet.LAFLDMAP[lnE,1]
                  &lcMemFld  = EVALUATE(loFormSet.LAFLDMAP[lnE,2])
                ELSE
                  LOOP 
                ENDIF   
              ENDFOR  
            ENDIF    
        ENDIF



        IF ASCAN(loFormSet.LATABLEFLD,'ORDLINE',1,0,1)> 0
          lnPOs = ASUBSCRIPT(loFormSet.LATABLEFLD,ASCAN(loFormSet.LATABLEFLD,'ORDLINE',1,0,1),1)
          lcFldScat = loFormSet.LATABLEFLD[lnPOs ,2]
          SELECT ordline
          SCATTER FIELDS LIKE &lcFldScat. MEMO MEMVAR
          
           IF ASCAN(loFormSet.LAFLDMAP,'ORDLINE',1,0,2) > 0
              FOR lnE = 1 TO ALEN(loFormSet.LAFLDMAP,1)
                IF 'ORDLINE' $ loFormSet.LAFLDMAP[lnE,2]
                  lcMemFld = 'm.'+loFormSet.LAFLDMAP[lnE,1]
                  &lcMemFld  = EVALUATE(loFormSet.LAFLDMAP[lnE,2])
                ELSE
                  LOOP 
                ENDIF   
              ENDFOR  
            ENDIF    
          
        ENDIF



        IF lcRpPrjTp = 'O' AND !EMPTY(loFormSet.LATABLEPROF[1,1])
          FOR lnT = 1 TO ALEN(loFormSet.LATABLEPROF,1)
            DO CASE
              CASE  loFormSet.LATABLEPROF[lnT,2] = 'CS' && GET PROFILE VALUE FOR SALES Order Header
                IF gfSeek("SO"+PADR(OrdHdr.CORDTYPE+OrdHdr.ORDER,130)+loFormSet.LATABLEPROF[lnT,1],"PROFVALU")
                  lcPrFFld = 'P'+loFormSet.LATABLEPROF[lnT,1]
                  m.&lcPrFFld. = PROFVALU.cpro_value
                ENDIF
              CASE  loFormSet.LATABLEPROF[lnT,2] = 'ST' && GET PROFILE VALUE FOR SALES Order Header
                IF gfSeek("SO"+OrdHdr.CORDTYPE+OrdHdr.ORDER,"PROFVALU")
                  SELECT PROFVALU
                  LOCATE REST WHILE cpro_type+ckey+cpro_code ="SO"+OrdHdr.CORDTYPE+OrdHdr.ORDER FOR cpro_code  = loFormSet.LATABLEPROF[lnT,1] AND ;
                    SUBSTR(ckey,14,6) = STR(ordline.LINENO,6)
                  IF FOUND()
                    lcPrFFld = 'P'+loFormSet.LATABLEPROF[lnT,1]
                    m.&lcPrFFld. = PROFVALU.cpro_value
                  ENDIF
                ENDIF

*!*                  IF gfSeek(IIF(lcCTType  = '2',"POPP",'CTPU')+PADR(CUTPICK.CTKTNO+CUTPICK.CTKTLINENO,130)+loFormSet.LATABLEPROF[lnT,1],"PROFVALU")
*!*                    lcPrFFld = 'P'+loFormSet.LATABLEPROF[lnT,1]
*!*                    m.&lcPrFFld. = PROFVALU.cpro_value
*!*                  ENDIF
*  E302650,3 MMT 12/29/2009 make project monitor work based on report templated selected [Start]
*!*                CASE  loFormSet.LATABLEPROF[lnT,2] = 'VN' AND lcCTType  = '2'&& GET PROFILE VALUE FOR PO Order Header
*!*                  IF gfSeek("POPP"+PADR(CUTPICK.CTKTNO,130)+loFormSet.LATABLEPROF[lnT,1],"PROFVALU")
*!*                    lcPrFFld = 'P'+loFormSet.LATABLEPROF[lnT,1]
*!*                    m.&lcPrFFld. = PROFVALU.cpro_value
*!*                  ENDIF
**  E302650,3 MMT 12/29/2009 make project monitor work based on report templated selected [End]
            ENDCASE
          ENDFOR
        ENDIF
        m.CLCTOTOPN = 0
        FOR lnA = 1 TO 8
          lcA =STR(lnA,1)
          m.CLCOPN&lcA. = MAX(m.CLCOPN&lcA.,0)
          m.CLCTOTOPN = m.CLCTOTOPN + m.CLCOPN&lcA.
        ENDFOR      

        llTaskFnd = .F.
        FOR lnI =1 TO ALEN(loFormSet.LATEMPACT,1)
          lcI = ALLTRIM(STR(lnI))
          m.cOprID&lcI.= ''
          m.cOpDsc&lcI.= ''
          m.COPCTG&lcI.= ''
          m.cTskSt&lcI.= ''
          m.dAcFnh&lcI.= {}
          m.DClFnh&lcI.= {}
          m.mOPCOM&lcI. = ""
        ENDFOR
        lfTaskList()
        *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
        IF !EMPTY(loFormSet.LATEMPACT[1])
          llAllEmpty = .T.
          FOR lnD =1 TO ALEN(loFormSet.LATEMPACT,1)
            lcD = ALLTRIM(STR(lnD))
            IF !EMPTY(m.cTskSt&lcD.)
              llAllEmpty = .F.
              EXIT 
            ENDIF
          ENDFOR 
          IF llAllEmpty
            LOOP 
          ENDIF
        ENDIF  
        *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
        
        FOR lnR = 1 TO ALEN(loFormSet.LATEMPFL,1)
          IF !EMPTY(loFormSet.LATEMPFL[lnR,1]) AND USED(loFormSet.LATEMPFL[lnR,1])
            INSERT INTO (loFormSet.LATEMPFL[lnR,1]) FROM MEMVAR
          ENDIF
        ENDFOR
      ENDSCAN

      *!*      ELSE
      *!*        IF lcRpPrjTp = 'O' AND !EMPTY(lcPCmpDate)
      *!*          RETURN .F.
      *!*        ENDIF
      *!*        =gfSeek(lcRpPrjTp +PmPrjHd.cprj_id,'ORDLINE','ORDLINE')
      *!*        SELECT ordline
      *!*        SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = lcRpPrjTp +OrdHdr.ORDER FOR ;
      *!*            IIF(SUBSTR(PmPrjHd.cstyle,1,lnmajorlen) = REPLICATE('*',lnmajorlen),.T.,IIF(PmPrjHd.LINENO= 0,STYLE = ALLTRIM(PmPrjHd.cstyle),STYLE = PmPrjHd.cstyle)) AND ;
      *!*            IIF(PmPrjHd.LINENO <> 0,ordline.LINENO = PmPrjHd.LINENO,.T.)
      *!*          IF gfSeek("SO"+lcRpPrjTp+ordline.ORDER+STR(ordline.LINENO,6),"PROFVALU")
      *!*            lnPrfCnt = 1
      *!*            SELECT PROFVALU
      *!*            SCAN REST WHILE cpro_type+ckey+cpro_code = "SO"+lcRpPrjTp +ordline.ORDER+STR(ordline.LINENO,6)
      *!*              lcPrfCnt = STR(lnPrfCnt,1)
      *!*            ENDSCAN
      *!*          ENDIF

      *!*          IF ASCAN(loFormSet.LATABLEFLD,'ORDLINE')> 0
      *!*            lnPOs = ASUBSCRIPT(loFormSet.LATABLEFLD,ASCAN(loFormSet.LATABLEFLD,'ORDLINE'),1)
      *!*            lcFldScat = loFormSet.LATABLEFLD[lnPOs ,2]
      *!*            SELECT ordline
      *!*            SCATTER FIELDS &lcFldScat. MEMO MEMVAR
      *!*          ENDIF
      *!*          IF !EMPTY(loFormSet.LATABLEPROF[1,1])
      *!*            FOR lnT = 1 TO ALEN(loFormSet.LATABLEPROF,1)
      *!*              DO CASE
      *!*                CASE  loFormSet.LATABLEPROF[lnT,2] = 'CS' && GET PROFILE VALUE FOR SALES Order Header
      *!*                  IF gfSeek("SO"+PADR(OrdHdr.CORDTYPE+OrdHdr.ORDER,130)+loFormSet.LATABLEPROF[lnT,1],"PROFVALU")
      *!*                    lcPrFFld = 'P'+loFormSet.LATABLEPROF[lnT,1]
      *!*                    m.&lcPrFFld. = PROFVALU.cpro_value
      *!*                  ENDIF
      *!*                CASE  loFormSet.LATABLEPROF[lnT,2] = 'ST' && GET PROFILE VALUE FOR SALES Order Header
      *!*                  IF gfSeek("SO"+OrdHdr.CORDTYPE+OrdHdr.ORDER,"PROFVALU")
      *!*                    SELECT PROFVALU
      *!*                    LOCATE REST WHILE cpro_type+ckey+cpro_code ="SO"+OrdHdr.CORDTYPE+OrdHdr.ORDER FOR cpro_code  = loFormSet.LATABLEPROF[lnT,1] AND ;
      *!*                      SUBSTR(ckey,14,6) = STR(ordline.LINENO,6)
      *!*                    IF FOUND()
      *!*                      lcPrFFld = 'P'+loFormSet.LATABLEPROF[lnT,1]
      *!*                      m.&lcPrFFld. = PROFVALU.cpro_value
      *!*                    ENDIF
      *!*                  ENDIF
      *!*              ENDCASE
      *!*            ENDFOR
      *!*          ENDIF
      *!*          =gfSeek(ordline.STYLE,'STYLE')
      *!*          IF ASCAN(loFormSet.LATABLEFLD,'STYLE')> 0
      *!*            lnPOs = ASUBSCRIPT(loFormSet.LATABLEFLD,ASCAN(loFormSet.LATABLEFLD,'STYLE'),1)
      *!*            lcFldScat = loFormSet.LATABLEFLD[lnPOs ,2]
      *!*            SELECT STYLE
      *!*            SCATTER FIELDS &lcFldScat. MEMO MEMVAR
      *!*          ENDIF
      *!*          =gfSeek('S'+STYLE.SCALE,'SCALE')

      *!*          IF ASCAN(loFormSet.LATABLEFLD,'ORDHDR')> 0
      *!*            lnPOs = ASUBSCRIPT(loFormSet.LATABLEFLD,ASCAN(loFormSet.LATABLEFLD,'ORDHDR'),1)
      *!*            lcFldScat = loFormSet.LATABLEFLD[lnPOs ,2]
      *!*            SELECT OrdHdr
      *!*            IF 'MUSRCOMMSO' $ lcFldScat
      *!*              SCATTER FIELDS LIKE &lcFldScat. EXCEPT MUSRCOMMSO MEMO MEMVAR
      *!*              =gfSeek('B'+ORDER,'NOTEPAD','NOTEPAD')
      *!*              m.MUSRCOMMSO = NOTEPAD.Mnotes
      *!*            ELSE
      *!*              IF 'CSOSUM' $ lcFldScat
      *!*                SCATTER FIELDS LIKE &lcFldScat. EXCEPT CSOSUM MEMO MEMVAR
      *!*              ELSE
      *!*                SCATTER FIELDS &lcFldScat. MEMO MEMVAR
      *!*              ENDIF
      *!*            ENDIF
      *!*          ENDIF
      *!*          llTaskFnd = .F.
      *!*          FOR lnI =1 TO ALEN(loFormSet.LATEMPACT,1)
      *!*            lcI = ALLTRIM(STR(lnI))
      *!*            m.cOprID&lcI.= ''
      *!*            m.cOpDsc&lcI.= ''
      *!*            m.COPCTG&lcI.= ''
      *!*            m.cTskSt&lcI.= ''
      *!*            m.dAcFnh&lcI.= {}
      *!*            m.DClFnh&lcI.= {}
      *!*            m.mOPCOM&lcI. = ""
      *!*          ENDFOR

      *!*          lfTaskList()
      *!*          FOR lnR = 1 TO ALEN(loFormSet.LATEMPFL,1)
      *!*            IF !EMPTY(loFormSet.LATEMPFL[lnR,1]) AND USED(loFormSet.LATEMPFL[lnR,1])
      *!*              INSERT INTO (loFormSet.LATEMPFL[lnR,1]) FROM MEMVAR
      *!*            ENDIF
      *!*          ENDFOR
      *!*        ENDSCAN
      *!*      ENDIF

    CASE lcRpPrjTp $ 'NADRPC' &&   PO or Cut Tkt
      *  B609778,1 MMT 12/25/2011 fix bug of not Displaying the Cancelled projects[Start]     
*!*        IF POSHDR.STATUS = 'X'
*!*          RETURN .F.
*!*        ENDIF
      *  B609778,1 MMT 12/25/2011 fix bug of not Displaying the Cancelled projects[END]
      llGetOrdSum = .F.
      IF ASCAN(loFormSet.LATABLEFLD,'POSHDR',1,0,1)> 0
        lnPOs = ASUBSCRIPT(loFormSet.LATABLEFLD,ASCAN(loFormSet.LATABLEFLD,'POSHDR',1,0,1),1)
        lcFldScat = loFormSet.LATABLEFLD[lnPOs ,2]
        SELECT POSHDR
        IF 'MUSRCOMMPO' $ lcFldScat 
          SCATTER FIELDS LIKE &lcFldScat. EXCEPT MUSRCOMMPO MEMO MEMVAR
          IF 'MUSRCOMMPO' $ lcFldScat 
            *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
            *=gfSeek(POSHDR.Cstytype+POSHDR.PO,'NOTEPAD','NOTEPAD')
            =gfSeek(IIF(POSHDR.Cstytype = 'U','I',POSHDR.Cstytype)+POSHDR.PO,'NOTEPAD','NOTEPAD')            
            *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
            m.MUSRCOMMPO = NOTEPAD.Mnotes
          ENDIF
        ELSE
          SCATTER FIELDS LIKE  &lcFldScat. MEMO MEMVAR
        ENDIF
        IF ASCAN(loFormSet.LAFLDMAP,'POSHDR',1,0,2) > 0
              FOR lnE = 1 TO ALEN(loFormSet.LAFLDMAP,1)
                IF 'POSHDR' $ loFormSet.LAFLDMAP[lnE,2]
                  lcMemFld = 'm.'+loFormSet.LAFLDMAP[lnE,1]
                  &lcMemFld  = EVALUATE(loFormSet.LAFLDMAP[lnE,2])
                ELSE
                  LOOP 
                ENDIF   
              ENDFOR  
            ENDIF    
      ENDIF
      *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
      IF ASCAN(loFormSet.LATABLEFLD,'APVENDOR',1,0,1)> 0
        lnPOs = ASUBSCRIPT(loFormSet.LATABLEFLD,ASCAN(loFormSet.LATABLEFLD,'APVENDOR',1,0,1),1)
        lcFldScat = loFormSet.LATABLEFLD[lnPOs ,2]
        =gfSeek(POSHDR.VENDOR ,'APVENDOR')
        SELECT APVENDOR
        SCATTER FIELDS LIKE &lcFldScat. MEMO MEMVAR
        IF ASCAN(loFormSet.LAFLDMAP,'APVENDOR',1,0,2) > 0
          FOR lnE = 1 TO ALEN(loFormSet.LAFLDMAP,1)
           IF 'APVENDOR' $ loFormSet.LAFLDMAP[lnE,2]
             lcMemFld = 'm.'+loFormSet.LAFLDMAP[lnE,1]
             &lcMemFld  = EVALUATE(loFormSet.LAFLDMAP[lnE,2])
           ELSE
             LOOP 
           ENDIF   
          ENDFOR  
        ENDIF 
      ENDIF
      *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
      
      SELECT CUTPICK
      =gfSetOrder('CUTPKORD')

      SELECT POSLN
      =gfSeek(POSHDR.CBUSDOCU+POSHDR.Cstytype+POSHDR.PO)
      *B609286,1 MMT 06/07/2010 SO - project monitor screen not working for style filter [Start]      
*!*        SCAN REST WHILE CBUSDOCU+Cstytype+PO+CINVTYPE+STYLE+STR(LINENO,6)+trancd = POSHDR.CBUSDOCU+POSHDR.Cstytype+POSHDR.PO FOR ;
*!*            IIF(SUBSTR(PmPrjHd.cstyle,1,lnmajorlen) = REPLICATE('*',lnmajorlen),.T.,IIF(PmPrjHd.LINENO= 0,STYLE = ALLTRIM(PmPrjHd.cstyle),STYLE = PmPrjHd.cstyle)) AND ;
*!*            IIF(PmPrjHd.LINENO <> 0,LINENO = PmPrjHd.LINENO,.T.) AND TRANCD = '1'
      SCAN REST WHILE CBUSDOCU+Cstytype+PO+CINVTYPE+STYLE+STR(LINENO,6)+trancd = POSHDR.CBUSDOCU+POSHDR.Cstytype+POSHDR.PO FOR ;
          IIF(SUBSTR(PmPrjHd.cstyle,1,lnmajorlen) = REPLICATE('*',lnmajorlen),.T.,IIF(PmPrjHd.LINENO= 0,STYLE = ALLTRIM(PmPrjHd.cstyle),STYLE = PmPrjHd.cstyle)) AND ;
          IIF(PmPrjHd.LINENO <> 0,LINENO = PmPrjHd.LINENO,.T.) AND TRANCD = '1' AND IIF(llStyleSelect ,SEEK(SUBSTR(Style,1,lnmajorlen),lcSelSty),.T.)
      *B609286,1 MMT 06/07/2010 SO - project monitor screen not working for style filter [End]          
        m.CLCTOTOPN = 0
        m.CLCTOTREC= 0
        m.CLCTOTINT= 0
        m.CSOSUM =  ''
        *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
        m.DPORCDAT ={}
        *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
        FOR lnA = 1 TO 8
          lcA =STR(lnA,1)
          m.CLCOPN&lcA. = 0
          m.CLCREC&lcA. = 0
          m.CLCINT&lcA. = 0
        ENDFOR   
        lcSeekExp = IIF(lcRpPrjTp  = 'P','2','1')+PmPrjHd.cprj_id+STR(POSLN.LINENO,6)
        IF gfSeek(lcSeekExp ,'CUTPICK') AND !(lcRpPrjTp $ 'NADR')
          SELECT CUTPICK
          lcCTType  = trancd
          SCAN REST WHILE trancd+CTKTNO+CTKTLINENO+ORDER+STYLE+cordline = lcSeekExp
            IF ASCAN(loFormSet.LATABLEFLD,'CUTPICK',1,0,1)> 0
              lnPOs = ASUBSCRIPT(loFormSet.LATABLEFLD,ASCAN(loFormSet.LATABLEFLD,'CUTPICK',1,0,1),1)
              lcFldScat = loFormSet.LATABLEFLD[lnPOs ,2]
              SELECT CUTPICK
              SCATTER FIELDS LIKE &lcFldScat. MEMO MEMVAR
              IF ASCAN(loFormSet.LAFLDMAP,'CUTPICK',1,0,2) > 0
              FOR lnE = 1 TO ALEN(loFormSet.LAFLDMAP,1)
                IF 'CUTPICK' $ loFormSet.LAFLDMAP[lnE,2]
                  lcMemFld = 'm.'+loFormSet.LAFLDMAP[lnE,1]
                  &lcMemFld  = EVALUATE(loFormSet.LAFLDMAP[lnE,2])
                ELSE
                  LOOP 
                ENDIF   
              ENDFOR  
            ENDIF    
              
            ENDIF
            =gfSeek('O'+CUTPICK.ORDER,'ORDHDR')
            m.ACCOUNT = OrdHdr.ACCOUNT
            IF ASCAN(loFormSet.LATABLEFLD,'ORDHDR',1,0,1)> 0
              lnPOs = ASUBSCRIPT(loFormSet.LATABLEFLD,ASCAN(loFormSet.LATABLEFLD,'ORDHDR',1,0,1),1)
              lcFldScat = loFormSet.LATABLEFLD[lnPOs ,2]
              SELECT OrdHdr
              IF 'MUSRCOMMSO' $ lcFldScat OR 'CSOSUM' $ lcFldScat
                SCATTER FIELDS LIKE &lcFldScat. EXCEPT MUSRCOMMSO,CSOSUM MEMO MEMVAR
                IF 'MUSRCOMMSO' $ lcFldScat 
                  =gfSeek('B'+ORDER,'NOTEPAD','NOTEPAD')
                  m.MUSRCOMMSO = NOTEPAD.Mnotes
                ENDIF   
                IF  'CSOSUM' $ lcFldScat
                  llGetOrdSum = .T.
                ENDIF
              ELSE
                SCATTER FIELDS LIKE &lcFldScat. MEMO MEMVAR
              ENDIF
               IF ASCAN(loFormSet.LAFLDMAP,'ORDHDR',1,0,2) > 0
              FOR lnE = 1 TO ALEN(loFormSet.LAFLDMAP,1)
                IF 'ORDHDR' $ loFormSet.LAFLDMAP[lnE,2]
                  lcMemFld = 'm.'+loFormSet.LAFLDMAP[lnE,1]
                  &lcMemFld  = EVALUATE(loFormSet.LAFLDMAP[lnE,2])
                ELSE
                  LOOP 
                ENDIF   
              ENDFOR  
            ENDIF    
          ENDIF
            
          *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
          IF OrdHdr.MULTI = 'N' AND ASCAN(loFormSet.LATABLEFLD,'CUSTOMER',1,0,1)> 0
            lnPOs = ASUBSCRIPT(loFormSet.LATABLEFLD,ASCAN(loFormSet.LATABLEFLD,'CUSTOMER',1,0,1),1)
            lcFldScat = loFormSet.LATABLEFLD[lnPOs ,2]
            =gfSeek(IIF(EMPTY(OrdHdr.STORE),'M'+OrdHdr.ACCOUNT,'S'+OrdHdr.ACCOUNT+OrdHdr.STORE),'CUSTOMER')
            SELECT Customer
            SCATTER FIELDS LIKE &lcFldScat. MEMO MEMVAR
            IF ASCAN(loFormSet.LAFLDMAP,'CUSTOMER',1,0,2) > 0
              FOR lnE = 1 TO ALEN(loFormSet.LAFLDMAP,1)
                IF 'CUSTOMER' $ loFormSet.LAFLDMAP[lnE,2]
                  lcMemFld = 'm.'+loFormSet.LAFLDMAP[lnE,1]
                  &lcMemFld  = EVALUATE(loFormSet.LAFLDMAP[lnE,2])
                ELSE
                  LOOP 
                ENDIF   
              ENDFOR  
             ENDIF         
           ENDIF
           *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
           
            =gfSeek('O'+OrdHdr.ORDER+CUTPICK.cordline,'ORDLINE','ORDLINE')
            IF llGetOrdSum
              m.CSOSUM = m.CSOSUM + IIF(EMPTY(m.CSOSUM),'',CHR(13)+CHR(10))+'SO:'+OrdHdr.ORDER+;
              ' Complete:'+DTOC(OrdHdr.COMPLETE)+' TOTQTY:'+ALLTRIM(STR(CUTPICK.TOTQTY,10))
              *  E302650,3 MMT 12/29/2009 make project monitor work based on report templated selected [Start]
              m.CSOSUM = m.CSOSUM + " Selling Price:"+STR(ORDLINE.price,13,3) 
              **  E302650,3 MMT 12/29/2009 make project monitor work based on report templated selected [End]
            ENDIF
            IF ASCAN(loFormSet.LATABLEFLD,'ORDLINE',1,0,1)> 0
              lnPOs = ASUBSCRIPT(loFormSet.LATABLEFLD,ASCAN(loFormSet.LATABLEFLD,'ORDLINE',1,0,1),1)
              lcFldScat = loFormSet.LATABLEFLD[lnPOs ,2]
              SELECT ordline
              SCATTER FIELDS LIKE &lcFldScat. MEMO MEMVAR
              IF ASCAN(loFormSet.LAFLDMAP,'ORDLINE',1,0,2) > 0
                FOR lnE = 1 TO ALEN(loFormSet.LAFLDMAP,1)
                  IF 'ORDLINE' $ loFormSet.LAFLDMAP[lnE,2]
                    lcMemFld = 'm.'+loFormSet.LAFLDMAP[lnE,1]
                    &lcMemFld  = EVALUATE(loFormSet.LAFLDMAP[lnE,2])
                  ELSE
                    LOOP 
                  ENDIF   
                ENDFOR  
              ENDIF                  
            ENDIF
            *=gfSeek(IIF(lcRpPrjTp  = 'P','PP','PU')+CUTPICK.CTKTNO+'0001' + CUTPICK.STYLE + PADR(CUTPICK.CTKTLINENO,6)+'1','POSLN','POSLN')

            IF !EMPTY(loFormSet.LATABLEPROF[1,1])
              FOR lnT = 1 TO ALEN(loFormSet.LATABLEPROF,1)
                DO CASE
                  CASE  loFormSet.LATABLEPROF[lnT,2] = 'CS' && GET PROFILE VALUE FOR SALES Order Header
                    IF gfSeek("SO"+PADR(OrdHdr.CORDTYPE+OrdHdr.ORDER,130)+loFormSet.LATABLEPROF[lnT,1],"PROFVALU")
                      lcPrFFld = 'P'+loFormSet.LATABLEPROF[lnT,1]
                      m.&lcPrFFld. = PROFVALU.cpro_value
                    ENDIF
                  CASE  loFormSet.LATABLEPROF[lnT,2] = 'ST' && GET PROFILE VALUE FOR SALES Order Header
                    IF gfSeek("SO"+OrdHdr.CORDTYPE+OrdHdr.ORDER,"PROFVALU")
                      SELECT PROFVALU
                      LOCATE REST WHILE cpro_type+ckey+cpro_code ="SO"+OrdHdr.CORDTYPE+OrdHdr.ORDER FOR cpro_code  = loFormSet.LATABLEPROF[lnT,1] AND ;
                        SUBSTR(ckey,14,6) = STR(ordline.LINENO,6)
                      IF FOUND()
                        lcPrFFld = 'P'+loFormSet.LATABLEPROF[lnT,1]
                        m.&lcPrFFld. = PROFVALU.cpro_value
                      ENDIF
                    ENDIF
*  E302650,3 MMT 12/29/2009 make project monitor work based on report templated selected [Start]
  *                  IF gfSeek(IIF(lcRpPrjTp  = 'P',"POPP",'CTPU')+PADR(CUTPICK.CTKTNO+CUTPICK.CTKTLINENO,130)+loFormSet.LATABLEPROF[lnT,1],"PROFVALU")
                    IF gfSeek(IIF(lcRpPrjTp  = 'P',"POPP",'CTPU')+PADR(CUTPICK.CTKTNO+CUTPICK.CTKTLINENO,128)+loFormSet.LATABLEPROF[lnT,1],"PROFVALU")
*  E302650,3 MMT 12/29/2009 make project monitor work based on report templated selected [End]
                      lcPrFFld = 'P'+loFormSet.LATABLEPROF[lnT,1]
                      m.&lcPrFFld. = PROFVALU.cpro_value
                    ENDIF

                  CASE  loFormSet.LATABLEPROF[lnT,2] = 'VN' AND lcRpPrjTp  = 'P'&& GET PROFILE VALUE FOR PO Order Header
                    *  E302650,3 MMT 12/29/2009 make project monitor work based on report templated selected [Start]
                   * IF gfSeek("POPP"+PADR(CUTPICK.CTKTNO,130)+loFormSet.LATABLEPROF[lnT,1],"PROFVALU")
                    IF gfSeek("POPP"+PADR(CUTPICK.CTKTNO,128)+loFormSet.LATABLEPROF[lnT,1],"PROFVALU")
                    **  E302650,3 MMT 12/29/2009 make project monitor work based on report templated selected [End]
                      lcPrFFld = 'P'+loFormSet.LATABLEPROF[lnT,1]
                      m.&lcPrFFld. = PROFVALU.cpro_value
                    ENDIF
                ENDCASE
              ENDFOR
            ENDIF
          ENDSCAN
        ENDIF
        llTaskFnd = .F.
        =gfSeek(POSLN.STYLE,'STYLE')
        IF !EMPTY(loFormSet.LATABLEPROF[1,1])
          FOR lnT = 1 TO ALEN(loFormSet.LATABLEPROF,1)
            DO CASE
              CASE  loFormSet.LATABLEPROF[lnT,2] = 'ST' && GET PROFILE VALUE FOR SALES Order Header
              *  E302650,3 MMT 12/29/2009 make project monitor work based on report templated selected [Start]
               * IF gfSeek(IIF(lcRpPrjTp  = 'P',"POPP",'CTPU')+PADR(POSLN.PO+STR(POSLN.LINENO,6),130)+loFormSet.LATABLEPROF[lnT,1],"PROFVALU")
                 IF gfSeek(IIF(lcRpPrjTp  = 'P',"POPP",'CTPU')+PADR(POSLN.PO+STR(POSLN.LINENO,6),128)+loFormSet.LATABLEPROF[lnT,1],"PROFVALU")
                *  E302650,3 MMT 12/29/2009 make project monitor work based on report templated selected [End]
                  lcPrFFld = 'P'+loFormSet.LATABLEPROF[lnT,1]
                  m.&lcPrFFld. = PROFVALU.cpro_value
                ENDIF

              CASE  loFormSet.LATABLEPROF[lnT,2] = 'VN' AND lcRpPrjTp  = 'P'&& GET PROFILE VALUE FOR PO Order Header
                *  E302650,3 MMT 12/29/2009 make project monitor work based on report templated selected [Start]
                *IF gfSeek("POPP"+PADR(POSHDR.PO,130)+loFormSet.LATABLEPROF[lnT,1],"PROFVALU")
                  IF gfSeek("POPP"+PADR(POSHDR.PO,128)+loFormSet.LATABLEPROF[lnT,1],"PROFVALU")
                *  E302650,3 MMT 12/29/2009 make project monitor work based on report templated selected [End]
                  lcPrFFld = 'P'+loFormSet.LATABLEPROF[lnT,1]
                  m.&lcPrFFld. = PROFVALU.cpro_value
                ENDIF
            ENDCASE
          ENDFOR
        ENDIF


        IF ASCAN(loFormSet.LATABLEFLD,'STYLE',1,0,1)> 0
          lnPOs = ASUBSCRIPT(loFormSet.LATABLEFLD,ASCAN(loFormSet.LATABLEFLD,'STYLE',1,0,1),1)
          lcFldScat = loFormSet.LATABLEFLD[lnPOs ,2]
          SELECT STYLE
          SCATTER FIELDS LIKE &lcFldScat. MEMO MEMVAR
          
           IF ASCAN(loFormSet.LAFLDMAP,'STYLE',1,0,2) > 0
              FOR lnE = 1 TO ALEN(loFormSet.LAFLDMAP,1)
                IF 'STYLE' $ loFormSet.LAFLDMAP[lnE,2]
                  lcMemFld = 'm.'+loFormSet.LAFLDMAP[lnE,1]
                  &lcMemFld  = EVALUATE(loFormSet.LAFLDMAP[lnE,2])
                ELSE
                  LOOP 
                ENDIF   
              ENDFOR  
            ENDIF     
        ENDIF
        =gfSeek('S'+STYLE.SCALE,'SCALE')
        IF ASCAN(loFormSet.LATABLEFLD,'POSLN',1,0,1)> 0
          lnPOs = ASUBSCRIPT(loFormSet.LATABLEFLD,ASCAN(loFormSet.LATABLEFLD,'POSLN',1,0,1),1)
          lcFldScat = loFormSet.LATABLEFLD[lnPOs ,2]
          *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
          SELECT POSLN
          *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
           IF ASCAN(loFormSet.LAFLDMAP,'POSLN',1,0,2) > 0
              FOR lnE = 1 TO ALEN(loFormSet.LAFLDMAP,1)
                IF 'POSLN' $ loFormSet.LAFLDMAP[lnE,2]
                  lcMemFld = 'm.'+loFormSet.LAFLDMAP[lnE,1]
                  &lcMemFld  = EVALUATE(loFormSet.LAFLDMAP[lnE,2])
                ELSE
                  LOOP 
                ENDIF   
              ENDFOR  
            ENDIF     
            *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
*!*            IF 'CLCOPN' $ lcFldScat OR 'CLCREC' $ lcFldScat OR ;
*!*               'CLCINT' $ lcFldScat OR 'CLCTOTOPN' $ lcFldScat OR ;                
*!*               'CLCTOTREC' $ lcFldScat OR 'CLCTOTINT' $ lcFldScat
*!*               
*!*              SCATTER FIELDS LIKE &lcFldScat. ;
*!*              EXCEPT CLCOPN1,CLCOPN2,CLCOPN3,CLCOPN4,CLCOPN5,CLCOPN6,CLCOPN7,CLCOPN8,CLCTOTOPN,;
*!*                     CLCREC1,CLCREC2,CLCREC3,CLCREC4,CLCREC5,CLCREC6,CLCREC7,CLCREC8,CLCTOTREC,;
*!*                     CLCINT1,CLCINT2,CLCINT3,CLCINT4,CLCINT5,CLCINT6,CLCINT7,CLCINT8,CLCTOTINT   MEMO MEMVAR 
          IF 'CLCOPN' $ lcFldScat OR 'CLCREC' $ lcFldScat OR ;
             'CLCINT' $ lcFldScat OR 'CLCTOTOPN' $ lcFldScat OR ;                
             'CLCTOTREC' $ lcFldScat OR 'CLCTOTINT' $ lcFldScat OR 'DPORCDAT' $ lcFldScat 
             
            SCATTER FIELDS LIKE &lcFldScat. ;
            EXCEPT CLCOPN1,CLCOPN2,CLCOPN3,CLCOPN4,CLCOPN5,CLCOPN6,CLCOPN7,CLCOPN8,CLCTOTOPN,;
                   CLCREC1,CLCREC2,CLCREC3,CLCREC4,CLCREC5,CLCREC6,CLCREC7,CLCREC8,CLCTOTREC,;
                   CLCINT1,CLCINT2,CLCINT3,CLCINT4,CLCINT5,CLCINT6,CLCINT7,CLCINT8,CLCTOTINT,DPORCDAT   MEMO MEMVAR 
        *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]                   
            SELECT POSLN_PO
            =gfSeek(POSLN.CBUSDOCU+POSLN.Cstytype+POSLN.PO+POSLN.CINVTYPE+ POSLN.STYLE+STR(POSLN.LINENO,6))
            *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
            *IF 'CLCOPN' $ lcFldScat
            IF 'CLCOPN' $ lcFldScat OR 'CLCTOTOPN' $ lcFldScat            
            *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
              SCAN REST WHILE CBUSDOCU+CSTYTYPE+PO+CINVTYPE+STYLE+STR(LINENO,6)+TRANCD =;
                               POSLN.CBUSDOCU+POSLN.Cstytype+POSLN.PO+POSLN.CINVTYPE+ POSLN.STYLE+STR(POSLN.LINENO,6) FOR ;
                               TRANCD <> '3'                                                               
                FOR lnA = 1 TO 8
                  lcA =STR(lnA,1)
                  m.CLCOPN&lcA. = m.CLCOPN&lcA. + IIF(TRANCD $'245',-1*POSLN_PO.Qty&lcA.,POSLN_PO.Qty&lcA.)
                  m.CLCTOTOPN = m.CLCTOTOPN + IIF(TRANCD $'245',-1*POSLN_PO.Qty&lcA.,POSLN_PO.Qty&lcA.)
                ENDFOR 
              ENDSCAN 
            ENDIF 
            SELECT POSLN_PO
            =Seek(POSLN.CBUSDOCU+POSLN.Cstytype+POSLN.PO+POSLN.CINVTYPE+ POSLN.STYLE+STR(POSLN.LINENO,6))
            *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
            *IF 'CLCINT' $ lcFldScat
            IF 'CLCINT' $ lcFldScat OR  'CLCTOTINT' $ lcFldScat            
            *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
              SCAN REST WHILE CBUSDOCU+CSTYTYPE+PO+CINVTYPE+STYLE+STR(LINENO,6)+TRANCD =;
                               POSLN.CBUSDOCU+POSLN.Cstytype+POSLN.PO+POSLN.CINVTYPE+ POSLN.STYLE+STR(POSLN.LINENO,6) FOR ;
                               TRANCD = '3'                                                               
                FOR lnA = 1 TO 8
                  lcA =STR(lnA,1)
                  m.CLCINT&lcA. = m.CLCINT&lcA. +  POSLN_PO.Qty&lcA.
                  m.CLCTOTINT= m.CLCTOTINT+ POSLN_PO.Qty&lcA.
                ENDFOR 
              ENDSCAN                                                                
            ENDIF   
            SELECT POSLN_PO
            =Seek(POSLN.CBUSDOCU+POSLN.Cstytype+POSLN.PO+POSLN.CINVTYPE+ POSLN.STYLE+STR(POSLN.LINENO,6))
            *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
            *IF 'CLCREC' $ lcFldScat
            IF 'CLCREC' $ lcFldScat OR 'CLCTOTREC' $ lcFldScat OR 'DPORCDAT' $ lcFldScat 
            *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
              SCAN REST WHILE CBUSDOCU+CSTYTYPE+PO+CINVTYPE+STYLE+STR(LINENO,6)+TRANCD =;
                               POSLN.CBUSDOCU+POSLN.Cstytype+POSLN.PO+POSLN.CINVTYPE+ POSLN.STYLE+STR(POSLN.LINENO,6) FOR ;
                               TRANCD = '2'                                                               

                FOR lnA = 1 TO 8
                  lcA =STR(lnA,1)
                  m.CLCREC&lcA. = m.CLCREC&lcA. +  POSLN_PO.Qty&lcA.
                  m.CLCTOTREC= m.CLCTOTREC+ POSLN_PO.Qty&lcA.
                ENDFOR 
                *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
                IF EMPTY(m.DPORCDAT) OR POSLN_PO.Date > m.DPORCDAT
                  m.DPORCDAT = POSLN_PO.Date 
                ENDIF
                *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]

             ENDSCAN                                                                
            ENDIF 
            m.CLCTOTOPN = 0
            FOR lnA = 1 TO 8
              lcA =STR(lnA,1)
              m.CLCOPN&lcA. = MAX(m.CLCOPN&lcA.,0)
              m.CLCTOTOPN = m.CLCTOTOPN + m.CLCOPN&lcA.
            ENDFOR      
          ELSE
            SCATTER FIELDS LIKE &lcFldScat. MEMO MEMVAR
          ENDIF 
        ENDIF

        FOR lnI =1 TO ALEN(loFormSet.LATEMPACT,1)
          lcI = ALLTRIM(STR(lnI))
          m.cOprID&lcI.= ''
          m.cOpDsc&lcI.= ''
          m.COPCTG&lcI.= ''
          m.cTskSt&lcI.= ''
          m.dAcFnh&lcI.= {}
          m.DClFnh&lcI.= {}
          m.mOPCOM&lcI. = ""
        ENDFOR

        lfTaskList()
        
        *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
        IF !EMPTY(loFormSet.LATEMPACT[1])        
          llAllEmpty = .T.
          FOR lnD =1 TO ALEN(loFormSet.LATEMPACT,1)
            lcD = ALLTRIM(STR(lnD))
            IF !EMPTY(m.cTskSt&lcD.)
              llAllEmpty = .F.
              EXIT 
            ENDIF
          ENDFOR 
          IF llAllEmpty
            LOOP 
          ENDIF
        ENDIF  
        *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
        
        FOR lnR = 1 TO ALEN(loFormSet.LATEMPFL,1)
          IF !EMPTY(loFormSet.LATEMPFL[lnR,1]) AND USED(loFormSet.LATEMPFL[lnR,1])
            INSERT INTO (loFormSet.LATEMPFL[lnR,1]) FROM MEMVAR
          ENDIF
        ENDFOR

      ENDSCAN

      *!*        IF gfSeek(lcSeekExp,'CUTPICK')
      *!*          SELECT CUTPICK
      *!*          lcCTType  = trancd
      *!*          SCAN REST WHILE trancd+CTKTNO+STYLE  = lcSeekExp FOR ;
      *!*              IIF(SUBSTR(PmPrjHd.cstyle,1,lnmajorlen) = REPLICATE('*',lnmajorlen),.T.,IIF(PmPrjHd.LINENO= 0,STYLE = ALLTRIM(PmPrjHd.cstyle),STYLE = PmPrjHd.cstyle)) AND ;
      *!*              IIF(PmPrjHd.LINENO <> 0,CTKTLINENO = STR(PmPrjHd.LINENO,6),.T.)
      *!*
      *!*          ENDSCAN
      *!*        ELSE && Work From POSLN Direct
      *!*          =gfSeek(POSHDR.CBUSDOCU+POSHDR.Cstytype+POSHDR.PO,'POSLN')

      *!*          SELECT POSLN
      *!*          SCAN REST WHILE CBUSDOCU+Cstytype+PO+CINVTYPE+STYLE+STR(LINENO,6)+trancd = POSHDR.CBUSDOCU+POSHDR.Cstytype+POSHDR.PO ;
      *!*              FOR trancd = '1' AND IIF(SUBSTR(PmPrjHd.cstyle,1,lnmajorlen) = REPLICATE('*',lnmajorlen),.T.,IIF(PmPrjHd.LINENO= 0,STYLE = ALLTRIM(PmPrjHd.cstyle),STYLE = PmPrjHd.cstyle)) AND ;
      *!*              IIF(PmPrjHd.LINENO <> 0,LINENO = PmPrjHd.LINENO,.T.)

      *!*            IF ASCAN(loFormSet.LATABLEFLD,'POSLN')> 0
      *!*              lnPOs = ASUBSCRIPT(loFormSet.LATABLEFLD,ASCAN(loFormSet.LATABLEFLD,'POSLN'),1)
      *!*              lcFldScat = loFormSet.LATABLEFLD[lnPOs ,2]
      *!*              SELECT POSLN
      *!*              SCATTER FIELDS &lcFldScat. MEMO MEMVAR
      *!*            ENDIF

      *!*            =gfSeek(POSLN.STYLE,'Style','Style')
      *!*            IF ASCAN(loFormSet.LATABLEFLD,'STYLE')> 0
      *!*              lnPOs = ASUBSCRIPT(loFormSet.LATABLEFLD,ASCAN(loFormSet.LATABLEFLD,'STYLE'),1)
      *!*              lcFldScat = loFormSet.LATABLEFLD[lnPOs ,2]
      *!*              SELECT STYLE
      *!*              SCATTER FIELDS &lcFldScat. MEMO MEMVAR
      *!*            ENDIF

      *!*            IF !EMPTY(loFormSet.LATABLEPROF[1,1])
      *!*              FOR lnT = 1 TO ALEN(loFormSet.LATABLEPROF,1)
      *!*                DO CASE
      *!*                  CASE  loFormSet.LATABLEPROF[lnT,2] = 'ST' && GET PROFILE VALUE FOR SALES Order Header
      *!*                    IF gfSeek(IIF(POSLN.CBUSDOCU+POSLN.Cstytype = 'PP',"POPP",'CTPU')+PADR(POSHDR.PO+STR(POSLN.LINENO,6),130)+loFormSet.LATABLEPROF[lnT,1],"PROFVALU")
      *!*                      lcPrFFld = 'P'+loFormSet.LATABLEPROF[lnT,1]
      *!*                      m.&lcPrFFld. = PROFVALU.cpro_value
      *!*                    ENDIF

      *!*                  CASE  loFormSet.LATABLEPROF[lnT,2] = 'VN' AND POSHDR.Cstytype+POSHDR.CBUSDOCU ='PP'&& GET PROFILE VALUE FOR PO Order Header
      *!*                    IF gfSeek("POPP"+PADR(POSHDR.PO,130)+loFormSet.LATABLEPROF[lnT,1],"PROFVALU")
      *!*                      lcPrFFld = 'P'+loFormSet.LATABLEPROF[lnT,1]
      *!*                      m.&lcPrFFld. = PROFVALU.cpro_value
      *!*                    ENDIF
      *!*                ENDCASE
      *!*              ENDFOR
      *!*            ENDIF



      *!*            =gfSeek(POSLN.STYLE,'STYLE')
      *!*            =gfSeek('S'+STYLE.SCALE,'SCALE')
      *!*            llTaskFnd   = .F.

      *!*            FOR lnI =1 TO ALEN(loFormSet.LATEMPACT,1)
      *!*              lcI = ALLTRIM(STR(lnI))
      *!*              m.cOprID&lcI.= ''
      *!*              m.cOpDsc&lcI.= ''
      *!*              m.COPCTG&lcI.= ''
      *!*              m.cTskSt&lcI.= ''
      *!*              m.dAcFnh&lcI.= {}
      *!*              m.DClFnh&lcI.= {}
      *!*              m.mOPCOM&lcI. = ""
      *!*            ENDFOR

      *!*            lfTaskList()
      *!*            FOR lnR = 1 TO ALEN(loFormSet.LATEMPFL,1)
      *!*              IF !EMPTY(loFormSet.LATEMPFL[lnR,1]) AND USED(loFormSet.LATEMPFL[lnR,1])
      *!*                INSERT INTO (loFormSet.LATEMPFL[lnR,1]) FROM MEMVAR
      *!*              ENDIF
      *!*            ENDFOR
      *!*          ENDSCAN
      *!*        ENDIF
  ENDCASE

  *!*************************************************************
  *! Name      : lfGetTasks
  *! Developer : Mariam Mazhar
  *! Date      : 06/01/2009
  *! Purpose   : Get Tasks of Project
  *!*************************************************************
FUNCTION lfTaskList
  SELECT PMPRJDT
  gfSetOrder('PMPRJDTS')
  IF gfSeek(PmPrjHd.cprj_typ+PmPrjHd.cprj_id+PmPrjHd.cstyle+STR(PmPrjHd.LINENO,6),'PMPRJDT')
    SELECT PMPRJDT
    SCAN REST WHILE cprj_typ+cprj_id+cstyle+STR(LINENO,6)+COPRT_CTG+COPRT_ID = PmPrjHd.cprj_typ+PmPrjHd.cprj_id+PmPrjHd.cstyle+STR(PmPrjHd.LINENO,6);
        FOR IIF(llTskSelected,SEEK(COPRT_CTG+'-'+COPRT_ID,lcTskSel),.T.) AND IIF(lcRpRprt = 'C',!lshw2cust,.T.)
      IF llNtfySelected
        IF gfSeek(PMPRJDT.cprj_typ+PMPRJDT.cprj_id+PMPRJDT.cstyle+PMPRJDT.COPRT_CTG+PMPRJDT.COPRT_ID,'PMPRJNTF')
          llUsrFound = .F.
          SELECT PMPRJNTF
          SCAN REST WHILE cprj_typ+cprj_id+cstyle+STR(LINENO,6)+COPRT_CTG+COPRT_ID+cUser_Id = ;
              PMPRJDT.cprj_typ+PMPRJDT.cprj_id+PMPRJDT.cstyle+STR(PMPRJDT.LINENO,6)+PMPRJDT.COPRT_CTG+PMPRJDT.COPRT_ID ;
              FOR  SEEK(cUser_Id ,lcNTFSel)
            llUsrFound = .T.
          ENDSCAN
          IF !llUsrFound
            SELECT PMPRJDT
            LOOP
          ENDIF
        ELSE
          LOOP
        ENDIF
      ENDIF
      SELECT SYSCHDUL
      IF gfSeek(SUBSTR(PMPRJDT.cprj_typ,1,1) + SUBSTR(PMPRJDT.cprj_id,1,6) + ;
          SUBSTR(PMPRJDT.cstyle,1,19)+STR(PMPRJDT.LINENO,6)+PMPRJDT.COPRT_CTG+PMPRJDT.COPRT_ID)
*! B609555,1 MMT 03/27/2011 Task List screen gives error while completing task[Start]
        IF SYSCHDUL.COPERSTAT= 'C'
          lnOldSel = SELECT()
          SELECT SYSCHDUL
          LOCATE REST WHILE CCONTTYPE+CSEQNUMBER+CSTYLE+STR(LINENO,6)+CCONT_ID+COPERSTAT+CUSER_ID=SUBSTR(PMPRJDT.cprj_typ,1,1) + SUBSTR(PMPRJDT.cprj_id,1,6) + ;
  SUBSTR(PMPRJDT.cstyle,1,19)+STR(PMPRJDT.LINENO,6)+PMPRJDT.COPRT_CTG+PMPRJDT.COPRT_ID FOR COPERSTAT <> 'C'
          IF !FOUND()
            =gfSeek(SUBSTR(PMPRJDT.cprj_typ,1,1) + SUBSTR(PMPRJDT.cprj_id,1,6) + ;
  SUBSTR(PMPRJDT.cstyle,1,19)+STR(PMPRJDT.LINENO,6)+PMPRJDT.COPRT_CTG+PMPRJDT.COPRT_ID,'SYSCHDUL')
          ENDIF 
          SELECT(lnOldSel) 
        ENDIF 
        *! B609555,1 MMT 03/27/2011 Task List screen gives error while completing task[End]           
          
        IF lcRpOprStt $ 'OC'
          IF SYSCHDUL.COPERSTAT <> lcRpOprStt
            SELECT PMPRJDT
            LOOP
          ENDIF
        ELSE
          IF lcRpOprStt = 'L'
            IF EMPTY(PMPRJDT.DCLC_FNSH) OR (!EMPTY(PMPRJDT.dAct_Fnsh) AND (PMPRJDT.dAct_Fnsh =< PMPRJDT.DCLC_FNSH)) OR ;
                (SYSCHDUL.COPERSTAT <> 'C' AND EMPTY(PMPRJDT.dAct_Fnsh) AND (oAriaApplication.SystemDate =< PMPRJDT.DCLC_FNSH))
              SELECT PMPRJDT
              LOOP
            ENDIF
          ENDIF
        ENDIF
        *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
        *IF lcRpOprStt = 'O' AND LNRPDYCMP <> 0 AND (EMPTY(PMPRJDT.DCLC_FNSH) OR (PMPRJDT.DCLC_FNSH <> oAriaApplication.SystemDate + LNRPDYCMP))        
        IF lcRpOprStt = 'O' AND LNRPDYCMP <> 0 AND (EMPTY(PMPRJDT.DCLC_FNSH) OR (PMPRJDT.DCLC_FNSH > oAriaApplication.SystemDate + LNRPDYCMP))
        *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
          LOOP
        ENDIF

        llTaskFnd = .T.
        IF lcRpPrjTp = 'O' AND llRpShWDy AND PMPRJDT.COPRT_CTG+"-"+PMPRJDT.COPRT_ID = lcRpTaskNom
          lnDateDiff = OrdHdr.COMPLETE - PMPRJDT.DCLC_FNSH
          m.nTaskStat = lnDateDiff
          lcNomDesc =  PMPRJDT.COPRT_DSC
        ENDIF
        IF ASCAN(loFormSet.LATEMPACT,PADR(PMPRJDT.COPRT_CTG,3)+PADR(PMPRJDT.COPRT_ID,5)) > 0
          lnPOs = ASUBSCRIPT(loFormSet.LATEMPACT,ASCAN(loFormSet.LATEMPACT,PADR(PMPRJDT.COPRT_CTG,3)+PADR(PMPRJDT.COPRT_ID,5)),1)
          lcTaskNum = ALLTRIM(STR(lnPOs))
          m.cOprID&lcTaskNum. = PMPRJDT.COPRT_ID
          m.cOpDsc&lcTaskNum. = PMPRJDT.COPRT_DSC
          m.COPCTG&lcTaskNum. = PMPRJDT.COPRT_CTG
          IF !EOF('SYSCHDUL')
            IF SYSCHDUL.COPERSTAT  = 'C'
              m.cTskSt&lcTaskNum. = 'Completed '+DTOC(PMPRJDT.dAct_Fnsh)
            ELSE
              IF !EOF('SYSCHDUL')
                m.cTskSt&lcTaskNum. = 'Due  '+DTOC(PMPRJDT.DCLC_FNSH)
              ELSE
                m.cTskSt&lcTaskNum. = ''
              ENDIF
            ENDIF
          ENDIF
          m.dAcFnh&lcTaskNum. = PMPRJDT.dAct_Fnsh
          m.DClFnh&lcTaskNum. = PMPRJDT.DCLC_FNSH
          m.mOPCOM&lcTaskNum. = m.cTskSt&lcTaskNum.+CHR(13)+CHR(10)+PMPRJDT.mOprt_Com
*!*          ELSE
*!*            lcTaskNum = ALLTRIM(STR(lnTaskNum))
*!*            m.cTskSt&lcTaskNum. = ""
        ENDIF
      ENDIF
    ENDSCAN
  ENDIF
  SELECT PMPRJDT
  gfSetOrder('PMPRJDT')


  *!*************************************************************
  *! Name      : lfTSK
  *! Developer : Mariam Mazhar
  *! Date      : 06/01/2009
  *! Purpose   : Add Task to browse screen
  *!*************************************************************
FUNCTION lfTSK
  PARAMETERS lcIndx
  *Get the Cursor Name that hold Activities
  *T3 = ''
  lcFlNm = ''
  lcFilName = ''
  FOR lnC = 1 TO ALEN(loFormSet.LATEMPFL,1)
    lcFldName = loFormSet.LATEMPFL[lnC,1]+'.DClFnh'+ALLTRIM(lcIndx)
    IF TYPE(lcFldName) = 'U'
      LOOP
    ELSE
      lcFlNm  = loFormSet.LATEMPFL[lnC,2]
      lcFilName = loFormSet.LATEMPFL[lnC,1]
      EXIT 
    ENDIF
  ENDFOR
  IF TYPE('oBrowse') <> 'O'
    lcExpRet = '<VFP:CustomObject SourceControl = "cTskSt'+lcIndx+'"'
  ELSE

    lcClrBind = [IIF(EMPT(EVAL(]+lcFlNm+[+'.dAcFnh&lcIndx')) AND B>0 AND BETW(EVAL(]+lcFlNm+[+'.DClFnh&lcIndx'),ldSD,]+;
      [ldSD+B),A,IIF(EMPT(EVAL(]+lcFlNm+[+'.dAcFnh&lcIndx')) AND C>0 AND BETW(EVAL(]+;
      lcFlNm+[+'.DClFnh&lcIndx'),ldSD-C,]+;
      [ldSD-1),D,]+;
      [IIF(EMPT(EVAL(]+lcFlNm+[+'.dAcFnh&lcIndx')) AND C>0 And EVAL(]+;
      lcFlNm+[+'.DClFnh&lcIndx')<ldSD-C,E,0)))"]

    lcExpRet = [<VFP:CustomObject txtBoxDelg ="lfGetTNot" ObjectName = "PMPRJCNT"  SourceControl = "]+;
      loFormSet.LATEMPFL[1,1]+[.cTskSt]+lcIndx+[" HandlerObject= "loParentForm" Delegate = "lfEdtTTask"  DynamicForeColor = "]+lcClrBind+["]

    IF TYPE('GLOBALBROWSEWINDOW') = 'O'
      IF GLOBALBROWSEWINDOW.Container1.chkColumn.ENABLED = .F.
        GLOBALBROWSEWINDOW.Container1.chkColumn.ENABLED = .T.
        GLOBALBROWSEWINDOW.Custfields = loFormSet.lcTempFld
        BINDEVENT(GLOBALBROWSEWINDOW.Container1.chkExport,'Valid',loParentForm,'lfOrdBrowExp')
        BINDEVENT(GLOBALBROWSEWINDOW.Container1.chkExport,'Valid',loParentForm,'lfReOrdBrowExp',1)
        BINDEVENT(GLOBALBROWSEWINDOW.AriaGrid1,'restorepreference',loParentForm,'lfRestorePreference',1)
*        BINDEVENT(GLOBALBROWSEWINDOW,'mColumnMouseDown',loParentForm,'lfColumnDown',0)

        BINDEVENT(GLOBALBROWSEWINDOW,'assignimages',loParentForm,'lfReColumndown',1)
        BINDEVENT(GLOBALBROWSEWINDOW,'mColumnMouseDown',loParentForm,'lfReColumndown',1)
        BINDEVENT(GLOBALBROWSEWINDOW,'getfilefields',loParentForm,'lfgetfilefields',1)
        GLOBALBROWSEWINDOW.Container1.cmdselect.VISIBLE = .F.
        *  E302694,1 MMT 05/16/2010 Add Shortcut to Project Monitor to complete tasks[Start]
        GLOBALBROWSEWINDOW.AriaGrid1.Column1.Readonly = .F.        
        *  E302694,1 MMT 05/16/2010 Add Shortcut to Project Monitor to complete tasks[End]
      ENDIF
    ENDIF

  ENDIF

  IF TYPE('GLOBALBROWSEWINDOW.laforecolors[1]') = 'U' OR TYPE('GLOBALBROWSEWINDOW.laforecolors[1]') = 'L'
    IF A <> 0 OR D <> 0 OR E <> 0
      DIMENSION GLOBALBROWSEWINDOW.laforecolors[1]
      STORE '' TO GLOBALBROWSEWINDOW.laforecolors
      *B609386,1 MMT 08/25/2010 Error while export to Excel when same color selected twice[Start]
      *IF A <> 0
      IF A <> 0  AND ASCAN(GLOBALBROWSEWINDOW.laforecolors,A) = 0
      *B609386,1 MMT 08/25/2010 Error while export to Excel when same color selected twice[End]
        IF EMPTY(GLOBALBROWSEWINDOW.laforecolors[1])
          GLOBALBROWSEWINDOW.laforecolors[1]= A
        ELSE
          DIMENSION GLOBALBROWSEWINDOW.laforecolors[ALEN(GLOBALBROWSEWINDOW.laforecolors,1)+1]
          GLOBALBROWSEWINDOW.laforecolors[ALEN(GLOBALBROWSEWINDOW.laforecolors,1)]= A
        ENDIF
      ENDIF
      *B609386,1 MMT 08/25/2010 Error while export to Excel when same color selected twice[Start]
      *IF D <> 0      
      IF D <> 0 AND ASCAN(GLOBALBROWSEWINDOW.laforecolors,D) = 0
      *B609386,1 MMT 08/25/2010 Error while export to Excel when same color selected twice[End]      
        IF EMPTY(GLOBALBROWSEWINDOW.laforecolors[1])
          GLOBALBROWSEWINDOW.laforecolors[1]= D
        ELSE
          DIMENSION GLOBALBROWSEWINDOW.laforecolors[ALEN(GLOBALBROWSEWINDOW.laforecolors,1)+1]
          GLOBALBROWSEWINDOW.laforecolors[ALEN(GLOBALBROWSEWINDOW.laforecolors,1)]= D
        ENDIF
      ENDIF
      *B609386,1 MMT 08/25/2010 Error while export to Excel when same color selected twice[Start]
      *IF E <> 0      
      IF E <> 0 AND ASCAN(GLOBALBROWSEWINDOW.laforecolors,E) = 0
      *B609386,1 MMT 08/25/2010 Error while export to Excel when same color selected twice[End]      
        IF EMPTY(GLOBALBROWSEWINDOW.laforecolors[1])
          GLOBALBROWSEWINDOW.laforecolors[1]= E
        ELSE
          DIMENSION GLOBALBROWSEWINDOW.laforecolors[ALEN(GLOBALBROWSEWINDOW.laforecolors,1)+1]
          GLOBALBROWSEWINDOW.laforecolors[ALEN(GLOBALBROWSEWINDOW.laforecolors,1)]= E
        ENDIF
      ENDIF
    ENDIF
  ENDIF
  RETURN lcExpRet
  
FUNCTION lfFLD
PARAMETERS lcFldID
lcFieldName = ''
*  E302650,2 MMT 12/24/2009 make project monitor work based on report templated selected [Start]
*!*  DO CASE 
*!*   CASE lcFldID = 'A'
*!*     lcFieldName = 'ACCOUNT'
*!*   CASE lcFldID = 'C' 
*!*     lcFieldName = 'CPRJ_ID' 
*!*   CASE lcFldID = 'V' 
*!*     lcFieldName = 'VENDOR' 
*!*   CASE lcFldID = 'P'
*!*     lcFieldName = 'PO' 
*!*   CASE lcFldID = 'O'
*!*     lcFieldName = 'ORDER' 
*!*   CASE lcFldID = 'S'
*!*     lcFieldName = 'STYLE' 
*!*   CASE lcFldID = 'T'
*!*     lcFieldName = 'STORE' 
*!*   CASE lcFldID = 'D'    
*!*     lcFieldName = 'CVENDCODE' 
*!*  ENDCASE 
lcFieldName = lcFldID
*  E302650,2 MMT 12/24/2009 make project monitor work based on report templated selected [End]
lcFlNm = ''
lcFilName = ''
LOCAL lnC
FOR lnC = 1 TO ALEN(loFormSet.LATEMPFL,1)
  lcFldName = loFormSet.LATEMPFL[lnC,1]+'.'+ALLTRIM(lcFieldName)
  IF TYPE(lcFldName) = 'U'
    LOOP
  ELSE
    lcFlNm  = loFormSet.LATEMPFL[lnC,2]
    lcFilName = loFormSet.LATEMPFL[lnC,1]
    EXIT 
  ENDIF
ENDFOR


IF TYPE('oBrowse') <> 'O'
  lcExpRet = '<VFP:CustomObject SourceControl = "'+lcFilName+'.'+lcFieldName+'"'
ELSE

  lcExpRet = [<VFP:CustomObject  ObjectName = "PMPRJCNT"  SourceControl = "]+;
    lcFilName+[.]+lcFieldName+[" HandlerObject= "loParentForm" Delegate = "lfOpnTr" DynamicForeColor = "0"]

  IF TYPE('GLOBALBROWSEWINDOW') = 'O'
    IF GLOBALBROWSEWINDOW.Container1.chkColumn.ENABLED = .F.
      GLOBALBROWSEWINDOW.Container1.chkColumn.ENABLED = .T.
      GLOBALBROWSEWINDOW.Custfields = loFormSet.lcTempFld
      BINDEVENT(GLOBALBROWSEWINDOW.Container1.chkExport,'Valid',loParentForm,'lfOrdBrowExp')
      BINDEVENT(GLOBALBROWSEWINDOW.Container1.chkExport,'Valid',loParentForm,'lfReOrdBrowExp',1)
*      BINDEVENT(GLOBALBROWSEWINDOW,'mColumnMouseDown',loParentForm,'lfColumnDown',0)
      BINDEVENT(GLOBALBROWSEWINDOW,'mColumnMouseDown',loParentForm,'lfReColumndown',1)
      BINDEVENT(GLOBALBROWSEWINDOW,'assignimages',loParentForm,'lfReColumndown',1)
      BINDEVENT(GLOBALBROWSEWINDOW,'getfilefields',loParentForm,'lfgetfilefields',1)
      
      BINDEVENT(GLOBALBROWSEWINDOW.AriaGrid1,'restorepreference',loParentForm,'lfRestorePreference',1)
      GLOBALBROWSEWINDOW.Container1.cmdselect.VISIBLE = .F.
      *  E302694,1 MMT 05/16/2010 Add Shortcut to Project Monitor to complete tasks[Start]
      GLOBALBROWSEWINDOW.AriaGrid1.Column1.Readonly = .F.      
      *  E302694,1 MMT 05/16/2010 Add Shortcut to Project Monitor to complete tasks[End]
    ENDIF
  ENDIF

ENDIF
RETURN lcExpRet

*  E302694,1 MMT 05/16/2010 Add Shortcut to Project Monitor to complete tasks[Start]
*!*************************************************************
*! Name      : lfChkShrtCutOpns
*! Developer : Mariam Mazhar
*! Date      : 05/16/2010
*! Purpose   : Shortcut Option enable\Disbale
*!*************************************************************
FUNCTION lfChkShrtCutOpns
LPARAMETERS lcCurrTsk
lnCurAlias = SELECT()
lcCurTskNum = 0
lnDotPos = ATC('.',lcCurrTsk)
lcJustTsk = SUBSTR(lcCurrTsk,lnDotPos+1)
lcCurTskNum = STRTRAN(UPPER(lcJustTsk) ,'CTSKST','')
lcBars   = LANG_MFPROJMON_CompleteShct
llCanComp = .F.
llComplAll = .F.
llNonSelected = .F.
IF !EMPTY(LCRPRPTTM) AND LCRPRPTTM <> 'N/A' AND !EMPTY(loFormSet.LATEMPFL[1,1])AND USED(loFormSet.LATEMPFL[1,1])
  llCanComp = lfCanComplTask(lcCurrTsk)
  *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
  *SELECT(loFormSet.LATEMPFL[1,1])
  *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
  lnCurRecord = RECNO()
  LOCATE FOR lSelect
  IF FOUND()
    llComplAll = .T.
  ELSE
    llComplAll = .F.    
  ENDIF
  LOCATE FOR !lSelect   
  IF FOUND()
    llNonSelected = .T.
  ELSE
    llNonSelected = .F.    
  ENDIF
  IF BETWEEN(lnCurRecord,1,RECCOUNT())
    GO RECORD lnCurRecord
  ENDIF 

ELSE

  llCanComp = lfCanComplTask(lcCurrTsk)
  *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
  *SELECT(loFormSet.lcTempFile)
  *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
  lnCurRecord = RECNO()
  LOCATE FOR lSelect
  IF FOUND()
    llComplAll = .T.
  ELSE
    llComplAll = .F.    
  ENDIF 
  LOCATE FOR !lSelect   
  IF FOUND()
    llNonSelected = .T.
  ELSE
    llNonSelected = .F.    
  ENDIF  
  IF BETWEEN(lnCurRecord,1,RECCOUNT())
    GO RECORD lnCurRecord
  ENDIF 
ENDIF 

lcStatus = IIF(llCanComp ,'T','F')
lcStatus = lcStatus + IIF(llComplAll ,'T','F')
lcStatus = lcStatus + 'T'
lcStatus = lcStatus + IIF(llNonSelected,'T','F') 
lcStatus = lcStatus + IIF(llComplAll ,'T','F') 
SELECT(lnCurAlias) 
*!*************************************************************
*! Name      : lfCompleteTask
*! Developer : Mariam Mazhar
*! Date      : 05/16/2010
*! Purpose   : Complete Task
*!*************************************************************
FUNCTION lfCompleteTask
LPARAMETERS lnBarSelect,lcCurrTsk
*  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
LOCAL lnCurAlias,lnCurRecord 
*  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
lnCurAlias = SELECT()
lnCurRecord = RECNO()
lcCurTskNum = 0
lnDotPos = ATC('.',lcCurrTsk)
lcJustTsk = SUBSTR(lcCurrTsk,lnDotPos+1)
lcCurTskNum = STRTRAN(UPPER(lcJustTsk) ,'CTSKST','')
lcTaskIdCnt = lcCurrTsk
lnBackClor = 16777215
DO CASE  
  CASE lnBarSelect =1 
    WAIT WINDOW LANG_MFPROJMON_COMPACT NOWAIT 
    IF !EMPTY(LCRPRPTTM) AND LCRPRPTTM <> 'N/A' AND !EMPTY(loFormSet.LATEMPFL[1,1])AND USED(loFormSet.LATEMPFL[1,1])
      loFormSet.lfEdtTTask(.T.)
    ELSE
      loFormSet.lfEdtTask(.T.)
    ENDIF 
  CASE lnBarSelect =2
     SCAN FOR lSelect
       *Need to check first if task can be completed or not
       WAIT WINDOW LANG_MFPROJMON_COMPACTS  NOWAIT 
       IF !lfCanComplTask(lcCurrTsk)
         LOOP
       ENDIF 
       IF !EMPTY(LCRPRPTTM) AND LCRPRPTTM <> 'N/A' AND !EMPTY(loFormSet.LATEMPFL[1,1])AND USED(loFormSet.LATEMPFL[1,1])
         loFormSet.lfEdtTTask(.T.)
       ELSE
         loFormSet.lfEdtTask(.T.)
       ENDIF 
     ENDSCAN
  CASE lnBarSelect = 4
     REPLACE ALL lSelect WITH .T.
  CASE lnBarSelect = 5
     REPLACE ALL lSelect WITH .F.
ENDCASE

SELECT(lnCurAlias) 
IF BETWEEN(lnCurRecord,1,RECCOUNT())
  GO RECORD lnCurRecord
ENDIF 
*!*************************************************************
*! Name      : lfCanComplTask
*! Developer : Mariam Mazhar
*! Date      : 05/16/2010
*! Purpose   : Check if can Complete Task
*!*************************************************************
FUNCTION lfCanComplTask
LPARAMETERS lcCurrTsk
lnCurAlias = SELECT()
lcCurTskNum = 0
lnDotPos = ATC('.',lcCurrTsk)
lcJustTsk = SUBSTR(lcCurrTsk,lnDotPos+1)
lcCurTskNum = STRTRAN(UPPER(lcJustTsk) ,'CTSKST','')
LOCAL llCanComp 
llCanComp = .F.
IF !EMPTY(LCRPRPTTM) AND LCRPRPTTM <> 'N/A' AND !EMPTY(loFormSet.LATEMPFL[1,1])AND USED(loFormSet.LATEMPFL[1,1])
  *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
  IF TYPE('GLOBALBROWSEWINDOW') = 'O' AND UPPER(GLOBALBROWSEWINDOW.ALIAS)<>UPPeR(loFormSet.LATEMPFL[1,1])
  *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
    lcPrjTyp = EVALUATE(loFormSet.LATEMPFL[1,1]+'.CPRJ_TYP')
    lcPrjId = EVALUATE(loFormSet.LATEMPFL[1,1]+'.CPRJ_ID')
    lcStyle = EVALUATE(loFormSet.LATEMPFL[1,1]+'.CSTYLE')
    lnLineNo = EVALUATE(loFormSet.LATEMPFL[1,1]+'.LINENO')
  *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
  ELSE
    lcPrjTyp = EVALUATE(loFormSet.LATEMPFL[1,1]+'.CPRJ_TYP')
    lcPrjId = EVALUATE(loFormSet.LATEMPFL[1,1]+'.CPRJ_ID')
    lcStyle = EVALUATE(loFormSet.LATEMPFL[1,1]+'.CSTYLE')
    lnLineNo = EVALUATE(loFormSet.LATEMPFL[1,1]+'.LINENO')
  ENDIF  
  *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
 
  *Get The Temp That Hold the Task ID and Category
  lcTaskID = STRTRAN(UPPER(lcCurrTsk) ,UPPER('cTskSt'),'COPRID')
  lnDotPos = ATC('.', lcTaskID)
  lcTaskFld = ''
  IF lnDotPos > 0
    lcTaskFld = SUBSTR(lcTaskID ,lnDotPos+1)
  ENDIF   
  lcFlNm = ''
  lcFilName = ''
  IF !EMPTY(lcTaskFld)
    FOR lnC = 1 TO ALEN(loFormSet.LATEMPFL,1)
      lcFldName = loFormSet.LATEMPFL[lnC,1]+'.'+ALLTRIM(lcTaskFld)
      IF TYPE(lcFldName) = 'U'
        LOOP
      ELSE
        lcTaskFld = loFormSet.LATEMPFL[lnC,1]+"."+ALLTRIM(lcTaskFld)
        lcFilName = loFormSet.LATEMPFL[lnC,1]
        EXIT 
      ENDIF
    ENDFOR
  ENDIF 

  
  IF !EMPTY(lcTaskFld)
    lcTaskID  = EVALUATE(lcTaskFld)
    lcCtgId = EVALUATE(STRTRAN(UPPER(lcTaskFld),UPPER('cOprID'),'COPCTG'))  
    lcOprtID  = lcTaskID  
  ENDIF 
  
  *Get The File That Hold the task actual Complete Date
  lcFlNm = ''
  lcFilName = ''
  FOR lnC = 1 TO ALEN(loFormSet.LATEMPFL,1)
    lcFldName = loFormSet.LATEMPFL[lnC,1]+'.DACFnh'+ALLTRIM(lcCurTskNum)
    IF TYPE(lcFldName) = 'U'
      LOOP
    ELSE
      lcFlNm  = loFormSet.LATEMPFL[lnC,2]
      lcFilName = loFormSet.LATEMPFL[lnC,1]
      EXIT 
    ENDIF
  ENDFOR
  
  *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
  IF UPPER(lcFilName) = UPPER(loFormSet.LATEMPFL[1,1])
    IF TYPE('GLOBALBROWSEWINDOW') = 'O' AND UPPER(GLOBALBROWSEWINDOW.ALIAS)<>UPPeR(loFormSet.LATEMPFL[1,1])
      lcFilName = GLOBALBROWSEWINDOW.ALIAS
    ENDIF
  ENDIF
  *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
  
  IF !EMPTY(lcFilName) 
    IF !EMPTY(EVALUATE(ALLTRIM(lcFldName))) OR EMPTY(EVALUATE(lcCurrTsk))
      llCanComp = .F.
    ELSE
      llCanComp = .T.
    ENDIF   
  ENDIF 
ELSE
  *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
  IF TYPE('GLOBALBROWSEWINDOW') = 'O' AND UPPER(GLOBALBROWSEWINDOW.ALIAS)<>UPPeR(loFormSet.lcTempFile)
    lcPrjTyp = EVALUATE(GLOBALBROWSEWINDOW.ALIAS+'.CPRJ_TYP')
    lcPrjId = EVALUATE(GLOBALBROWSEWINDOW.ALIAS+'.CPRJ_ID')
    lcStyle = EVALUATE(GLOBALBROWSEWINDOW.ALIAS+'.CSTYLE')
    lnLineNo = EVALUATE(GLOBALBROWSEWINDOW.ALIAS+'.LINENO')
  ELSE
  *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
    lcPrjTyp = EVALUATE(loFormSet.lcTempFile+'.CPRJ_TYP')
    lcPrjId = EVALUATE(loFormSet.lcTempFile+'.CPRJ_ID')
    lcStyle = EVALUATE(loFormSet.lcTempFile+'.CSTYLE')
    lnLineNo = EVALUATE(loFormSet.lcTempFile+'.LINENO')
  *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[Start]
  ENDIF
  *  B609473,1 MMT 12/14/2010 Completing task in Project Monitor takes long time[End]
  lcOprtID = EVALUATE(STRTRAN(lcCurrTsk,'cTskSt','cOprID'))
  lcCtgId= EVALUATE(STRTRAN(lcCurrTsk,'cTskSt','COPCTG'))
  IF !EMPTY(EVAL(T3+'.dAcFnh'+ALLTRIM(lcCurTskNum))) OR EMPTY(EVALUATE(lcCurrTsk))
    llCanComp = .F.
  ELSE
    llCanComp = .T.
  ENDIF 
ENDIF 
*Check if Task Can be Completed or not
IF llCanComp 
  =gfSEEK(SUBSTR(lcPrjTyp,1,LEN(cPrj_Typ)) + SUBSTR(lcPrjId,1,LEN(cPrj_ID)) +  SUBSTR(lcStyle,1,LEN(cStyle)) +;
          STR(lnLineNo,6)+lcCtgId+lcOprtID,'SYSCHDUL','COPRUSR') 
*! B609555,1 MMT 03/27/2011 Task List screen gives error while completing task[Start]
                IF SYSCHDUL.COPERSTAT= 'C'
                  lnOldSel = SELECT()
                  SELECT SYSCHDUL
                  LOCATE REST WHILE CCONTTYPE+CSEQNUMBER+CSTYLE+STR(LINENO,6)+CCONT_ID+COPERSTAT+CUSER_ID=SUBSTR(lcPrjTyp,1,LEN(cPrj_Typ)) + SUBSTR(lcPrjId,1,LEN(cPrj_ID)) +  SUBSTR(lcStyle,1,LEN(cStyle)) +;
          STR(lnLineNo,6)+lcCtgId+lcOprtID FOR COPERSTAT <> 'C'
                  IF !FOUND()
                    =gfSeek(SUBSTR(lcPrjTyp,1,LEN(cPrj_Typ)) + SUBSTR(lcPrjId,1,LEN(cPrj_ID)) +  SUBSTR(lcStyle,1,LEN(cStyle)) +;
          STR(lnLineNo,6)+lcCtgId+lcOprtID,'SYSCHDUL')
                  ENDIF 
                  SELECT(lnOldSel) 
                ENDIF 
                **! B609555,1 MMT 03/27/2011 Task List screen gives error while completing task[End]           
  IF EOF('SYSCHDUL') OR !(SYSCHDUL.COPERSTAT = 'O' AND SYSCHDUL.LPREDCOMP)
    llCanComp  = .F.
  ENDIF   
ENDIF 
RETURN llCanComp
*  E302694,1 MMT 05/16/2010 Add Shortcut to Project Monitor to complete tasks[End]

*  B609778,1 MMT 12/25/2011 fix bug of not Displaying the Cancelled projects[Start]     
*!*************************************************************
*! Name      : lfvOStatus
*! Developer : Mariam Mazhar (MMT)
*! Date      : 12/25/2011
*! Purpose   : - Evaluate Status expression.
*!           : - Raise change status flag. 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : String have Pipes,Number of Pieps.
*!*************************************************************
*! Returns            : InList Expression like ["AS","BS","CS"]
*!*************************************************************
*! Example   : = lfvOStatus()
*!*************************************************************
FUNCTION lfvOStatus
PRIVATE lcOldStat,lcCurrChr

lcOldStat = lcRpStatus  && Save old status value.
=gfMover(@laRpSource,@laRpTarget,'Select Project Status',.T.,'')  && call mover function.

lcRpPrjStt = ' '
*-- Loop to make Status expression.

IF !EMPTY(laRpTarget[1])
  FOR lnI = 1 TO ALEN(laRpTarget,1)
    lcRpPrjStt= lcRpPrjStt+ IIF(laRpTarget[lnI] = 'Planning','P',;
                              IIF(laRpTarget[lnI] = 'In progress','I',;
                              IIF(laRpTarget[lnI] = 'Complete','C',;
                              IIF(laRpTarget[lnI] = 'Cancelled','X',''))))
  ENDFOR  && end Loop to make Status expression.
ENDIF
IF EMPTY(lcRpPrjStt)
  lcRpPrjStt = 'PIXC'
ENDIF
*!*****************************************************************************************
*! Name      : RefreshStatus
*! Developer : Mariam Mazhar (MMT)
*! Date      : 12/25/2011
*! Purpose   : Refresh Status option selection text box
*! Entry no. : B609778
*!*****************************************************************************************
*!
FUNCTION RefreshStatus
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
*-- end of RefreshStatus.
*  B609778,1 MMT 12/25/2011 fix bug of not Displaying the Cancelled projects[END]     