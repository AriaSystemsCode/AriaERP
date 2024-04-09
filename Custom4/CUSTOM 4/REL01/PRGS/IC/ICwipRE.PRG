***********************************************************************
*:  Program File: ICWIPRE.PRG
*:  Desc.       : Custom WIP report for RELIQ
*:  System      : Aria 4XP
*:  Developer   : MMT - Mariam Mazhar
*:  Date        : 07/14/2021
*:  Reference   : C202426
*:************************************************************************
WAIT WINDOW 'Collecting report data...' NOWAIT 
oCrystal = CreateObject("CrystalRunTime.Application")
*oCrystal = CreateObject("CrystalDesignRunTime.Application.13")
lcReportFileName = ADDBS(UPPER(oAriaApplication.ClientReportHome))+"\IC\ICWIP.rpt"
oReport  = oCrystal.OpenReport(lcReportFileName)
LOCAL lcDataPath As String
oReport.EnableParameterPrompting = .F.
LOCAL lcLastSubReport, lnTableIndex
lcLastSubReport = ""
lnTableIndex    = 0
 =gfOpenTable("Style","Style",'SH')
lcTempFile = gfTempName()
SELECT * FROM STYLE WHERE !DELETED() INTO DBF (oAriaApplication.WorkDir+lcTempFile+".DBF") 
lnTableIndex = 1
oReport.Database.Tables.Item[lnTableIndex].Setlogoninfo (oAriaApplication.WorkDir+lcTempFile+".DBF")
oReport.Database.Tables.Item[lnTableIndex].SetTableLocation (oAriaApplication.WorkDir+lcTempFile+".DBF",'','')
lcConnBuff ="Provider=VFPOLEDB.1;Data Source="+oAriaApplication.WorkDir+lcTempFile+".DBF"+";Password='';userid=''"
oReport.Database.Tables.Item[lnTableIndex].SetTableLocation (JUSTSTEM(oAriaApplication.WorkDir+lcTempFile+".DBF"),JUSTSTEM(oAriaApplication.WorkDir+lcTempFile+".DBF"),lcConnBuff)
oReport.Database.Tables.Item(lnTableIndex).TestConnectivity()
oReport.DiscardSavedData()
WAIT WINDOW 'Collecting report data...' NOWAIT 
*oReport.ConvertDateTimeType = 1  && crConvertDateTimeToDate 
oReport.CaseInsensitiveSQLData = .T.
loViewer = NEWOBJECT("crviewer",ADDBS(oAriaApplication.ClassDir) + "Crystal.vcx")
WITH loViewer
    .caption  = "WIP Report"
    .oCrystal = oCrystal 
    .oReport  = oReport
    .mSetup()
    .Show()
    .Destroy()
  ENDWITH
  oReport  = NULL
  oCrystal = NULL
  WAIT CLEAR 