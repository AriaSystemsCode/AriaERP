<%@ Language=VBScript %>
<%

Response.Buffer = True
'check paths entered by user[satrt]
'Set objCheck = Server.CreateObject("Scripting.FileSystemObject")
'Response.Write objCheck.FolderExists (Request("flDataFiles")) 
 
'if objCheck.FolderExists (Request("flDataFiles")) and objCheck.FolderExists (Request("flSystemPath")) then
'else
'	Response.Write "Invalid data path, please correct<br>"
'	Response.Write ("<a href=javascript:window.history.back();>Back</a>")
'	Response.End 
'end if

'check paths entered by user[end]
'ARD - Enhance CRM Setup [start]
Dim strAppPath
Dim strFilePath
strAppPath = Trim(Request.ServerVariables("APPL_PHYSICAL_PATH"))
If Right(strAppPath,1) = "\" Then
	strFilePath = "admin\crmsetup\setup\setup.txt"
Else
	strFilePath = "\admin\crmsetup\setup\setup.txt"
End If
''Response.Write(strAppPath & strFilePath)

'Dim objFileSystem
'Dim objTextFile
'Set objFileSystem = Server.CreateObject("Scripting.FileSystemObject")
'Set objTextFile = objFileSystem.CreateTextFile(strAppPath & strFilePath , True)

'ARD - Enhance CRM Setup [End]

strLine = ""
strLine = strLine & "CompanyID=" & Request("txtCompID")
strLine = strLine & " AND " & "DataPath=" & Request("flDataFiles")
Session("Data") = Request("flDataFiles")
strLine = strLine & " AND " & "SystemPath=" & Request("flSystemPath")
strLine = strLine & " AND " & "LogoPath=" & Request("LogoPath")
strLine = strLine & " AND " & "SQLServer=" & Request("txtServerName")
strLine = strLine & " AND " & "DBName=CRM"' & Request("txtDBName")
strLine = strLine & " AND " & "SqlUserName=" & Request("txtsqluser")
strLine = strLine & " AND " & "SqlPassWord=" & Request("txtsqlpas")
strLine = strLine & " AND " & "DBType=" & Request("lstSerType")

'ARD - Enhance CRM Setup [start]
Session("strLine")= strLine
Session("DBType") = Request("lstSerType")
'Response.Write(Session("strLine"))
'Response.End 
'strLine = strLine & " AND " & "UserCanEdit=" & Request("lstEditProfile")
'strLine = strLine & " AND " & "NotificationAddress=" & Request("txtNotifyAddress")
'strLine = strLine & " AND " & "ConfirmationAddress=" & Request("lstconfirmadd")
'strLine = strLine & " AND " & "OTSSetup=" & Trim(Request("lstOTS"))
'strLine = strLine & " AND " & "PasswordField=" & Request("lstPassField")
'WAL_5/5/2004_create SQL DB if not exist[start]
on error resume next
If Session("DBType") = "ORACLE" then
	strConn = "Provider=MSDATASHAPE;Data Provider=OraOLEDB.Oracle;Data Source="&Trim(Request("txtServerName"))&";user id=" & Trim(Request("txtsqluser")) & ";password=" & Trim(Request("txtsqlpas")) & ""	
Else
	strConn = "Provider=MSDATASHAPE;Data Provider=SQLOLEDB;Initial Catalog=master;Data Source="&Trim(Request("txtServerName"))&";uid=" & Trim(Request("txtsqluser")) & ";pwd=" & Trim(Request("txtsqlpas")) & ""
End If
set conn=server.CreateObject("ADODB.connection")
conn.Open strConn
if len(err.Description) > 0 then
	Response.Write err.Description 
	Response.Write ("<br><a href=javascript:window.history.back();>Back</a>")
	Response.End
end if
on error goto 0
set rsCheck  = server.CreateObject ("ADODB.Recordset")
rsCheck.Open "select * from sysdatabases where name = 'CRM'",conn,1,3
Response.Write rscheck.EOF

if rscheck.EOF and rscheck.BOF then
	conn.Execute ("create database CRM collate SQL_Latin1_General_CP1_CI_AI")
	conn.Close 
	If Session("DBType") = "ORACLE" then
		strConn = "Provider=MSDATASHAPE;Data Provider=OraOLEDB.Oracle;Data Source="&Trim(Request("txtServerName"))&";user id=" & Trim(Request("txtsqluser")) & ";password=" & Trim(Request("txtsqlpas")) & ""	
	Else
		strConn = "Provider=MSDATASHAPE;Data Provider=SQLOLEDB;Initial Catalog=CRM;Data Source="&Trim(Request("txtServerName"))&";uid=" & Trim(Request("txtsqluser")) & ";pwd=" & Trim(Request("txtsqlpas")) & ""
	End If
	
	conn.Open strConn
	conn.Execute ("CREATE TABLE [dbo].[CustClassification] ([CustID] [char] (5) NOT NULL ,[CustGroup] [int] NOT NULL ,[DispInSearch] [bit] NULL)")
	
	conn.Execute ("CREATE TABLE [dbo].[BillBlassEmail] (" &_
	"[RowID]  uniqueidentifier ROWGUIDCOL  NOT NULL ," &_
	"[strType] [varchar] (50) NULL ," &_
	"[strFirstName] [varchar] (50) NULL ," &_
	"[strLastName] [varchar] (50) NULL ," &_
	"[strtAddress1] [varchar] (50) NULL ," &_
	"[strtAddress2] [varchar] (50) NULL ," &_
	"[strCity] [varchar] (30) NULL ," &_
	"[strState] [varchar] (2) NULL ," &_
	"[strZIP] [varchar] (6) NULL ," &_
	"[strCountry] [varchar] (3) NULL ," &_
	"[strPhone] [varchar] (20) NULL ," &_
	"[strCellPhone] [varchar] (20) NULL ," &_
	"[strEmail] [varchar] (100) NULL ," &_
	"[strHearAbout] [varchar] (50) NULL ," &_
	"[dDate] [datetime] NULL " &_
	") ON [PRIMARY]")
	
	conn.Execute ("CREATE TABLE [dbo].[CustGroup] (" &_
	"[GroupID] [int] IDENTITY (1, 1) NOT FOR REPLICATION  NOT NULL ," &_
	"[Description] [varchar] (60) NULL ," &_
	"[PackID] [char] (21) NULL" &_ 
	") ON [PRIMARY]")
	
	conn.Execute ("CREATE TABLE [dbo].[Email] (" &_
	"[email] [varchar] (50) NULL ," &_
	"[cissueno] [varchar] (50) NULL " &_
	") ON [PRIMARY]")
	
	conn.Execute ("CREATE TABLE [dbo].[Privileges] (" &_
	"[cUser_ID] [char] (10) NOT NULL ," &_
	"[cTokenID] [varchar] (20) NOT NULL ," &_
	"[Profile] [char] (5) NULL " &_
	") ON [PRIMARY]")
	'wal_add primary key on the privilage table
	conn.Execute ("ALTER TABLE [dbo].[Privileges] ADD " &_
	"CONSTRAINT [PK_Privileges] PRIMARY KEY  CLUSTERED " &_
	"(" &_
		"[cUser_ID]," &_
		"[cTokenID]," &_
		"[Profile]" &_
	") ON [PRIMARY] ")
		
	conn.Execute ("CREATE TABLE [dbo].[SecurityGroups] (" &_
	"[cGroupID] [varchar] (10) NOT NULL ," &_
	"[cDescription] [varchar] (30) NULL ," &_
	"[cParentGroup] [varchar] (10) NULL " &_
	") ON [PRIMARY]")
	
	conn.Execute ("CREATE TABLE [dbo].[SecurityTokens] (" &_
	"[cTokenID] [varchar] (10) NOT NULL ," &_
	"[cDescription] [varchar] (30) NULL ," &_
	"[cGroupID] [varchar] (10) NULL " &_
	") ON [PRIMARY]")
	
	conn.Execute ("CREATE TABLE [dbo].[Trans] (" &_
	"[Contact] [varchar] (30) NULL , " &_
	"[Account] [varchar] (5) NOT NULL , " &_
	"[CADD_Time] [varchar] (11) NULL , " &_
	"[CADD_Date] [datetime] NULL , " &_
	"[TransType] [varchar] (30) NULL , " &_
	"[TransNumber] [varchar] (30) NULL , " &_
	"[Memo] [text] NULL ," &_
	"[Notify_mail] [varchar] (30) NULL , " &_
	"[Confirm_mail] [varchar] (30) NULL " &_ 
	") ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]")
	
	conn.Execute ("CREATE TABLE [dbo].[syuuser] (" &_
	"[cuser_id] [char] (10) NOT NULL , " &_
	"[cusr_name] [char] (35) NOT NULL , " &_
	"[cusr_pass] [int] NOT NULL , " &_
	"[cwe_mail] [char] (200) NULL , " &_
	"[lusr_chgp] [bit] NULL , " &_
	"[cusr_loca] [char] (30) NULL , " &_
	"[cusr_phon] [char] (17) NULL , " &_
	"[cusr_levl] [char] (1) NULL , " &_
	"[cusr_cloc] [char] (4) NULL , " &_
	"[cusr_audt] [char] (1) NULL , " &_
	"[cusr_kbuf] [char] (3) NULL ," &_  
	"[lusr_msgr] [bit] NULL , " &_ 
	"[cusr_dclr] [char] (10) NULL , " &_
	"[cusr_dcom] [char] (2) NULL , " &_ 
	"[cusr_dprt] [char] (20) NULL , " &_
	"[cusr_dwks] [char] (6) NULL , " &_ 
	"[cusr_dmdl] [char] (2) NULL , " &_
	"[cdef_bmp] [char] (12) NULL , " &_
	"[dusr_begn] [datetime] NULL , " &_
	"[dusr_end] [datetime] NULL , " &_
	"[cusr_begt] [char] (8) NULL , " &_
	"[cusr_endt] [char] (8) NULL , " &_
	"[lusr_logd] [bit] NULL , " &_
	"[dusr_lld] [smallint] NULL , " &_
	"[cusr_llt] [char] (11) NULL , " &_
	"[cusr_llot] [char] (11) NULL , " &_
	"[cusr_grup] [char] (4) NULL , " &_
	"[lusr_1st] [bit] NULL , " &_
	"[lusr_ustb] [bit] NULL , " &_
	"[cusr_resr] [char] (8) NULL , " &_
	"[lusr_dtask] [bit] NULL , " &_
	"[cextprg] [char] (10) NULL , " &_
	"[cdivision] [char] (6) NULL , " &_
	"[cadd_user] [char] (10) NULL , " &_
	"[dadd_date] [datetime] NULL , " &_
	"[cadd_time] [char] (11) NULL , " &_
	"[llok_stat] [bit] NULL , " &_
	"[clok_user] [char] (10) NULL,  " &_
	"[dlok_date] [smalldatetime] NULL , " &_
	"[clok_time] [char] (8) NULL , " &_
	"[cowner] [char] (16) NULL , " &_
	"[profile] [char] (5) NULL  " &_
	") ON [PRIMARY]")
	
	conn.Execute ("CREATE TABLE [dbo].[SuIssDt] (" &_
	"[CIssueNo] [char] (6) NOT NULL , " &_
	"[cRespType] [char] (1) NULL , " &_
	"[cRespBy] [char] (60) NULL , " &_
	"[mRespAct] [varchar] (5000) NULL , " &_
	"[DRespDate] [datetime] NULL , " &_
	"[tRespTime] [datetime] NULL , " &_
	"[intLineNo] [int] NOT NULL " &_
	") ON [PRIMARY]")
	
	conn.Execute ("CREATE TABLE [dbo].[SuIssHdr] (" &_
	"[CIssueNo] [char] (6) NOT NULL , " &_
	"[CCust_Id] [char] (5) NULL , " &_
	"[DIssStart] [datetime] NULL , " &_
	"[CIssType] [char] (6) NULL , " &_
	"[Cbug_app] [char] (3) NULL , " &_
	"[CMod_Id] [char] (10) NULL , " &_
	"[CissStat] [char] (1) NULL , " &_
	"[CIssPrior] [char] (1) NULL , " &_
	"[CissSubjct] [char] (60) NULL , " &_
	"[mIssDetail] [varchar] (5000) NULL , " &_
	"[Contact] [char] (30) NULL , " &_
	"[Cwe_mail] [char] (60) NULL , " &_
	"[Phone] [char] (16) NULL , " &_
	"[CExt] [char] (6) NULL , " &_
	"[Fax] [char] (16) NULL , " &_
	"[nIssPercnt] [float] NULL , " &_
	"[DIssComp] [datetime] NULL , " &_
	"[CAriaAct] [char] (60) NULL , " &_
	"[CTrackRef] [char] (7) NULL , " &_
	"[cCont_Code] [char] (6) NULL , " &_
	"[mPriReson] [varchar] (500) NULL " &_
	") ON [PRIMARY]")
end if
'Response.End 
'WAL_5/5/2004_create SQL DB if not exist[start]	


'intial values for security model 'wma
'SecurityGroups table
conn.Execute ("Use CRM")
conn.Execute ("Delete SecurityTokens")
conn.Execute ("Delete SecurityGroups")

conn.Execute ("INSERT INTO SecurityGroups (cGroupID, cDescription) VALUES ('CRMADMIN', 'Customer Relationship Mgmt.') ")
conn.Execute ("INSERT INTO SecurityGroups (cGroupID, cDescription) VALUES ('CRMCUST', 'Customer Relationship Mgmt.') ")
conn.Execute ("INSERT INTO SecurityGroups (cGroupID, cDescription) VALUES ('CRMSALES', 'Customer Relationship Mgmt.') ")  	  
conn.Execute ("INSERT INTO SecurityGroups (cGroupID, cDescription, cParentGroup) VALUES ('AHELPDESK', 'Helpdesk', 'CRMADMIN') ")
conn.Execute ("INSERT INTO SecurityGroups (cGroupID, cDescription, cParentGroup) VALUES ('ALAYOUT', 'Layout & Menu', 'CRMADMIN') ")
conn.Execute ("INSERT INTO SecurityGroups (cGroupID, cDescription, cParentGroup) VALUES ('ALOG', 'Customer Log', 'CRMADMIN') ")
conn.Execute ("INSERT INTO SecurityGroups (cGroupID, cDescription, cParentGroup) VALUES ('AMENU', 'Menu Items', 'CRMADMIN') ")
conn.Execute ("INSERT INTO SecurityGroups (cGroupID, cDescription, cParentGroup) VALUES ('ASETUP', 'CRM Setup', 'CRMADMIN') ")
conn.Execute ("INSERT INTO SecurityGroups (cGroupID, cDescription, cParentGroup) VALUES ('AUPLOAF', 'Style Images', 'CRMADMIN') ")
conn.Execute ("INSERT INTO SecurityGroups (cGroupID, cDescription, cParentGroup) VALUES ('UPLOAD', 'Upload Documents', 'CRMADMIN') ")
conn.Execute ("INSERT INTO SecurityGroups (cGroupID, cDescription, cParentGroup) VALUES ('CCATALOG', 'Catalog', 'CRMCUST') ")
conn.Execute ("INSERT INTO SecurityGroups (cGroupID, cDescription, cParentGroup) VALUES ('STORE', 'Customer Stores', 'CRMCUST') ")
conn.Execute ("INSERT INTO SecurityGroups (cGroupID, cDescription, cParentGroup) VALUES ('CCUSTOMER', 'Customer Profile', 'CRMCUST') ")
conn.Execute ("INSERT INTO SecurityGroups (cGroupID, cDescription, cParentGroup) VALUES ('CHELPDESK', 'Helpdesk', 'CRMCUST') ")
conn.Execute ("INSERT INTO SecurityGroups (cGroupID, cDescription, cParentGroup) VALUES ('CINVOICE', 'Invoice', 'CRMCUST') ")
conn.Execute ("INSERT INTO SecurityGroups (cGroupID, cDescription, cParentGroup) VALUES ('CORDER', 'Order', 'CRMCUST') ")
conn.Execute ("INSERT INTO SecurityGroups (cGroupID, cDescription, cParentGroup) VALUES ('COTS', 'Check OTS', 'CRMCUST') ")
conn.Execute ("INSERT INTO SecurityGroups (cGroupID, cDescription, cParentGroup) VALUES ('CRETURN', 'Return Authorization', 'CRMCUST') ")
conn.Execute ("INSERT INTO SecurityGroups (cGroupID, cDescription, cParentGroup) VALUES ('SCATALOG', 'Catalog', 'CRMSALES') ")
conn.Execute ("INSERT INTO SecurityGroups (cGroupID, cDescription, cParentGroup) VALUES ('SCUSTOMER', 'Customer Profile', 'CRMSALES') ")
conn.Execute ("INSERT INTO SecurityGroups (cGroupID, cDescription, cParentGroup) VALUES ('SINVOICE', 'Invoice', 'CRMSALES') ")
conn.Execute ("INSERT INTO SecurityGroups (cGroupID, cDescription, cParentGroup) VALUES ('SORDER', 'Order', 'CRMSALES') ")													
conn.Execute ("INSERT INTO SecurityGroups (cGroupID, cDescription, cParentGroup) VALUES ('SOTS', 'Check OTS', 'CRMSALES') ")
conn.Execute ("INSERT INTO SecurityGroups (cGroupID, cDescription, cParentGroup) VALUES ('SRETURN', 'Return Authorization', 'CRMSALES') ")
	
'SecurityTokens table
conn.Execute ("INSERT INTO SecurityTokens (cTokenID, cDescription, cGroupID) VALUES ('AHELPDESK', 'Manage Helpdesk', 'AHELPDESK') ")
conn.Execute ("INSERT INTO SecurityTokens (cTokenID, cDescription, cGroupID) VALUES ('ALAYOUT', 'CRM Layout & Menu', 'ALAYOUT') ")
conn.Execute ("INSERT INTO SecurityTokens (cTokenID, cDescription, cGroupID) VALUES ('ALOG', 'View Customer Log', 'ALOG') ")
conn.Execute ("INSERT INTO SecurityTokens (cTokenID, cDescription, cGroupID) VALUES ('AMENU', 'CRM Menu Items', 'AMENU') ")
conn.Execute ("INSERT INTO SecurityTokens (cTokenID, cDescription, cGroupID) VALUES ('ASECURITY', 'Users and Security', 'ASECURITY') ")
conn.Execute ("INSERT INTO SecurityTokens (cTokenID, cDescription, cGroupID) VALUES ('ASETUP', 'CRM Setup', 'ASETUP') ")
conn.Execute ("INSERT INTO SecurityTokens (cTokenID, cDescription, cGroupID) VALUES ('AUPLOAF', 'Upload Style Images', 'AUPLOAF') ")
	
conn.Execute ("INSERT INTO SecurityTokens (cTokenID, cDescription, cGroupID) VALUES ('CCATALOG', 'Catalog', 'CCATALOG') ")													
conn.Execute ("INSERT INTO SecurityTokens (cTokenID, cDescription, cGroupID) VALUES ('CDELCONT', 'Delete Contact', 'CCUSTOMER') ")
conn.Execute ("INSERT INTO SecurityTokens (cTokenID, cDescription, cGroupID) VALUES ('CEDITCONT', 'Edit Contact', 'CCUSTOMER') ")
conn.Execute ("INSERT INTO SecurityTokens (cTokenID, cDescription, cGroupID) VALUES ('CEDITCUST', 'Edit Customer Profile', 'CCUSTOMER') ")
conn.Execute ("INSERT INTO SecurityTokens (cTokenID, cDescription, cGroupID) VALUES ('CEDITSTORE', 'Edit Store', 'CCUSTOMER') ")																																						
conn.Execute ("INSERT INTO SecurityTokens (cTokenID, cDescription, cGroupID) VALUES ('CADDCONT', 'Add new Contact', 'CCUSTOMER') ")
conn.Execute ("INSERT INTO SecurityTokens (cTokenID, cDescription, cGroupID) VALUES ('CVIEWCUST', 'View Customer Profile', 'CCUSTOMER') ")													
conn.Execute ("INSERT INTO SecurityTokens (cTokenID, cDescription, cGroupID) VALUES ('CADDSTORE', 'Add Store', 'CCUSTOMER') ")
conn.Execute ("INSERT INTO SecurityTokens (cTokenID, cDescription, cGroupID) VALUES ('CHLPDESK', 'Helpdesk', 'CHELPDESK') ")
conn.Execute ("INSERT INTO SecurityTokens (cTokenID, cDescription, cGroupID) VALUES ('CCHECKINV', 'Check Invoice Status', 'CINVOICE') ")
conn.Execute ("INSERT INTO SecurityTokens (cTokenID, cDescription, cGroupID) VALUES ('CVIEWSTMNT', 'View Statement', 'CINVOICE') ")					
conn.Execute ("INSERT INTO SecurityTokens (cTokenID, cDescription, cGroupID) VALUES ('CCHKORD', 'Check Order Status', 'CORDER') ")
conn.Execute ("INSERT INTO SecurityTokens (cTokenID, cDescription, cGroupID) VALUES ('CORDCONF', 'Order Confirmation', 'CORDER') ")													
conn.Execute ("INSERT INTO SecurityTokens (cTokenID, cDescription, cGroupID) VALUES ('CADDORD', 'Remote Order', 'CORDER') ")
conn.Execute ("INSERT INTO SecurityTokens (cTokenID, cDescription, cGroupID) VALUES ('CCHECKOTS', 'Check OTS', 'COTS') ")
conn.Execute ("INSERT INTO SecurityTokens (cTokenID, cDescription, cGroupID) VALUES ('CCHECKRA', 'Check RA Status', 'CRETURN') ")
conn.Execute ("INSERT INTO SecurityTokens (cTokenID, cDescription, cGroupID) VALUES ('CADDRA', 'Add New RA', 'CRETURN') ")
	
conn.Execute ("INSERT INTO SecurityTokens (cTokenID, cDescription, cGroupID) VALUES ('SCATALOG', 'Catalog', 'SCATALOG') ")
conn.Execute ("INSERT INTO SecurityTokens (cTokenID, cDescription, cGroupID) VALUES ('SADDCONT', 'Add new Contact', 'SCUSTOMER') ")													
conn.Execute ("INSERT INTO SecurityTokens (cTokenID, cDescription, cGroupID) VALUES ('SADDCUST', 'Add Customer Profile', 'SCUSTOMER') ")
conn.Execute ("INSERT INTO SecurityTokens (cTokenID, cDescription, cGroupID) VALUES ('SADDSTORE', 'Add Store', 'SCUSTOMER') ")
conn.Execute ("INSERT INTO SecurityTokens (cTokenID, cDescription, cGroupID) VALUES ('SDELCONT', 'Delete Contact', 'SCUSTOMER') ")
conn.Execute ("INSERT INTO SecurityTokens (cTokenID, cDescription, cGroupID) VALUES ('SEDITCONT', 'Edit Contact', 'SCUSTOMER') ")
conn.Execute ("INSERT INTO SecurityTokens (cTokenID, cDescription, cGroupID) VALUES ('SEDITCUST', 'Edit Customer Profile', 'SCUSTOMER') ")
conn.Execute ("INSERT INTO SecurityTokens (cTokenID, cDescription, cGroupID) VALUES ('SEDITSTORE', 'Edit Store', 'SCUSTOMER') ")													
conn.Execute ("INSERT INTO SecurityTokens (cTokenID, cDescription, cGroupID) VALUES ('SHISTACT', 'History Activities', 'SCUSTOMER') ")
conn.Execute ("INSERT INTO SecurityTokens (cTokenID, cDescription, cGroupID) VALUES ('SPENDACT', 'Pending Activities', 'SCUSTOMER') ")
conn.Execute ("INSERT INTO SecurityTokens (cTokenID, cDescription, cGroupID) VALUES ('SVIEWCUST', 'View Customer Profile', 'SCUSTOMER') ")
conn.Execute ("INSERT INTO SecurityTokens (cTokenID, cDescription, cGroupID) VALUES ('SVIEWSTMNT', 'View Statement', 'SINVOICE') ")																																						
conn.Execute ("INSERT INTO SecurityTokens (cTokenID, cDescription, cGroupID) VALUES ('SCREDITCOL', 'Credit Collection Report', 'SINVOICE') ")
conn.Execute ("INSERT INTO SecurityTokens (cTokenID, cDescription, cGroupID) VALUES ('SINVPAYMNT', 'Invoice Payment', 'SINVOICE') ")													
conn.Execute ("INSERT INTO SecurityTokens (cTokenID, cDescription, cGroupID) VALUES ('SCHECKINV', 'Check Invoice Status', 'SINVOICE') ")
conn.Execute ("INSERT INTO SecurityTokens (cTokenID, cDescription, cGroupID) VALUES ('SADDORD', 'Remote Order', 'SORDER') ")
conn.Execute ("INSERT INTO SecurityTokens (cTokenID, cDescription, cGroupID) VALUES ('SEDITORD', 'Modify Order', 'SORDER') ")
conn.Execute ("INSERT INTO SecurityTokens (cTokenID, cDescription, cGroupID) VALUES ('SCHKORD', 'Check Order Status', 'SORDER') ")
conn.Execute ("INSERT INTO SecurityTokens (cTokenID, cDescription, cGroupID) VALUES ('SORDCONF', 'Order Confirmation', 'SORDER') ")
conn.Execute ("INSERT INTO SecurityTokens (cTokenID, cDescription, cGroupID) VALUES ('SCHECKOTS', 'Check OTS', 'SOTS') ")
conn.Execute ("INSERT INTO SecurityTokens (cTokenID, cDescription, cGroupID) VALUES ('SCHECKRA', 'Check RA Status', 'SRETURN') ")
conn.Execute ("INSERT INTO SecurityTokens (cTokenID, cDescription, cGroupID) VALUES ('SADDRA', 'Add New RA', 'SRETURN') ")

	
'wma save only in second page	
'objTextFile.WriteLine(strLine)
'objTextFile.Close
'Set objFileSystem = Nothing
'strAppPath = Trim(Request.ServerVariables("APPL_PHYSICAL_PATH"))




'Dim objFile
'Set objFile = Server.CreateObject("Scripting.FileSystemObject")

'If Right(strAppPath,1) = "\" Then
'	strFilePath = "admin\crmsetup\setup\setup.txt"
'Else
'	strFilePath = "\admin\crmsetup\setup\setup.txt"
'End If
'Set objTxtFile = objFile.OpenTextFile(strAppPath & strFilePath,1)
'Dim strLine
'strFile = objTxtFile.ReadAll
'Dim strArSetups
'strArSetups = Split(strFile," AND ", -1 , 1)
''Declare Vartiables To Hold the temporary key and values
'Dim strKey
'Dim strValue
'For intLoop = 0 To UBound(strArSetups)
'	strArKeyValue = Split(strArSetups(intLoop) , "=" , -1 , 1)
'	Session(strArKeyValue(0)) = strArKeyValue(1)
'Next
'ARD - Enhance CRM Setup [End]

Response.Redirect("crmSetup1.asp")
'Application("CompanyID")

'Application("DataPath")

'Application("SystemPath")

'Application("UserCanEdit")

'Application("NotificationAddress")
%>

