if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[CustClassification]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
drop table [dbo].[CustClassification]
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[CustGroup]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
drop table [dbo].[CustGroup]
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Email]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
drop table [dbo].[Email]
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[SuIssDt]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
drop table [dbo].[SuIssDt]
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[SuIssHdr]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
drop table [dbo].[SuIssHdr]
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Trans]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
drop table [dbo].[Trans]
GO

CREATE TABLE [dbo].[CustClassification] (
	[CustID] [char] (5) NOT NULL ,
	[CustGroup] [int] NOT NULL ,
	[DispInSearch] [bit] NULL 
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[CustGroup] (
	[GroupID] [int] IDENTITY (1, 1) NOT FOR REPLICATION  NOT NULL ,
	[Description] [varchar] (60) NULL ,
	[PackID] [char] (21) NULL 
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[Email] (
	[email] [varchar] (50) NULL ,
	[cissueno] [varchar] (50) NULL 
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[SuIssDt] (
	[CIssueNo] [char] (6) NOT NULL ,
	[cRespType] [char] (1) NULL ,
	[cRespBy] [char] (60) NULL ,
	[mRespAct] [varchar] (5000) NULL ,
	[DRespDate] [datetime] NULL ,
	[tRespTime] [datetime] NULL ,
	[intLineNo] [int] NOT NULL 
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[SuIssHdr] (
	[CIssueNo] [char] (6) NOT NULL ,
	[CCust_Id] [char] (5) NULL ,
	[DIssStart] [datetime] NULL ,
	[CIssType] [char] (1) NULL ,
	[Cbug_app] [char] (3) NULL ,
	[CMod_Id] [char] (10) NULL ,
	[CissStat] [char] (1) NULL ,
	[CIssPrior] [char] (1) NULL ,
	[CissSubjct] [char] (60) NULL ,
	[mIssDetail] [varchar] (5000) NULL ,
	[Contact] [char] (30) NULL ,
	[Cwe_mail] [char] (60) NULL ,
	[Phone] [char] (16) NULL ,
	[CExt] [char] (6) NULL ,
	[Fax] [char] (16) NULL ,
	[nIssPercnt] [float] NULL ,
	[DIssComp] [datetime] NULL ,
	[CAriaAct] [char] (60) NULL ,
	[CTrackRef] [char] (7) NULL ,
	[cCont_Code] [char] (2) NULL ,
	[mPriReson] [varchar] (500) NULL 
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[Trans] (
	[Contact] [varchar] (30) NULL ,
	[Account] [varchar] (5) NOT NULL ,
	[CADD_Time] [varchar] (11) NULL ,
	[CADD_Date] [datetime] NULL ,
	[TransType] [varchar] (30) NULL ,
	[TransNumber] [varchar] (30) NULL ,
	[Memo] [text] NULL ,
	[Notify_mail] [varchar] (30) NULL ,
	[Confirm_mail] [varchar] (30) NULL 
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO

