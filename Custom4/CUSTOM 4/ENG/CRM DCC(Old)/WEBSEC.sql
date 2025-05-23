if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[FK_SecurityTokens_SecurityGroups]') and OBJECTPROPERTY(id, N'IsForeignKey') = 1)
ALTER TABLE [dbo].[SecurityTokens] DROP CONSTRAINT FK_SecurityTokens_SecurityGroups
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[FK_Privileges_SecurityTokens]') and OBJECTPROPERTY(id, N'IsForeignKey') = 1)
ALTER TABLE [dbo].[Privileges] DROP CONSTRAINT FK_Privileges_SecurityTokens
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[FK_Privileges_syuuser]') and OBJECTPROPERTY(id, N'IsForeignKey') = 1)
ALTER TABLE [dbo].[Privileges] DROP CONSTRAINT FK_Privileges_syuuser
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Privileges]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
drop table [dbo].[Privileges]
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[SecurityGroups]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
drop table [dbo].[SecurityGroups]
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[SecurityTokens]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
drop table [dbo].[SecurityTokens]
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[syuuser]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
drop table [dbo].[syuuser]
GO

CREATE TABLE [dbo].[Privileges] (
	[cUser_ID] [char] (10) NOT NULL ,
	[cTokenID] [varchar] (10) NOT NULL 
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[SecurityGroups] (
	[cGroupID] [varchar] (10) NOT NULL ,
	[cDescription] [varchar] (30) NULL ,
	[cParentGroup] [varchar] (10) NULL 
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[SecurityTokens] (
	[cTokenID] [varchar] (10) NOT NULL ,
	[cDescription] [varchar] (30) NULL ,
	[cGroupID] [varchar] (10) NULL 
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[syuuser] (
	[cuser_id] [char] (10) NOT NULL ,
	[cusr_name] [char] (35) NOT NULL ,
	[cusr_pass] [char] (8) NOT NULL ,
	[cwe_mail] [char] (200) NULL ,
	[lusr_chgp] [bit] NULL ,
	[cusr_loca] [char] (30) NULL ,
	[cusr_phon] [char] (17) NULL ,
	[cusr_levl] [char] (1) NULL ,
	[cusr_cloc] [char] (4) NULL ,
	[cusr_audt] [char] (1) NULL ,
	[cusr_kbuf] [char] (3) NULL ,
	[lusr_msgr] [bit] NULL ,
	[cusr_dclr] [char] (10) NULL ,
	[cusr_dcom] [char] (2) NULL ,
	[cusr_dprt] [char] (20) NULL ,
	[cusr_dwks] [char] (6) NULL ,
	[cusr_dmdl] [char] (2) NULL ,
	[cdef_bmp] [char] (12) NULL ,
	[dusr_begn] [datetime] NULL ,
	[dusr_end] [datetime] NULL ,
	[cusr_begt] [char] (8) NULL ,
	[cusr_endt] [char] (8) NULL ,
	[lusr_logd] [bit] NULL ,
	[dusr_lld] [smallint] NULL ,
	[cusr_llt] [char] (11) NULL ,
	[cusr_llot] [char] (11) NULL ,
	[cusr_grup] [char] (4) NULL ,
	[lusr_1st] [bit] NULL ,
	[lusr_ustb] [bit] NULL ,
	[cusr_resr] [char] (8) NULL ,
	[lusr_dtask] [bit] NULL ,
	[cextprg] [char] (10) NULL ,
	[cdivision] [char] (6) NULL ,
	[cadd_user] [char] (10) NULL ,
	[dadd_date] [datetime] NULL ,
	[cadd_time] [char] (11) NULL ,
	[llok_stat] [bit] NULL ,
	[clok_user] [char] (10) NULL ,
	[dlok_date] [smalldatetime] NULL ,
	[clok_time] [char] (8) NULL ,
	[cowner] [char] (16) NULL 
) ON [PRIMARY]
GO

ALTER TABLE [dbo].[Privileges] WITH NOCHECK ADD 
	CONSTRAINT [PK_Privileges] PRIMARY KEY  CLUSTERED 
	(
		[cUser_ID],
		[cTokenID]
	)  ON [PRIMARY] 
GO

ALTER TABLE [dbo].[SecurityGroups] WITH NOCHECK ADD 
	CONSTRAINT [PK_SecurityGroup] PRIMARY KEY  CLUSTERED 
	(
		[cGroupID]
	)  ON [PRIMARY] 
GO

ALTER TABLE [dbo].[SecurityTokens] WITH NOCHECK ADD 
	CONSTRAINT [PK_SecurityTokens] PRIMARY KEY  CLUSTERED 
	(
		[cTokenID]
	)  ON [PRIMARY] 
GO

ALTER TABLE [dbo].[syuuser] WITH NOCHECK ADD 
	CONSTRAINT [PK_syuuser] PRIMARY KEY  CLUSTERED 
	(
		[cuser_id]
	)  ON [PRIMARY] 
GO

ALTER TABLE [dbo].[Privileges] ADD 
	CONSTRAINT [FK_Privileges_SecurityTokens] FOREIGN KEY 
	(
		[cTokenID]
	) REFERENCES [dbo].[SecurityTokens] (
		[cTokenID]
	) NOT FOR REPLICATION ,
	CONSTRAINT [FK_Privileges_syuuser] FOREIGN KEY 
	(
		[cUser_ID]
	) REFERENCES [dbo].[syuuser] (
		[cuser_id]
	) NOT FOR REPLICATION 
GO

alter table [dbo].[Privileges] nocheck constraint [FK_Privileges_SecurityTokens]
GO

alter table [dbo].[Privileges] nocheck constraint [FK_Privileges_syuuser]
GO

ALTER TABLE [dbo].[SecurityTokens] ADD 
	CONSTRAINT [FK_SecurityTokens_SecurityGroups] FOREIGN KEY 
	(
		[cGroupID]
	) REFERENCES [dbo].[SecurityGroups] (
		[cGroupID]
	) NOT FOR REPLICATION 
GO

alter table [dbo].[SecurityTokens] nocheck constraint [FK_SecurityTokens_SecurityGroups]
GO

