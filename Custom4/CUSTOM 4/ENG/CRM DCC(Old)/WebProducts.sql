if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[CatgClassSubClass]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
drop table [dbo].[CatgClassSubClass]
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[CustClassification]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
drop table [dbo].[CustClassification]
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[CustGroup]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
drop table [dbo].[CustGroup]
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[WebCatg]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
drop table [dbo].[WebCatg]
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[WebProducts]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
drop table [dbo].[WebProducts]
GO

CREATE TABLE [dbo].[CatgClassSubClass] (
	[Id] [varchar] (255) NULL ,
	[Descr] [varchar] (255) NULL ,
	[Parent] [varchar] (255) NULL ,
	[ImdDir] [varchar] (255) NULL ,
	[NoOfImgs] [varchar] (255) NULL 
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[CustClassification] (
	[CustID] [char] (5) NOT NULL ,
	[CustGroup] [int] NOT NULL ,
	[DispInSearch] [bit] NULL 
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[CustGroup] (
	[GroupID] [int] NOT NULL ,
	[Description] [varchar] (60) NULL ,
	[PackID] [char] (21) NOT NULL 
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[WebCatg] (
	[ID] [varchar] (50) NOT NULL ,
	[DESCRIPTION] [varchar] (60) NOT NULL ,
	[PARENT] [varchar] (50) NOT NULL ,
	[ImgDir] [varchar] (100) NULL ,
	[NoOfImgs] [int] NULL 
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[WebProducts] (
	[PARENT] [varchar] (50) NOT NULL ,
	[STYLE] [char] (19) NOT NULL ,
	[DESCRIPTION] [char] (60) NULL 
) ON [PRIMARY]
GO

