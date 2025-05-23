/****** Object:  Table [dbo].[AMAZON_TRANSMISSION_LOG_T]    Script Date: 08/18/2010 17:10:30 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [dbo].[AMAZON_TRANSMISSION_LOG_T](
	[TRANSMISSION] [int] IDENTITY(1,1) NOT NULL,
	[Downloaded] [int] NULL,
	[DATE] [datetime] NULL,
	[STATUS] [char](20) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[InputXML] [varchar](max) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[HasNext] [bit] NULL,
	[NextToken] [char](1000) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Progress] [varchar](max) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Error] [varchar](max) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[TRANSMISSION_KEY] [uniqueidentifier] NULL CONSTRAINT [DF_AMAZON_TRANSMISSION_LOG_T_TRANSMISSION_KEY]  DEFAULT (newid())
) ON [PRIMARY]

GO
SET ANSI_PADDING OFF