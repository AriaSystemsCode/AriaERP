Use Master

Declare @DatabaseName		sysName
Declare @PrimaryName		sysName
Declare @LogName		sysName
Declare @Path			sysName
Declare @PrimaryFileName	sysName
Declare @LogFileName		sysName
Declare @BackUpFileName		nvarchar(4000)

Select 	@DatabaseName 		= '%Database%' ,
		@PrimaryName		= @DatabaseName + '_Data' ,
		@LogName			= @DatabaseName + '_Log'  ,
		@Path 				= '%DatabasePath%'			  ,
		@PrimaryFileName	= @Path + @PrimaryName + '.mdf' ,
		@LogFileName		= @Path + @LogName + '.ldf' ,
		@BackUpFileName		= '%BackUpPath%'


If Exists	(Select *
			From sysDatabases
			Where name = @DatabaseName
		)

		Execute ('Drop Database ' + @DatabaseName)



Execute('
CREATE DATABASE ' + @DatabaseName + '
	ON 
	( NAME =  ' + @PrimaryName + ',
	   FILENAME = ''' + @PrimaryFileName + ''',
	   SIZE = 10,
	   MAXSIZE = 50,
	   FILEGROWTH = 5 )
	LOG ON
	( NAME = ' + @LogName + ',
	   FILENAME = ''' + @LogFileName + ''',
	   SIZE = 5MB,
	   MAXSIZE = 25MB,
	   FILEGROWTH = 5MB )
')
