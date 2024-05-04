Declare @DBName varchar (100)
Declare @DBPath nvarchar (4000)
Declare @DBUser nvarchar (100)
DECLARE @SqlStatement nvarchar(4000)
Select @DBName = '{0}'
Select @DBPath = '{1}'
Select @DBUser = '{2}'

IF NOT EXISTS (SELECT name FROM sys.databases WHERE name = @DBName)
BEGIN

	Set @SqlStatement = 'CREATE DATABASE [' + @DBName + '] ON  PRIMARY ( NAME = ' + @DBName + ' , FILENAME = ''' + @DBPath + '.mdf'' )
	LOG ON ( NAME = ''' + @DBName + '_log'', FILENAME = ''' + @DBPath + '_log.ldf'' )'
	EXEC sp_executesql @SqlStatement

	EXEC dbo.sp_dbcmptlevel @dbname= @DBName , @new_cmptlevel=90

	IF (1 = FULLTEXTSERVICEPROPERTY('IsFullTextInstalled'))
	begin
	EXEC [{0}].[dbo].[sp_fulltext_database] @action = 'enable'
	end

	Set @SqlStatement = 'ALTER DATABASE ['+@DBName+'] SET ANSI_NULL_DEFAULT OFF 

	ALTER DATABASE ['+@DBName +'] SET ANSI_NULLS OFF 

	ALTER DATABASE ['+@DBName+'] SET ANSI_PADDING OFF 

	ALTER DATABASE ['+@DBName+'] SET ANSI_WARNINGS OFF 

	ALTER DATABASE ['+@DBName+'] SET ARITHABORT OFF 

	ALTER DATABASE ['+@DBName+'] SET AUTO_CLOSE ON 

	ALTER DATABASE ['+@DBName+'] SET AUTO_CREATE_STATISTICS ON 

	ALTER DATABASE ['+@DBName+'] SET AUTO_SHRINK ON 

	ALTER DATABASE ['+@DBName+'] SET AUTO_UPDATE_STATISTICS ON 

	ALTER DATABASE ['+@DBName+'] SET CURSOR_CLOSE_ON_COMMIT OFF 

	ALTER DATABASE ['+@DBName+'] SET CURSOR_DEFAULT  GLOBAL 

	ALTER DATABASE ['+@DBName+'] SET CONCAT_NULL_YIELDS_NULL OFF 

	ALTER DATABASE ['+@DBName+'] SET NUMERIC_ROUNDABORT OFF 

	ALTER DATABASE ['+@DBName+'] SET QUOTED_IDENTIFIER OFF 

	ALTER DATABASE ['+@DBName+'] SET RECURSIVE_TRIGGERS OFF 

	ALTER DATABASE ['+@DBName+'] SET  DISABLE_BROKER 

	ALTER DATABASE ['+@DBName+'] SET AUTO_UPDATE_STATISTICS_ASYNC OFF 

	ALTER DATABASE ['+@DBName+'] SET DATE_CORRELATION_OPTIMIZATION OFF 

	ALTER DATABASE ['+@DBName+'] SET TRUSTWORTHY OFF 

	ALTER DATABASE ['+@DBName+'] SET ALLOW_SNAPSHOT_ISOLATION OFF 

	ALTER DATABASE ['+@DBName+'] SET PARAMETERIZATION SIMPLE 

	ALTER DATABASE ['+@DBName+'] SET  READ_WRITE 

	ALTER DATABASE ['+@DBName+'] SET RECOVERY FULL 

	ALTER DATABASE ['+@DBName+'] SET  MULTI_USER 

	ALTER DATABASE ['+@DBName+'] SET PAGE_VERIFY CHECKSUM  

	ALTER DATABASE ['+@DBName+'] SET DB_CHAINING OFF'

	EXEC sp_executesql @SqlStatement
	set @SqlStatement ='USE [' + @DBName + ']

	IF NOT EXISTS (SELECT * FROM sys.database_principals WHERE name = N''' + @DBUser + ''')
	CREATE USER [' + @DBUser + '] FOR LOGIN [' + @DBUser + '] WITH DEFAULT_SCHEMA=[dbo]

	IF NOT EXISTS (SELECT * FROM dbo.sysusers WHERE name = N''' + @DBUser + ''')
	EXEC dbo.sp_grantdbaccess @loginame =  ' + @DBUser + ' , @name_in_db =  ' + @DBUser +'
	
	USE [' + @DBName + ']
	EXEC sp_addrolemember N''db_owner'','+@DBUser
	
	EXEC sp_executesql @SqlStatement

END