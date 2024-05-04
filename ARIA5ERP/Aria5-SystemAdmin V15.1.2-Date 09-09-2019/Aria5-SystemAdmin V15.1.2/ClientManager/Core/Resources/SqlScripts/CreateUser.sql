DECLARE @SqlStatement nvarchar(4000)
Declare @loginName varchar (100)
Declare @Password varchar (100)
Select @loginName = '{0}'
Select @Password = '{1}'
If not Exists (select loginname from master.dbo.syslogins where name = @loginName)
Begin 
	Set @SqlStatement = 'CREATE LOGIN ['+ @loginName + '] WITH PASSWORD=N''' + @Password + ''',DEFAULT_DATABASE=[master],DEFAULT_LANGUAGE=[us_english], CHECK_EXPIRATION=OFF, CHECK_POLICY=OFF'
	EXEC sp_executesql @SqlStatement
	Set @SqlStatement = 'EXEC sys.sp_addsrvrolemember @loginame = N''' + @loginName + ''', @rolename = N''bulkadmin'''
	EXEC sp_executesql @SqlStatement
	Set @SqlStatement = 'EXEC sys.sp_addsrvrolemember @loginame = N''' + @loginName + ''', @rolename = N''dbcreator'''
	EXEC sp_executesql @SqlStatement
	Set @SqlStatement = 'ALTER LOGIN [' + @loginName + '] enable'
	EXEC sp_executesql @SqlStatement
End