USE [master]
GO
IF NOT EXISTS ( SELECT name FROM master.sys.databases WHERE name = N'{0}')
begin
	EXEC sp_attach_db @dbname = N'{0}', @filename1 = N'{1}',@filename2 = N'{2}'
end
