Use %Database%

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
		@Path 				= '%DatabasePath%'		  ,
		@BackUpFileName		= '%BackUpPath%'

Select @PrimaryFileName = filename
	From sysFiles
	Where fileid = 1

Select @LogFileName = filename
	From sysFiles
	Where fileid = 2

Use Master

Declare @Id		Int
Declare @cmd	Varchar(1000)

Declare c Cursor For
	Select spid
		From master..sysprocesses
		Where hostname  <> ''

Open c


Fetch Next From c Into @Id

While(@@FETCH_STATUS<>-1)
Begin
	If(@Id <> @@SPID)
	Begin
		Select @cmd = 'Kill ' + Cast(@Id as Varchar)
		Execute(@cmd)
	End
	Fetch Next From c Into @Id
End

Close c
Deallocate c

Declare @RestoreCommand nVarchar(4000)

Select @RestoreCommand ='
RESTORE DATABASE ' + @DatabaseName + '
   FROM DISK = ''' + @BackUpFileName + '''
   WITH 
      MOVE ''%OldData%'' TO ''' + @PrimaryFileName + ''' ,
      MOVE ''%OldLog%''  TO ''' + @LogFileName + ''', REPLACE'


Execute(@RestoreCommand)