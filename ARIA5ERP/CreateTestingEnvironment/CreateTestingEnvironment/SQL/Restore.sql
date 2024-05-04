
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



Use %Database%

Declare @OldPrimaryFile		nvarchar(1000)
Declare @OldLogFile		nvarchar(1000)

Select @OldPrimaryFile = Name
	From sysFiles
	Where FileId = 1

Select @OldLogFile = Name
	From sysFiles
	Where FileId = 2

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
RESTORE FILELISTONLY 
   FROM DISK = ''' + @BackUpFileName + ''''


Execute(@RestoreCommand)