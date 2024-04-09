PROCEDURE MAIN
PARAMETERS lcDir,lcWorkDir,lcSysDir,llShared


*: lcDir : is the path of the Sqldictionary folders
*:lcWorkDir : is the path where files will be updated 
   DO FORM frmgnexe.scx  WITH lcDir,lcWorkDir,lcSysDir,llShared
   READ EVENTS
RETURN
