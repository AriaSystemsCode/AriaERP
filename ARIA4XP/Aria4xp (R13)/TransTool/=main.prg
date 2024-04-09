*!**************************************************************************
*! Name        : Main.prg
*! Description : Main Program to Start the Translation Tool
*! System      : Aria4XP
****************************************************************************
PUBLIC gcCurPath, gnConnHandler, AriaSysPath, Aria4SysPath, AriaAppPath

lcCurrentProcedure = UPPER(SYS(16,1))
lnPathStart        = AT(":",lcCurrentProcedure)- 1
lnLenOfPath        = RAT("\", lcCurrentProcedure) - (lnPathStart)
gcCurPath = SUBSTR(lcCurrentProcedure, lnPathStart, lnLenofPath)

gnConnHandler = 0
AriaSysPath = ""
AriaAppPath = ""

SET SAFETY OFF
SET DEFAULT TO (gcCurPath)
SET DELETED ON
SET CENTURY ON
SET DATE DMY
SET HOURS TO 12
SET MULTILOCKS ON
SET REPROCESS TO 0.1 SECONDS
SET STATUS OFF
SET NULLDISPLAY TO ''

frmPath = gcCurPath+'\forms'
icoPath = gcCurPath+'\icons'
tmpPath = gcCurPath+'\temp'


CLOSE DATABASES
IF ! gfOpenConn()
  RETURN
ENDIF  

USE (AriaSysPath +"SYCINST") IN 0 SHARED 
Aria4SysPath = ALLTRIM(sycinst.ca4sysdir)
USE IN sycinst

*DO FORM frm_login.scx
DO FORM (frmPath+"\admintool.scx")

READ EVENTS 
RETURN


*********************************
FUNCTION gfOpenConn

WAIT WINDOW "Connecting..." NOWAIT
XMLTOCURSOR('Config.xml','Setting',512)
SELECT Setting
lcConnStr="DRIVER=SQL Server;SERVER="+ALLTRIM(Server)+";UID="+ALLTRIM(Uid)+";PWD="+ALLTRIM(Pwd)+";DATABASE="+ALLTRIM(Database)+";"
AriaSysPath = ADDBS(Setting.Syspath)
AriaAppPath = ADDBS(Setting.Ariapath)
USE

gnConnHandler = SQLSTRINGCONNECT(lcConnStr)

WAIT clear
IF gnConnHandler  < 1
  MESSAGEBOX("Unable to Connect to Database!",16,"Translation Tool")
  RETURN .F.
ELSE
  RETURN .T.
ENDIF
*********************************
