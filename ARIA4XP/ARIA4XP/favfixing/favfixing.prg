*Modifications
*E303059,1 TMI 02/09/2012 Add a check that if there is more than one line in cilents table then do not run [Media] 
*********************************************************************************************************


*B609769,1 MAB 11/28/2011 Use the new unique key ............... Begin
PARAMETERS lcSysFiles
*_screen.Visible = .F. 
*_vfp.Visible = .F.
PRIVATE lcResourcesFolder, oXML

IF (VARTYPE(lcSysFiles) != "C") OR EMPTY(lcSysFiles)
  *E303059,1 TMI 02/09/2012 [Start] By default I'll assume that this program will run from under the root folder where aria27 is installed 
  *                                 so no need to show up a dialouge box to get the sysfiles folder, it would be SYSFILES 
  *lcSysFiles = GETDIR(SET("Default"),"Select Aria27 System Files")
  *MT
  *lcSysFiles = ADDBS(JUSTPATH(SYS(16)))+'SYSFILES\'
  lcSysFiles = ADDBS(FULLPATH(''))+'SYSFILES\'
  *MT
  *E303059,1 TMI 02/09/2012 [End  ] 
ENDIF

*E303059,1 TMI 02/09/2012 [Start] if there is more than one client then do not run 
IF FILE('clients.DBF')
  SELECT 0
  use clients
  lcDel = SET("Deleted")
  SET DELETED ON
  LOCATE
  COUNT TO lnCnt
  IF lnCnt>1
    RETURN
  ENDIF
  GO TOP 
  lcSysFiles = ADDBS(ALLTRIM(clients.CDATAPATH))+'SYSFILES\'
  SET DELETED &lcDel
ENDIF     
*E303059,1 TMI 02/09/2012 [End  ] 


SET SAFETY OFF 

IF EMPTY(lcSysFiles)
*  WAIT WINDOW "Invalid Folder, Can't apply fix." TIMEOUT 5
  WAIT WINDOW "Empty Sysfiles Folder, Can't apply fix." TIMEOUT 5
  RETURN .F.
ENDIF

lcSysFiles = ADDBS(lcSysFiles)
IF !FILE(lcSysFiles+"sycinst.dbf")
  *MT
  *WAIT WINDOW "Invalid Folder, Can't apply fix." TIMEOUT 5
  WAIT WINDOW "Invalid Sysfiles Folder, Can't apply fix." TIMEOUT 5
  *MT
  RETURN .F.
ENDIF

USE (lcSysFiles+"sycinst.dbf") IN 0 SHARED AGAIN ALIAS Papa
SELECT Papa
** Added by Maraim, 01/31/2012,[start] if no aria4xp is installed then exit,do not do anything
IF EMPTY(ALLTRIM(Papa.ca4sysdir))
  *WAIT WINDOW "Invalid Aria4 Sqldictionary Folder, Can't apply fix." TIMEOUT 5
  RETURN .F.
ENDIF 
** Added by Maraim, 01/31/2012,[end  ] if no aria4xp is installed then exit,do not do anything
lcResourcesFolder = ADDBS(STRTRAN(ALLTRIM(UPPER(Papa.ca4sysdir)),"\SQLDICTIONARY","")) + "RESOURCE\"
USE IN Papa
IF lcResourcesFolder = "RESOURCE\"
  * WAIT WINDOW "Invalid Folder, Can't apply fix." TIMEOUT 5
  WAIT WINDOW "Invalid Resouces Folder, Can't apply fix." TIMEOUT 5
  RETURN .F.
ENDIF


*lcThisFolder = ADDBS(JUSTPATH(SYS(16)))
*oXML = CREATEOBJECT('wwxml')
*NEWOBJECT('wwxml',lcThisFolder + 'Classes\wwxml.vcx')

USE (lcSysFiles+"sycmenu.dbf") IN 0 SHARED AGAIN ALIAS hisMenu
SELECT hisMenu
SET ORDER TO APPPOPBAR

USE (lcSysFiles+"syuuser.dbf") IN 0 SHARED AGAIN ALIAS hissons
SELECT hissons
SCAN
  WAIT WINDOW 'Updating...'
  =ConvertFav(lcResourcesFolder + ALLTRIM(cuser_id) + "\")
ENDSCAN
USE IN hissons
USE IN hisMenu
RETURN .T.

*==============================================
PROCEDURE ConvertFav
LPARAMETERS FavoritesFolder

LOCAL lcResourceFile, lnSelect, replaced
lcResourceFile = FavoritesFolder + "Favorites.xml"
IF !FILE(lcResourceFile)
  RETURN .F.
ENDIF
lnSelect = SELECT(0)

LOCAL lobjDOMDocument, lcXML
lcXML = FILETOSTR(lcResourceFile)
lobjDOMDocument = CREATEOBJECT("MSXML2.DOMDocument")
lobjDOMDocument.loadXML(lcXML)

LOCAL loRoot 
loRoot = lobjDOMDocument.childNodes(1).childNodes(0)

LOCAL lnIndex
LOCAL loVariable, lcName,  lcValue
lnCnt = 1
FOR lnIndex = 0 TO loRoot.childNodes.Length - 1
  loVariable = loRoot.childNodes(lnIndex)
  lcName     = loVariable.childNodes(2).text
  lcValue    = loVariable.childNodes(4).text
	  
  IF !EMPTY(ALLTRIM(lcValue)) AND SEEK(lcValue, "hisMenu")
    loVariable.childNodes(4).text = UPPER(ALLTRIM(hisMenu.capp_id)) + UPPER(ALLTRIM(hisMenu.cpross_id)) + UPPER(ALLTRIM(hisMenu.cproctype)) + UPPER(ALLTRIM(hisMenu.cmenuparam))
    replaced = .T.
  ENDIF

ENDFOR

IF replaced
  COPY FILE (lcResourceFile) TO (FavoritesFolder + "Favorites_old.xml")
  lobjDOMDocument.Save(lcResourceFile)
ENDIF
lobjDOMDocument = NULL
SELECT(lnSelect)
RETURN .T.
ENDPROC && ConvertFav
