PARAMETERS pentry,ppath
*XXXX
lcMapPath = '\\192.168.1.130\Aria4.0Working'
oNet = CreateObject("WScript.Network")
TRY 
  oNet.RemoveNetworkDrive ("R:",.T.)
CATCH
ENDTRY   
TRY  
  oNet.MapNetworkDrive("R:", lcMapPath)
CATCH
ENDTRY 

*XXXX
LOCAL lcExeFile ,lcGenExe 
  lcExeFile = ppath + (pentry) +'.EXE'
  * USE "(ppath)+genexe.dbf"
  lcGenExe = (ppath)+ 'genexe.dbf'
 
  lcGenExe=Strtran(lcGenExe,'.dbf','.pjx')
 * Copy To (lcEntryFolder)+lcEntry+'Log3.xls'   Fields Name, Source, csubfold ,Type, Tag,Key Xls
  Build Exe (lcExeFile) From (lcGenExe)
 
  * lcEntryFolder=this.CLEAREntryFolder(lcEntryFolder)