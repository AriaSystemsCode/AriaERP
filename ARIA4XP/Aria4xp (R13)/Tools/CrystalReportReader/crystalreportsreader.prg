*M
_SCReen.Visible =.F.
*LPARAMETERS lcAria4Folder 
lcAria4Folder  = GETDIR("","Select Reports folder")
IF !DIRECTORY(lcAria4Folder)
  RETURN .F.
ENDIF
CLOSE ALL 
SET CPDIALOG OFF 
SET SAFETY OFF 
SET MEMOWIDTH TO 8192 
lcLogFile = ADDBS(lcAria4Folder)+"HLOGFILE.TXT"
STRTOFILE('Reports Log File create on:'+DTOC(DATE())+"    "+TIME()+CHR(13)+CHR(10),lcLogFile ,1)
lfChkFldrs(lcAria4Folder)
MESSAGEBOX("LOG File:"+lcLogFile)



FUNCTION lfChkFldrs
LPARAMETERS lcParentFolder 
LOCAL lcDefOld ,lcCurFld 
lcDefOld = FULLPATH("")
SET DEFAULT TO (lcParentFolder)
lcCurFld = FULLPATH("")
LOCAL lnCntFlder ,laFolders,lnFldCnt
DIMENSION laFolders[1]
laFolders = ''
lnCntFlder = ADIR(laFolders,'',"D")
IF lnCntFlder > 0 
  FOR lnFldCnt = 1 TO ALEN(laFolders,1)
    IF laFolders[lnFldCnt,1] == "." OR laFolders[lnFldCnt,1] == '..'
      LOOP
    ELSE
      lfChkFldrs(laFolders[lnFldCnt,1])
      SET DEFAULT TO (lcCurFld)
    ENDIF  
  ENDFOR  
  lfFixFolder(lcParentFolder)
  SET DEFAULT TO (lcCurFld)
ELSE
  lfFixFolder(lcParentFolder)
  SET DEFAULT TO (lcCurFld)
ENDIF
SET DEFAULT TO (lcDefOld)

FUNCTION lfFixFolder
LPARAMETERS lcSourceDir
lnObjCnt = 0
lnObjCnt = ADIR(laObjArr,'*.RPT')
IF lnObjCnt > 0 
  FOR lnCntr = 1  TO lnObjCnt 
    FixHardCodedTxt(ALLTRIM(laObjArr[lnCntr,1]))
  ENDFOR  
ENDIF

FUNCTION FixHardCodedTxt
LPARAMETERS lcFileToFix
lcFileName = lcFileToFix
lcFilePath = ADDBS(JUSTPATH(lcFileName))
lcNameFile = JUSTSTEM(lcFileName)
loCrystal = CREATEOBJECT('CrystalRuntime.Application') 
loReport  = CREATEOBJECT('CrystalRuntime.Report') 
loReport  = loCrystal.OpenReport(lcFileName)
=STRTOFILE("*N000682,1 MMT 12/11/2012 Create File for Crystal reports to translated"+CHR(13)+CHR(10),lcFilePath+lcNameFile+"_CR.H",0)
FOR lnSectCnt= 1 TO loReport.Sections.Count 
  FOR lnObjectCount = 1 TO loReport.Sections.Item (lnSectCnt).ReportObjects.Count
   IF "TEXT" $ UPPER(loReport.Sections.Item (lnSectCnt).ReportObjects.item(lnObjectCount).name)
     loReport.Sections.Item (lnSectCnt).ReportObjects.item(lnObjectCount).text
     lcObjectName = "LANG_"+ALLTRIM(lcNameFile)+"_SECTION"+ALLTRIM(STR(lnSectCnt))+"_ITEM"+ALLTRIM(STR(lnObjectCount))
     =STRTOFILE("#DEFINE "+lcObjectName+" '"+loReport.Sections.Item (lnSectCnt).ReportObjects.item(lnObjectCount).text+"'"+CHR(13)+CHR(10),lcFilePath+lcNameFile+"_CR.H",1)     
   ENDIF   
  ENDFOR 
ENDFOR  

