_SCREEN.CAPTION = "Aria Send Object"
_vfp.Visible = .F.
HIDE WINDOW ALL
DECLARE INTEGER GetPrivateProfileString IN win32api STRING, STRING, STRING, STRING @, INTEGER, STRING
DECLARE INTEGER GetPrivateProfileInt IN win32api STRING, STRING, INTEGER, STRING

DIMENSION laDesSite[1,2]
STORE '' TO laDesSite
WINDOWSDIRECTORY = GETENV('winbootdir')
WINDOWSDIRECTORY = IIF(EMPTY(WINDOWSDIRECTORY),GETENV('SystemRoot'),WINDOWSDIRECTORY)+'\'
lcReturn = SPACE(100)
= GetPrivateProfileString  ("Aria Path", "Local", "NONE", @lcReturn, 100, "ARIA27.INI")
lcSyspath = ALLT(STRTRAN(lcReturn,CHR(0)))
IF lcSyspath = 'NONE' OR EMPTY(lcSyspath)
  RETURN
ENDIF  
SET FULLPATH ON
lcSyspath = IIF(!FILE(lcSyspath+'SYCINST.DBF'),'',lcSyspath)    
SET FULLPATH OFF
IF EMPTY(lcSyspath)
  RETURN .F.
ENDIF  
llUsedInst = USED('SYCINST')
IF !llUsedInst 
  USE (lcSyspath+'SYCINST') IN 0 
ENDIF
SELECT SYCINST
lcCurrentSite = SYCINST.CCURSITEID
lcCommunicationPath = ALLT(SYCINST.CCOMPATH)
lcCommunicationPath = lcCommunicationPath+'OUTBOX\'
IF !llUsedInst
  USE IN SYCINST
ENDIF

SELECT DIST SYCSITCM.CDESSITE,SYCSITES.CSITEDESC ;
 FROM (lcSysPath+'SYCSITCM'),(lcSysPath+'SYCSITES');
 WHERE SYCSITCM.CDESSITE=SYCSITES.CSITEID;
 AND SYCSITCM.CSORSITE = lcCurrentSite ;
 INTO ARRAY laDesSite
lcdessite = ladessite[1,1]
IF ALEN(laDesSite,1) > 1
  lcdessite = ''
  _vfp.Visible = .T.  
  DO FORM SELSITE WITH ladessite TO lcdessite
*  READ EVENTS
  _vfp.Visible = .F.  
ENDIF

IF EMPTY(lcdessite)
  RETURN
ENDIF
lcCurrentSite = ALLT(lcCurrentSite)
lcdessite = ALLT(lcdessite)
lcCommunicationPath = lcCommunicationPath+lcdessite+'\'
llError = .F.
ON ERROR llError = .T.
DELETE FILE ('A:\*.ZIP')
COPY FILE (lcCommunicationPath+lcdessite+'.ZIP') TO ('A:\'+lcdessite+'.ZIP')
ON ERROR
IF !llError AND FILE('A:\'+lcdessite+'.ZIP')
  SET TEXTME ON
  SET TEXTME TO A:\RECEIVE.BAT
  \COPY A:\<<lcdessite+'.ZIP'>> C:\ARIACOMM\INBOX\<<lcCurrentSite>>\<<lcdessite>>.ZIP
  \C:\ARIACOMM\ARIALOG <<lcCurrentSite>> R
  SET TEXTME TO
  SET TEXTME OFF
  RUN /N C:\ARIACOMM\ARIALOG &lcdessite S
ELSE
  =MESSAGEBOX("Cann't copy file "+lcCommunicationPath+lcdessite+'.ZIP!'+' Please try later',16,'Aria Send Object')  
ENDIF
