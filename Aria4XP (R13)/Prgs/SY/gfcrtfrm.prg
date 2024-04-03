*!*****************************************************************************************
*! Name      : GFCRTFRM
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 10/16/2002 10:02:28 PM
*! Purpose   :
*! Entry no. : N000398,1 - Build Aria3 Option Grid
*!*****************************************************************************************
*! Parameters:
*!****************************************************************************************
*! Returns   :
*!****************************************************************************************
*! Runs as following:
*!
*!****************************************************************************************
*! Modifications  :
*! E302567,1 MMT 01/06/2009 Change file paths for SAAS[Task:T20081225.0022]
*! B608981,1 MMT 08/24/2009 fix bug of cannot report from menu if it is working from request builder[T20080422.0042]
*! E302857,1 HES 02/10/2011 Avoid 'X:\Aria4xp\SRVRPTS' Fixed Path [T20110206.0017]
*!****************************************************************************************
*!
PARAMETERS lcFormName,lcArrayName,llChkChanges
LOCAL lnActiveAlias, llReturn
lnActiveAlias = SELECT(0)
SELECT 0
*BEYONDPRO_FRX ...................................... Begin
oAriaApplication.ActiveReportFullXML = ''
*BEYONDPRO_FRX ...................................... End
llReturn = CreateForm(lcFormName,lcArrayName,llChkChanges)
SELECT (lnActiveAlias)
RETURN llReturn
*-- end of main program.

*!*****************************************************************************************
*! Name      : CreateForm
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 10/16/2002 10:02:28 PM
*! Purpose   : Creates a report temporary form.
*! Entry no. : N000398,1 - Build Aria3 Option Grid
*!*****************************************************************************************
*! Parameters:
*!****************************************************************************************
*! Returns   :
*!****************************************************************************************
*! Runs as following:
*!
*!****************************************************************************************
*!
FUNCTION CreateForm
  PARAMETERS lcFormName,lcArrayName,llChkChanges

  lnMemoWid=SET('MEMOWIDTH')
  SET MEMOWIDTH TO 254

  PRIVATE lcFileName
  IF RAT('\',lcFormName)=0
    lcFullSet = SET('FULLPATH')
    SET FULLPATH ON

    *! E302567,1 MMT 01/06/2009 Change file paths for SAAS[Start]
    *lcFormName= IIF(FileExist(gcRepHome+lcFormName+'.FRX') .OR. FileExist(gcRepHome+lcFormName+'.LBX'),;
              gcRepHome+lcFormName,gcRepHome+gcAct_Appl+'\'+lcFormName)
    *! B608981,1 MMT 08/24/2009 fix bug of cannot report from menu if it is working from request builder[Start]
*!*		IF (oAriaApplication.MULTIINST AND FileExist(oAriaApplication.clientreporthome+ lcFormName+'.FRX')) OR ;
*!*		   (oAriaApplication.MULTIINST AND FileExist(oAriaApplication.clientreporthome+ lcFormName+'.LBX'))
*!*		
*!*	      lcFormName= IIF(FileExist(oAriaApplication.clientreporthome+lcFormName+'.FRX') .OR. FileExist(oAriaApplication.clientreporthome+lcFormName+'.LBX'),;
*!*	              oAriaApplication.clientreporthome+lcFormName,oAriaApplication.clientreporthome+gcAct_Appl+'\'+lcFormName)

    *! E302857,1 HES 02/10/2011 Avoid Fixed Path ------- BEGIN
*!*		IF (oAriaApplication.MULTIINST AND FileExist(oAriaApplication.clientreporthome+ lcFormName+'.FRX')) OR ;
*!*		   (oAriaApplication.MULTIINST AND FileExist(oAriaApplication.clientreporthome+ lcFormName+'.LBX')) OR ;
*!*		   (oAriaApplication.MULTIINST AND FileExist(oAriaApplication.clientreporthome+gcAct_Appl+'\'+lcFormName+'.FRX')) OR ;
*!*		   (oAriaApplication.MULTIINST AND FileExist(oAriaApplication.clientreporthome+gcAct_Appl+'\'+lcFormName+'.LBX')) OR;
*!*		   (oAriaApplication.MULTIINST AND FileExist('X:\Aria4xp\SRVRPTS\'+ lcFormName+'.FRX')) OR ;
*!*	       (oAriaApplication.MULTIINST AND FileExist('X:\Aria4xp\SRVRPTS\'+ lcFormName+'.LBX')) OR ;
*!*	       (oAriaApplication.MULTIINST AND FileExist('X:\Aria4xp\SRVRPTS\'+ gcAct_Appl+'\'+ lcFormName+'.FRX')) OR ;
*!*	       (oAriaApplication.MULTIINST AND FileExist('X:\Aria4xp\SRVRPTS\'+ gcAct_Appl+'\'+ lcFormName+'.LBX'))
	IF (oAriaApplication.MULTIINST AND FileExist(oAriaApplication.clientreporthome+ lcFormName+'.FRX')) OR ;
	   (oAriaApplication.MULTIINST AND FileExist(oAriaApplication.clientreporthome+ lcFormName+'.LBX')) OR ;
	   (oAriaApplication.MULTIINST AND FileExist(oAriaApplication.clientreporthome+gcAct_Appl+'\'+lcFormName+'.FRX')) OR ;
	   (oAriaApplication.MULTIINST AND FileExist(oAriaApplication.clientreporthome+gcAct_Appl+'\'+lcFormName+'.LBX')) OR;
	   (oAriaApplication.MULTIINST AND FileExist(oAriaApplication.CLIENTSRVREPORTHOME + lcFormName+'.FRX')) OR ;
       (oAriaApplication.MULTIINST AND FileExist(oAriaApplication.CLIENTSRVREPORTHOME + lcFormName+'.LBX')) OR ;
       (oAriaApplication.MULTIINST AND FileExist(oAriaApplication.CLIENTSRVREPORTHOME + gcAct_Appl+'\'+ lcFormName+'.FRX')) OR ;
       (oAriaApplication.MULTIINST AND FileExist(oAriaApplication.CLIENTSRVREPORTHOME + gcAct_Appl+'\'+ lcFormName+'.LBX'))
    *! E302857,1 HES 02/10/2011 Avoid Fixed Path ------- END
	
	  DO CASE
	
	    CASE FileExist(oAriaApplication.clientreporthome+ lcFormName+'.FRX') OR ;
           FileExist(oAriaApplication.clientreporthome+ lcFormName+'.LBX')
	      lcFormName= oAriaApplication.clientreporthome+ lcFormName

		CASE FileExist(oAriaApplication.clientreporthome+gcAct_Appl+'\'+ lcFormName+'.FRX') OR;
         FileExist(oAriaApplication.clientreporthome+gcAct_Appl+'\'+ lcFormName+'.LBX')

	      lcFormName= oAriaApplication.clientreporthome+gcAct_Appl+'\'+ lcFormName	

        *! E302857,1 HES 02/10/2011 Avoid Fixed Path ------- BEGIN
*!*		    CASE FileExist('X:\Aria4xp\SRVRPTS\'+ lcFormName+'.FRX') OR;
*!*	           FileExist('X:\Aria4xp\SRVRPTS\'+ lcFormName+'.LBX')
*!*		      lcFormName= 'X:\Aria4xp\SRVRPTS\'+ lcFormName	
*!*		    CASE FileExist('X:\Aria4xp\SRVRPTS\'+ gcAct_Appl+'\'+  lcFormName+'.FRX') OR ;
*!*	           FileExist('X:\Aria4xp\SRVRPTS\'+ gcAct_Appl+'\'+lcFormName+'.LBX')
*!*		      lcFormName= 'X:\Aria4xp\SRVRPTS\'+ gcAct_Appl+'\'+ lcFormName	
	    CASE FileExist(oAriaApplication.CLIENTSRVREPORTHOME + lcFormName+'.FRX') OR;
           FileExist(oAriaApplication.CLIENTSRVREPORTHOME + lcFormName+'.LBX')
	      lcFormName= oAriaApplication.CLIENTSRVREPORTHOME + lcFormName	
	    CASE FileExist(oAriaApplication.CLIENTSRVREPORTHOME + gcAct_Appl+'\'+  lcFormName+'.FRX') OR ;
           FileExist(oAriaApplication.CLIENTSRVREPORTHOME + gcAct_Appl+'\'+lcFormName+'.LBX')
	      lcFormName= oAriaApplication.CLIENTSRVREPORTHOME + gcAct_Appl+'\'+ lcFormName	  	
        *! E302857,1 HES 02/10/2011 Avoid Fixed Path ------- END 	

	  ENDCASE
	*! B608981,1 MMT 08/24/2009 fix bug of cannot report from menu if it is working from request builder[End]
	ELSE
	*! B608981,1 MMT 08/24/2009 fix bug of cannot report from menu if it is working from request builder[Start]
    * lcFormName= IIF(FileExist(gcRepHome+lcFormName+'.FRX') .OR. FileExist(gcRepHome+lcFormName+'.LBX'),;
              gcRepHome+lcFormName,gcRepHome+gcAct_Appl+'\'+lcFormName)
      lcSrvRpts = STRTRAN(UPPER(oAriaApplication.ReportHome),'REPORTS','SRVRPTS')
      DO CASE
	
	    CASE FileExist(oAriaApplication.ReportHome+ lcFormName+'.FRX') OR;
           FileExist(oAriaApplication.ReportHome+ lcFormName+'.LBX')
	      lcFormName= oAriaApplication.ReportHome+ lcFormName

		CASE FileExist(oAriaApplication.ReportHome+gcAct_Appl+'\'+ lcFormName+'.FRX') OR ;
		     FileExist(oAriaApplication.ReportHome+gcAct_Appl+'\'+ lcFormName+'.LBX')
		
	      lcFormName= oAriaApplication.ReportHome+gcAct_Appl+'\'+ lcFormName	

	    CASE FileExist(lcSrvRpts + lcFormName+'.FRX') OR;
	    	 FileExist(lcSrvRpts + lcFormName+'.LBX')
	      lcFormName= lcSrvRpts + lcFormName	
	
	    CASE FileExist(lcSrvRpts + gcAct_Appl+'\'+  lcFormName+'.FRX') OR;
	     FileExist(lcSrvRpts + gcAct_Appl+'\'+lcFormName+'.LBX')
	      lcFormName= lcSrvRpts + gcAct_Appl+'\'+ lcFormName	
	  ENDCASE
    *! B608981,1 MMT 08/24/2009 fix bug of cannot report from menu if it is working from request builder[End]
    ENDIF
    *! E302567,1 MMT 01/06/2009 Change file paths for SAAS[End]
    SET FULL &lcFullSet
  ENDIF
  lcFileName = IIF(RAT('\',lcFormName)>0,SUBSTR(lcFormName,RAT('\',lcFormName)+1),lcFormName)

  IF FileExist(gcWorkDir+lcOGTmpForm+'.FRX')
    ERASE &gcWorkDir.&lcOGTmpForm..FRX
    ERASE &gcWorkDir.&lcOGTmpForm..FRT
  ENDIF

  IF FileExist(gcWorkDir+lcOGTmpForm+'.LBX')
    ERASE &gcWorkDir.&lcOGTmpForm..LBX
    ERASE &gcWorkDir.&lcOGTmpForm..LBT
  ENDIF

  IF FileExist(ALLTRIM(lcFormName) + '.FRX') .OR. FileExist(ALLTRIM(lcFormName) + '.LBX')
    IF FileExist(ALLTRIM(lcFormName) + '.FRX')
      *BEYONDPRO_FRX ...................................... Begin
      oAriaApplication.SetActiveReportFullXML(ALLTRIM(lcFormName) + '.FRX')
      *BEYONDPRO_FRX ...................................... End
      SELECT * FROM (lcFormName+".FRX");
        WHERE PLATFORM=lcOGPlatForm AND objtype<>10;
        INTO DBF (gcWorkDir+lcOGTmpForm+".FRX")

      *-- Handle the FRX in England Case (A4 instead to Letter size)
      IF (lcOGPlatForm = "WINDOWS") AND (oAriaApplication.DefaultCountry = "ENG") AND USED(lcOGTmpForm)
        SELECT (lcOGTmpForm)
        LOCATE FOR OBJTYPE = 1 AND OBJCODE = 53 AND ASC(SUBSTR(Tag2,47,47))=1
        IF FOUND()
          REPLACE TAG2 WITH STUFF(TAG2,47,1,CHR(9))
        ENDIF
      ENDIF

    ELSE
      *BEYONDPRO_FRX ...................................... Begin
      oAriaApplication.SetActiveReportFullXML(ALLTRIM(lcFormName) + '.LBX')
      *BEYONDPRO_FRX ...................................... End
      SELECT * FROM (lcFormName+".LBX");
        WHERE PLATFORM=lcOGPlatForm AND objtype<>10;
        INTO DBF (gcWorkDir+lcOGTmpForm+".LBX")
    ENDIF
  ELSE
    STORE '' TO lcOGFormV, lcOGTmpForm
    RETURN
  ENDIF

  IF llChkChanges
    DELETE FOR !lfCanDisp() AND;
                INLIST(OBJTYPE ,8,5,7) AND BETWEEN(OBJCODE,0,7)
    REPLACE VPOS WITH lfOGVPos(),;
            HPOS WITH lfOGHPos(),;
            HEIGHT WITH lfOGHSize(),;
            WIDTH WITH lfOGWSize();
        FOR INLIST(OBJTYPE ,8,5,7) AND BETWEEN(OBJCODE,0,7)
    PACK
  ENDIF

  IF USED(lcOGTmpForm)
    USE IN (lcOGTmpForm)
  ENDIF

  IF USED(lcFileName)
    USE IN (lcFileName)
  ENDIF
  SET MEMOWIDTH TO lnMemoWid

  LOCAL lnSelected
  lnSelected = SELECT()

  *-- MAH 18/03/2009 T20090211.0027 [Start]
  *B608862 ,1 MMT 05/12/2009 Fix bug of Error While preview DOS reports[Start]
  IF lcOGPlatForm = "WINDOWS"
  *B608862 ,1 MMT 05/12/2009 Fix bug of Error While preview DOS reports[End]

    = lfCopyToVFP9(gcWorkDir, lcOGTmpForm)

  *B608862 ,1 MMT 05/12/2009 Fix bug of Error While preview DOS reports[Start]
  ENDIF
  *B608862 ,1 MMT 05/12/2009 Fix bug of Error While preview DOS reports[End]
  *-- MAH 18/03/2009 T20090211.0027 [End]

  SELECT(lnSelected)
ENDFUNC


*!*********************************************************************************
*!
*!             FUNCTION : lfModiStyle
*!
*!*********************************************************************************
*  function to change the style of the objects in the frx
FUNCTION lfModiStyle
PARAMETERS lnRecNo
lnArrPos=ASCAN(laOGObjPos,lnRecNo)
lnArrPos=IIF(lnArrPos>0,ASUBSCRIPT(laOGObjPos,lnArrPos,1),0)
RETURN IIF(lnArrPos=0,'',ALLTRIM(&lcArrayName[lnArrPos,6]))



*!*********************************************************************************
*!
*!             FUNCTION : lfOGVPos
*!
*!*********************************************************************************
*
FUNCTION lfOGVPos
PARAMETERS lnObjPos
m.YPos=VPOS
IF ATCLINE('#VPOSITION ',comment)>0
  lcClaus=MLINE(comment,ATCLINE('#VPOSITION ',comment))
  lcSize=ALLTRIM(SUBSTR(lcClaus,ATC("#VPOSITION ",lcClaus)+10))
  m.YPos=EVAL(lcSize)
ENDIF
RETURN m.Ypos

*!*********************************************************************************
*!
*!             FUNCTION : lfOGHPos
*!
*!*********************************************************************************
*
FUNCTION lfOGHPos
PARAMETERS lnObjPos
m.XPos=HPOS
IF ATCLINE('#HPOSITION ',comment)>0
  lcClaus=MLINE(comment,ATCLINE('#HPOSITION ',comment))
  lcSize=ALLTRIM(SUBSTR(lcClaus,ATC("#HPOSITION ",lcClaus)+10))
  m.XPos =EVAL(lcSize)
  m.XPos=ROUND(m.Xpos*IIF(lcOGPlatForm='WINDOWS' OR lcOGPlatForm='MAC',FONTMETRIC(6,ALLTRIM(FONTFACE),FONTSIZE)*104.16665,1),3)
ENDIF
RETURN m.XPos

*!*********************************************************************************
*!
*!             FUNCTION : lfOGHSize
*!
*!*********************************************************************************
*
FUNCTION lfOGHSize
PARAMETERS lnObjPos
m.Height = HEIGHT
lnClaus = ATCLINE('#HSIZE ',comment)
IF lnClaus > 0
  lcClaus=MLINE(comment,ATCLINE('#HSIZE ',comment))
  lcSize=ALLTRIM(SUBSTR(lcClaus,ATC("#HSIZE ",lcClaus)+6))
  m.Height=EVAL(lcSize)
ENDIF
RETURN m.HEIGHT

*!*********************************************************************************
*!
*!             FUNCTION : lfOGWSize
*!
*!*********************************************************************************
*
FUNCTION lfOGWSize
PARAMETERS lnObjPos
m.Width  = WIDTH
lnClaus = ATCLINE('#WSIZE ',comment)
IF lnClaus > 0
  lcClaus=MLINE(comment,ATCLINE('#WSIZE ',comment))
  lcSize=ALLTRIM(SUBSTR(lcClaus,ATC("#WSIZE ",lcClaus)+6))
  m.Width =EVAL(lcSize)
  m.width=ROUND(m.WIDTH*IIF(lcOGPlatForm='WINDOWS' OR lcOGPlatForm='MAC',FONTMETRIC(6,ALLTRIM(FONTFACE),FONTSIZE)*104.16665,1),3)
ENDIF
RETURN m.Width

*!*********************************************************************************
*!
*!             FUNCTION : lfCanDisp
*!
*!*********************************************************************************
*  Function to See if the object can be selected to the frx or not
FUNCTION lfCanDisp
lcCondition=''
llCanDisp=.T.
IF !EMPTY(Comment)
  IF ATCLINE("#OBJDISP",comment)>0
    lcCondition = SUBSTR(MLINE(comment,ATCLINE("#OBJDISP",comment)),;
    ATC("#OBJDISP",MLINE(comment,ATCLINE("#OBJDISP",comment)))+9)
  ENDIF
ENDIF
IF !EMPTY(lcCondition)
  lnNoCond=OCCURS('#OBJDISP',UPPER(comment))
  FOR lnCount=1 to lnNoCond
    lcCondition = ALLTRIM(SUBSTR(comment,ATC("#OBJDISP",comment,lnCount)))
    lcCondition = SUBSTR(MLINE(lcCondition,ATCLINE("#OBJDISP",lcCondition)),;
    ATC("#OBJDISP",MLINE(lcCondition,ATCLINE("#OBJDISP",lcCondition)))+9)
    llCanDisp=EVAL(lcCondition)
    IF !llCanDisp
      EXIT
    ENDIF
  ENDFOR
ENDIF
RETURN llCanDisp

*!*********************************************************************************
*!
*!             FUNCTION : lfGetCom
*!
*!*********************************************************************************
*
FUNCTION lfGetCom
PARAMETERS lcString
PRIVATE ALL
IF ATC('(',lcString)=0
  RETURN ATC(',',lcString)
ELSE
  lnCouts=0
  lnComPos=0
  FOR lnCount = 1 TO LEN(lcString)
    DO CASE
      CASE SUBSTR(lcString,lnCount,1)='('
        lnCouts=lnCouts+1
      CASE SUBSTR(lcString,lnCount,1)=')'
        lnCouts=lnCouts-1
      CASE SUBSTR(lcString,lnCount,1)=','
        IF lnCouts=0
          lnComPos=lnCount
          EXIT
        ENDIF
    ENDCASE
  ENDFOR
ENDIF
RETURN lnComPos

*!*****************************************************************************************
*! Name      : lfCopyToVFP9
*! Developer : MAH - Mahmud Said
*! Date      : 18/03/2009
*! Purpose   : Convert VFP Version 2.0 report format to VFP 9.0 report format
*! Entry no. : T20090211.0027
*!*****************************************************************************************
*! Parameters:
*! lcWorkDir : Report directory
*! lcTable   : Report file name
*!****************************************************************************************
*! Returns   :
*!****************************************************************************************

FUNCTION lfCopyToVFP9
LPARAMETERS lcWorkDir, lcTable

DIMENSION laFldStu[75,4]

laFldStu[01,1] = 'PLATFORM'
laFldStu[01,2] = 'C'
laFldStu[01,3] = 8
laFldStu[01,4] = 0

laFldStu[02,1] = 'UNIQUEID'
laFldStu[02,2] = 'C'
laFldStu[02,3] = 10
laFldStu[02,4] = 0

laFldStu[03,1] = 'TIMESTAMP'
laFldStu[03,2] = 'N'
laFldStu[03,3] = 10
laFldStu[03,4] = 0

laFldStu[04,1] = 'OBJTYPE'
laFldStu[04,2] = 'N'
laFldStu[04,3] = 2
laFldStu[04,4] = 0

laFldStu[05,1] = 'OBJCODE'
laFldStu[05,2] = 'N'
laFldStu[05,3] = 3
laFldStu[05,4] = 0

laFldStu[06,1] = 'NAME'
laFldStu[06,2] = 'M'
laFldStu[06,3] = 10
laFldStu[06,4] = 0

laFldStu[07,1] = 'EXPR'
laFldStu[07,2] = 'M'
laFldStu[07,3] = 10
laFldStu[07,4] = 0

laFldStu[08,1] = 'VPOS'
laFldStu[08,2] = 'N'
laFldStu[08,3] = 9
laFldStu[08,4] = 3

laFldStu[09,1] = 'HPOS'
laFldStu[09,2] = 'N'
laFldStu[09,3] = 9
laFldStu[09,4] = 3

laFldStu[10,1] = 'HEIGHT'
laFldStu[10,2] = 'N'
laFldStu[10,3] = 9
laFldStu[10,4] = 3

laFldStu[11,1] = 'WIDTH'
laFldStu[11,2] = 'N'
laFldStu[11,3] = 9
laFldStu[11,4] = 3

laFldStu[12,1] = 'STYLE'
laFldStu[12,2] = 'M'
laFldStu[12,3] = 10
laFldStu[12,4] = 0

laFldStu[13,1] = 'PICTURE'
laFldStu[13,2] = 'M'
laFldStu[13,3] = 10
laFldStu[13,4] = 0

laFldStu[14,1] = 'ORDER'
laFldStu[14,2] = 'M'
laFldStu[14,3] = 10
laFldStu[14,4] = 0

laFldStu[15,1] = 'UNIQUE'
laFldStu[15,2] = 'L'
laFldStu[15,3] = 1
laFldStu[15,4] = 0

laFldStu[16,1] = 'COMMENT'
laFldStu[16,2] = 'M'
laFldStu[16,3] = 10
laFldStu[16,4] = 0

laFldStu[17,1] = 'ENVIRON'
laFldStu[17,2] = 'L'
laFldStu[17,3] = 1
laFldStu[17,4] = 0

laFldStu[18,1] = 'BOXCHAR'
laFldStu[18,2] = 'C'
laFldStu[18,3] = 1
laFldStu[18,4] = 0

laFldStu[19,1] = 'FILLCHAR'
laFldStu[19,2] = 'C'
laFldStu[19,3] = 1
laFldStu[19,4] = 0

laFldStu[20,1] = 'TAG'
laFldStu[20,2] = 'M'
laFldStu[20,3] = 10
laFldStu[20,4] = 0

laFldStu[21,1] = 'TAG2'
laFldStu[21,2] = 'M'
laFldStu[21,3] = 10
laFldStu[21,4] = 0

laFldStu[22,1] = 'PENRED'
laFldStu[22,2] = 'N'
laFldStu[22,3] = 5
laFldStu[22,4] = 0

laFldStu[23,1] = 'PENGREEN'
laFldStu[23,2] = 'N'
laFldStu[23,3] = 5
laFldStu[23,4] = 0

laFldStu[24,1] = 'PENBLUE'
laFldStu[24,2] = 'N'
laFldStu[24,3] = 5
laFldStu[24,4] = 0

laFldStu[25,1] = 'FILLRED'
laFldStu[25,2] = 'N'
laFldStu[25,3] = 5
laFldStu[25,4] = 0

laFldStu[26,1] = 'FILLGREEN'
laFldStu[26,2] = 'N'
laFldStu[26,3] = 5
laFldStu[26,4] = 0

laFldStu[27,1] = 'FILLBLUE'
laFldStu[27,2] = 'N'
laFldStu[27,3] = 5
laFldStu[27,4] = 0

laFldStu[28,1] = 'PENSIZE'
laFldStu[28,2] = 'N'
laFldStu[28,3] = 5
laFldStu[28,4] = 0

laFldStu[29,1] = 'PENPAT'
laFldStu[29,2] = 'N'
laFldStu[29,3] = 5
laFldStu[29,4] = 0

laFldStu[30,1] = 'FILLPAT'
laFldStu[30,2] = 'N'
laFldStu[30,3] = 5
laFldStu[30,4] = 0

laFldStu[31,1] = 'FONTFACE'
laFldStu[31,2] = 'M'
laFldStu[31,3] = 10
laFldStu[31,4] = 0

laFldStu[32,1] = 'FONTSTYLE'
laFldStu[32,2] = 'N'
laFldStu[32,3] = 3
laFldStu[32,4] = 0

laFldStu[33,1] = 'FONTSIZE'
laFldStu[33,2] = 'N'
laFldStu[33,3] = 3
laFldStu[33,4] = 0

laFldStu[34,1] = 'MODE'
laFldStu[34,2] = 'N'
laFldStu[34,3] = 3
laFldStu[34,4] = 0

laFldStu[35,1] = 'RULER'
laFldStu[35,2] = 'N'
laFldStu[35,3] = 1
laFldStu[35,4] = 0

laFldStu[36,1] = 'RULERLINES'
laFldStu[36,2] = 'N'
laFldStu[36,3] = 1
laFldStu[36,4] = 0

laFldStu[37,1] = 'GRID'
laFldStu[37,2] = 'L'
laFldStu[37,3] = 1
laFldStu[37,4] = 0

laFldStu[38,1] = 'GRIDV'
laFldStu[38,2] = 'N'
laFldStu[38,3] = 2
laFldStu[38,4] = 0

laFldStu[39,1] = 'GRIDH'
laFldStu[39,2] = 'N'
laFldStu[39,3] = 2
laFldStu[39,4] = 0

laFldStu[40,1] = 'FLOAT'
laFldStu[40,2] = 'L'
laFldStu[40,3] = 1
laFldStu[40,4] = 0

laFldStu[41,1] = 'STRETCH'
laFldStu[41,2] = 'L'
laFldStu[41,3] = 1
laFldStu[41,4] = 0

laFldStu[42,1] = 'STRETCHTOP'
laFldStu[42,2] = 'L'
laFldStu[42,3] = 1
laFldStu[42,4] = 0

laFldStu[43,1] = 'TOP'
laFldStu[43,2] = 'L'
laFldStu[43,3] = 1
laFldStu[43,4] = 0

laFldStu[44,1] = 'BOTTOM'
laFldStu[44,2] = 'L'
laFldStu[44,3] = 1
laFldStu[44,4] = 0

laFldStu[45,1] = 'SUPTYPE'
laFldStu[45,2] = 'N'
laFldStu[45,3] = 1
laFldStu[45,4] = 0

laFldStu[46,1] = 'SUPREST'
laFldStu[46,2] = 'N'
laFldStu[46,3] = 1
laFldStu[46,4] = 0

laFldStu[47,1] = 'NOREPEAT'
laFldStu[47,2] = 'L'
laFldStu[47,3] = 1
laFldStu[47,4] = 0

laFldStu[48,1] = 'RESETRPT'
laFldStu[48,2] = 'N'
laFldStu[48,3] = 2
laFldStu[48,4] = 0

laFldStu[49,1] = 'PAGEBREAK'
laFldStu[49,2] = 'L'
laFldStu[49,3] = 1
laFldStu[49,4] = 0

laFldStu[50,1] = 'COLBREAK'
laFldStu[50,2] = 'L'
laFldStu[50,3] = 1
laFldStu[50,4] = 0

laFldStu[51,1] = 'RESETPAGE'
laFldStu[51,2] = 'L'
laFldStu[51,3] = 1
laFldStu[51,4] = 0

laFldStu[52,1] = 'GENERAL'
laFldStu[52,2] = 'N'
laFldStu[52,3] = 3
laFldStu[52,4] = 0

laFldStu[53,1] = 'SPACING'
laFldStu[53,2] = 'N'
laFldStu[53,3] = 3
laFldStu[53,4] = 0

laFldStu[54,1] = 'DOUBLE'
laFldStu[54,2] = 'L'
laFldStu[54,3] = 1
laFldStu[54,4] = 0

laFldStu[55,1] = 'SWAPHEADER'
laFldStu[55,2] = 'L'
laFldStu[55,3] = 1
laFldStu[55,4] = 0

laFldStu[56,1] = 'SWAPFOOTER'
laFldStu[56,2] = 'L'
laFldStu[56,3] = 1
laFldStu[56,4] = 0

laFldStu[57,1] = 'EJECTBEFOR'
laFldStu[57,2] = 'L'
laFldStu[57,3] = 1
laFldStu[57,4] = 0

laFldStu[58,1] = 'EJECTAFTER'
laFldStu[58,2] = 'L'
laFldStu[58,3] = 1
laFldStu[58,4] = 0

laFldStu[59,1] = 'PLAIN'
laFldStu[59,2] = 'L'
laFldStu[59,3] = 1
laFldStu[59,4] = 0

laFldStu[60,1] = 'SUMMARY'
laFldStu[60,2] = 'L'
laFldStu[60,3] = 1
laFldStu[60,4] = 0

laFldStu[61,1] = 'ADDALIAS'
laFldStu[61,2] = 'L'
laFldStu[61,3] = 1
laFldStu[61,4] = 0

laFldStu[62,1] = 'OFFSET'
laFldStu[62,2] = 'N'
laFldStu[62,3] = 3
laFldStu[62,4] = 0

laFldStu[63,1] = 'TOPMARGIN'
laFldStu[63,2] = 'N'
laFldStu[63,3] = 3
laFldStu[63,4] = 0

laFldStu[64,1] = 'BOTMARGIN'
laFldStu[64,2] = 'N'
laFldStu[64,3] = 3
laFldStu[64,4] = 0

laFldStu[65,1] = 'TOTALTYPE'
laFldStu[65,2] = 'N'
laFldStu[65,3] = 2
laFldStu[65,4] = 0

laFldStu[66,1] = 'RESETTOTAL'
laFldStu[66,2] = 'N'
laFldStu[66,3] = 2
laFldStu[66,4] = 0

laFldStu[67,1] = 'RESOID'
laFldStu[67,2] = 'N'
laFldStu[67,3] = 3
laFldStu[67,4] = 0

laFldStu[68,1] = 'CURPOS'
laFldStu[68,2] = 'L'
laFldStu[68,3] = 1
laFldStu[68,4] = 0

laFldStu[69,1] = 'SUPALWAYS'
laFldStu[69,2] = 'L'
laFldStu[69,3] = 1
laFldStu[69,4] = 0

laFldStu[70,1] = 'SUPOVFLOW'
laFldStu[70,2] = 'L'
laFldStu[70,3] = 1
laFldStu[70,4] = 0

laFldStu[71,1] = 'SUPRPCOL'
laFldStu[71,2] = 'N'
laFldStu[71,3] = 1
laFldStu[71,4] = 0

laFldStu[72,1] = 'SUPGROUP'
laFldStu[72,2] = 'N'
laFldStu[72,3] = 2
laFldStu[72,4] = 0

laFldStu[73,1] = 'SUPVALCHNG'
laFldStu[73,2] = 'L'
laFldStu[73,3] = 1
laFldStu[73,4] = 0

laFldStu[74,1] = 'SUPEXPR'
laFldStu[74,2] = 'M'
laFldStu[74,3] = 10
laFldStu[74,4] = 0

laFldStu[75,1] = 'user'
laFldStu[75,2] = 'M'
laFldStu[75,3] = 10
laFldStu[75,4] = 0

 *B608862 ,1 MMT 05/12/2009 Fix bug of Error While preview DOS reports[Start]
IF FileExist(ALLTRIM(lcFormName) + '.FRX')
 *B608862 ,1 MMT 05/12/2009 Fix bug of Error While preview DOS reports[End]
  COPY FILE (lcWorkDir + lcTable + ".frx") TO (lcWorkDir + "x" + lcTable + ".frx")
  COPY FILE (lcWorkDir + lcTable + ".frt") TO (lcWorkDir + "x" + lcTable + ".frt")
  CREATE TABLE (lcWorkDir + lcTable + ".frx") FROM ARRAY laFldStu

*B608862 ,1 MMT 05/12/2009 Fix bug of Error While preview DOS reports[Start]
ELSE
  COPY FILE (lcWorkDir + lcTable + ".LBX") TO (lcWorkDir + "x" + lcTable + ".LBX")
  COPY FILE (lcWorkDir + lcTable + ".LBT") TO (lcWorkDir + "x" + lcTable + ".LBT")
  CREATE TABLE (lcWorkDir + lcTable + ".LBX") FROM ARRAY laFldStu
ENDIF
*B608862 ,1 MMT 05/12/2009 Fix bug of Error While preview DOS reports[End]

SELECT &lcTable.
APPEND BLANK
REPLACE PLATFORM   WITH 'WINDOWS',;
        UNIQUEID   WITH '_0'+"12345454",;
        TIMESTAMP  WITH  784434970,;
        OBJTYPE    WITH 1,;
        OBJCODE    WITH 53,;
        VPOS       WITH 0,;
        HPOS       WITH 0,;
        HEIGHT     WITH 0,;
        WIDTH      WITH 0,;
        ENVIRON    WITH .F.,;
        FONTFACE   WITH 'Courier New',;
        FONTSTYLE  WITH 0,;
        FONTSIZE   WITH 10,;
        RULER      WITH 1,;
        RULERLINES WITH 1,;
        GRID       WITH .T.,;
        GRIDV      WITH 12,;
        GRIDH      WITH 12,;
        TOP        WITH .T.,;
        BOTTOM     WITH .T.,;
        ADDALIAS   WITH .T.,;
        CURPOS     WITH .T.
*B608862 ,1 MMT 05/12/2009 Fix bug of Error While preview DOS reports[Start]
IF FileExist(ALLTRIM(lcFormName) + '.FRX')
 *B608862 ,1 MMT 05/12/2009 Fix bug of Error While preview DOS reports[End]
  APPEND FROM (lcWorkDir + "x" + lcTable + ".frx")
 *B608862 ,1 MMT 05/12/2009 Fix bug of Error While preview DOS reports[Start]
ELSE
  APPEND FROM (lcWorkDir + "x" + lcTable + ".LBX")
ENDIF
 *B608862 ,1 MMT 05/12/2009 Fix bug of Error While preview DOS reports[End]
USE IN &lcTable.
