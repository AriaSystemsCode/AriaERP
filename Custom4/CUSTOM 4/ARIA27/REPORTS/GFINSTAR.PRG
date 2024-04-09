*!*********************************************************************************
*!
*!             FUNCTION : lfInitStyleArr
*!
*!*********************************************************************************
*E302566,1 MMT 01/06/2009 Modify SysFiles Paths for SAAS [Task:T20081225.0020]
* Function to initialize the objects for the form in an array
*FUNCTION lfInitStyleArr
PARAMETERS lcFormName,lcArrayName
PRIVATE lcErrHand,llErrHapend
*lcErrHand = ON('ERROR')
*llErrHapend=.F.
*ON ERROR llErrHapend=.T.
lnMemoWid=SET('MEMOWIDTH')
SET MEMOWIDTH TO 254
lcOGSelect=SELECT()
DIMENSION &lcArrayName[1,ALEN(&lcArrayName,2)]
STORE '' TO &lcArrayName
*E300406,1 Hesham El-Sheltawi (Start)
PRIVATE lcFileName
IF RAT('\',lcFormName)=0
  lcFullSet = SET('FULLPATH')
  SET FULLPATH ON
  
  *E302566,1 MMT 01/06/2009 Modify SysFiles Paths for SAAS [Start]
  *lcFormName= IIF(FILE(gcRepHome+lcFormName+'.FRX'),gcRepHome+lcFormName,gcRepHome+gcAct_Appl+'\'+lcFormName)
  if GCMULTIINST and (FILE(gcCRepHome+lcFormName+'.FRX') or FILE(gcCRepHome+gcAct_Appl+'\'+lcFormName))
    lcFormName= IIF(FILE(gcCRepHome+lcFormName+'.FRX'),gcCRepHome+lcFormName,gcCRepHome+gcAct_Appl+'\'+lcFormName)
  ELSE
    lcFormName= IIF(FILE(gcRepHome+lcFormName+'.FRX'),gcRepHome+lcFormName,gcRepHome+gcAct_Appl+'\'+lcFormName)
  ENDIF
  *E302566,1 MMT 01/06/2009 Modify SysFiles Paths for SAAS [End]
  
  SET FULL &lcFullSet
ENDIF  
lcFileName = IIF(RAT('\',lcFormName)>0,SUBSTR(lcFormName,RAT('\',lcFormName)+1),lcFormName)
IF !USED(lcFileName)
*IF !USED(lcFormName)
*E300406,1 Hesham El-Sheltawi (End)
  SELECT 0
  USE &lcFormName..FRX  
ENDIF
*******
LOCATE FOR PLATFORM=lcOGPlatForm
IF !FOUND()
  lcOGPlatForm='DOS'
ENDIF
*******
*lcFormName=IIF(ATC('.',lcFormName)=0,ALLTRIM(lcFormName)+'.FRX',lcFormName)
*E300406,1 Hesham El-Sheltawi (Start)
IF USED(lcFileName)
*IF !USED(lcFormName)
*E300406,1 Hesham El-Sheltawi (End)
  IF lcOGPlatForm='DOS' OR lcOGPlatForm='UNIX'
     SELECT EXPR,lfOGVPos() ,lfOGHPos(),;
            lfOGHSize(),lfOGWSize(),;
            STYLE,objtype,objcode;
            FROM &lcFormName..FRX;
      WHERE OBJTYPE IN(8,5,7) AND OBJCODE BETWEEN 0 AND 7;
        AND PLATFORM = lcOGPlatForm;
      ORDER BY VPOS,HPOS;
       INTO ARRAY &lcArrayName
  ELSE
    SELECT EXPR,lfOGVPos() ,lfOGHPos(),;
           lfOGHSize(),lfOGWSize(),;
           FONTFACE,FONTSTYLE,FONTSIZE,objtype,objcode;
           FROM &lcFormName..FRX;
     WHERE OBJTYPE IN(8,5,7) AND OBJCODE BETWEEN 0 AND 7;
       AND PLATFORM = lcOGPlatForm;
     ORDER BY VPOS,HPOS;
      INTO ARRAY &lcArrayName
  ENDIF
ENDIF

*E300406,1 Hesham El-Sheltawi (Start)
IF USED(lcFileName)
*IF !USED(lcFormName)
*E300406,1 Hesham El-Sheltawi (End)
  USE IN (lcFileName)
ENDIF
IF USED(lcOGTmpForm)
  USE IN (lcOGTmpForm)
ENDIF
*IF llErrHapend
*  DIMENSION &lcArrayName[1,ALEN(&lcArrayName,2)]
*  STORE '' TO &lcArrayName
*ENDIF
*ON ERROR &lcErrHand  
SELECT(lcOGSelect)  
SET MEMOWIDTH TO lnMemoWid


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
