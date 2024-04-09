*!*********************************************************************************
*!
*!             FUNCTION : lfFormCnt
*!
*!*********************************************************************************
*E302566,1 MMT 01/06/2009 Modify SysFiles Paths for SAAS [Task:T20081225.0020]
* Function to initialize the objects for the form in an array
PARAMETERS lcFormName,lcArrayName
DIMENSION &lcArrayName[1]
STORE .F. TO &lcArrayName
lnMemoWid=SET('MEMOWIDTH')
SET MEMOWIDTH TO 254
*E300406,1 Hesham El-Sheltawi (Start)
PRIVATE lcFileName
IF RAT('\',lcFormName)=0
  lcFullSet = SET('FULLPATH')
  SET FULLPATH ON
  
  *E302566,1 MMT 01/06/2009 Modify SysFiles Paths for SAAS [Start]
  *lcFormName= IIF(FILE(gcRepHome+lcFormName+'.FRX'),gcRepHome+lcFormName,gcRepHome+gcAct_Appl+'\'+lcFormName)
  IF GCMULTIINST and (FILE(gcCRepHome+lcFormName+'.FRX') OR FILE(gcCRepHome+gcAct_Appl+'\'+lcFormName))
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

*E300406,1 Hesham El-Sheltawi (Start)
IF USED(lcFileName)
*IF !USED(lcFormName)
*E300406,1 Hesham El-Sheltawi (End)
  SELECT VPOS,HPOS,lfCanDisp();
         FROM &lcFormName..FRX;
   WHERE OBJTYPE IN(8,5,7) AND OBJCODE BETWEEN 0 AND 7;
     AND PLATFORM = lcOGPlatForm;
   ORDER BY VPOS,HPOS;          
    INTO ARRAY &lcArrayName
    =gfADel(@&lcArrayName,1,2)  
    =gfADel(@&lcArrayName,1,2)                
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
SET MEMOWIDTH TO lnMemoWid

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

