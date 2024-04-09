*!*********************************************************************************
*!
*!             FUNCTION : lfCreateFrm
*!
*!*********************************************************************************
*E300900,1  HS  07/21/98   Give the user the to choose the report mode.
*!************************************************************************
*
*FUNCTION lfCreateFrm
PARAMETERS lcFormName,lcArrayName,llChkChanges

*E300900,1 Remove this line for we have canceled the Option that change the
*E300900,1 report objects style [Begin]
*DIMENSION laOGObjPos[1,3]
*E300900,1 Remove this line for we have canceled the Option [End]
*E301172,1 AMM 03/16/99 Adjust to fit label forms
*E302566,1 MMT 01/06/2009 Modify SysFiles Paths for SAAS [Task:T20081225.0020]
*!*********************************************************************************

lnMemoWid=SET('MEMOWIDTH')
SET MEMOWIDTH TO 254

*E300406,1 Hesham El-Sheltawi (Start)
PRIVATE lcFileName
IF RAT('\',lcFormName)=0
  lcFullSet = SET('FULLPATH')
  SET FULLPATH ON
  *E301172,1 AMM Search for the .FRX or .LBX files
  *lcFormName= IIF(FILE(gcRepHome+lcFormName+'.FRX'),gcRepHome+lcFormName,gcRepHome+gcAct_Appl+'\'+lcFormName)
  
  *E302566,1 MMT 01/06/2009 Modify SysFiles Paths for SAAS [Start]
  *lcFormName= IIF(FILE(gcRepHome+lcFormName+'.FRX') .OR. FILE(gcRepHome+lcFormName+'.LBX'),;
            gcRepHome+lcFormName,gcRepHome+gcAct_Appl+'\'+lcFormName)
  if (GCMULTIINST AND FILE(gcCRepHome+lcFormName+'.FRX') ) OR;
     (GCMULTIINST AND FILE(gcCRepHome+lcFormName+'.LBX')) OR ;
     (GCMULTIINST AND FILE(gcCRepHome+gcAct_Appl+'\'+lcFormName+'.LBX')) OR ;
     (GCMULTIINST AND FILE(gcCRepHome+gcAct_Appl+'\'+lcFormName+'.FRX'))
     
    lcFormName= IIF(FILE(gcCRepHome+lcFormName+'.FRX') .OR. FILE(gcCRepHome+lcFormName+'.LBX'),;
           gcCRepHome+lcFormName,gcCRepHome+gcAct_Appl+'\'+lcFormName)
    
  ELSE 
    lcFormName= IIF(FILE(gcRepHome+lcFormName+'.FRX') .OR. FILE(gcRepHome+lcFormName+'.LBX'),;
            gcRepHome+lcFormName,gcRepHome+gcAct_Appl+'\'+lcFormName)

  Endif           
  *E302566,1 MMT 01/06/2009 Modify SysFiles Paths for SAAS [End]
  
  *E301172,1 AMM end
  SET FULL &lcFullSet
ENDIF  
lcFileName = IIF(RAT('\',lcFormName)>0,SUBSTR(lcFormName,RAT('\',lcFormName)+1),lcFormName)
*E300406,1 Hesham El-Sheltawi (End)

IF FILE(gcWorkDir+lcOGTmpForm+'.FRX')
  ERASE &gcWorkDir.&lcOGTmpForm..FRX
  ERASE &gcWorkDir.&lcOGTmpForm..FRT  
ENDIF
*E301172,1 AMM Erase the temporary label files if any
IF FILE(gcWorkDir+lcOGTmpForm+'.LBX')
  ERASE &gcWorkDir.&lcOGTmpForm..LBX
  ERASE &gcWorkDir.&lcOGTmpForm..LBT  
ENDIF
*E301172,1 AMM end



*Renee 
*E301172,1 AMM Check the existance of .FRX or .LBX files
*IF FILE(ALLTRIM(lcFormName) + '.FRX')
IF FILE(ALLTRIM(lcFormName) + '.FRX') .OR. FILE(ALLTRIM(lcFormName) + '.LBX')
*E301172,1 AMM end
 *Renee end
  *E301172,1 AMM Create the temporary .FRX or .LBX
  *SELECT * FROM &lcFormName..FRX;
   WHERE PLATFORM=lcOGPlatForm AND objtype<>10;
    INTO DBF &gcWorkDir.&lcOGTmpForm..FRX
  IF FILE(ALLTRIM(lcFormName) + '.FRX')
    SELECT * FROM &lcFormName..FRX;
      WHERE PLATFORM=lcOGPlatForm AND objtype<>10;
      INTO DBF &gcWorkDir.&lcOGTmpForm..FRX
  ELSE
    SELECT * FROM &lcFormName..LBX;
      WHERE PLATFORM=lcOGPlatForm AND objtype<>10;
      INTO DBF &gcWorkDir.&lcOGTmpForm..LBX
  ENDIF
  *E301172,1 AMM end
*Renee 
ELSE
  STORE '' TO lcOGFormV, lcOGTmpForm
  
  RETURN
ENDIF
*Renee end

*E300900,1 Remove these lines for we have canceled the Option that change
*E300900,1 the report objects style [Begin]
*SELECT VPOS,HPOS,RECNO();
*  FROM &gcWorkDir.&lcOGTmpForm..FRX;
* WHERE OBJTYPE IN(8,5,7) AND OBJCODE BETWEEN 0 AND 7;
* ORDER BY VPOS,HPOS;
*  INTO ARRAY laOGObjPos
*=gfADel(@laOGObjPos,1,2)  
*=gfADel(@laOGObjPos,1,2)      
*E300900,1 Remove these lines for we have canceled the Option [End]

IF llChkChanges
  DELETE FOR !lfCanDisp() AND;
              INLIST(OBJTYPE ,8,5,7) AND BETWEEN(OBJCODE,0,7)           
  
  *E300900,1 Change this line for we have canceled the Option that change
  *E300900,1 the report objects style [Begin]
  
  *REPLACE VPOS WITH lfOGVPos(),;
  *        HPOS WITH lfOGHPos(),;
  *        HEIGHT WITH lfOGHSize(),;
  *        WIDTH WITH lfOGWSize(),;
  *        STYLE WITH lfModiStyle(RECNO());
  *    FOR INLIST(OBJTYPE ,8,5,7) AND BETWEEN(OBJCODE,0,7)
  REPLACE VPOS WITH lfOGVPos(),;
          HPOS WITH lfOGHPos(),;
          HEIGHT WITH lfOGHSize(),;
          WIDTH WITH lfOGWSize();
      FOR INLIST(OBJTYPE ,8,5,7) AND BETWEEN(OBJCODE,0,7)
  
  *E300900,1 Change this line for we have canceled the Option [End]
  
  PACK              

*E300900,1 Remove these lines for we have canceled the Option that change
*E300900,1 the report objects style [Begin]
*ELSE
*  REPLACE STYLE WITH lfModiStyle(RECNO());
*     FOR  INLIST(OBJTYPE ,8,5,7) AND BETWEEN(OBJCODE,0,7)
*E300900,1 Remove these lines for we have canceled the Option [End]

ENDIF

IF USED(lcOGTmpForm)
  USE IN (lcOGTmpForm)
ENDIF
*E300406,1 Hesham El-Sheltawi (Start)
IF USED(lcFileName)
*IF !USED(lcFormName)
*E300406,1 Hesham El-Sheltawi (End)
  USE IN (lcFileName)
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

