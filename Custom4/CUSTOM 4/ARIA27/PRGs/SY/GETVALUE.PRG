*:************************************************************************
*: Program file  : GETCALUE.PRG
*: Program desc. : Main system
*: For screen    :
*:         System: Aria advantage series
*:         Module: Main system 
*:      Developer: 
*:************************************************************************
*: Calls : 
*:         Procedures :
*:         Functions  : 
*:************************************************************************
*: Passed Parameters  : 
*:************************************************************************
***  this program to get the right hand side values **********
PRIVATE lcToProceed
DIMENSION laScrData[1]
STORE .F. TO llOk,llAdd
STORE IIF(cRightDes='V',IIF(cLeftType='D',CTOD(mrightHead),mrighthead),'') TO laScrData[1]
lcOld=''
IF cLeftType ='N'
  STORE VAL(mRightHead) TO laScrData[1]
ENDIF


** initialize the arrays needed if the field have valid entries or valid codes
** two arrays one have the valid values and the other have the selected values
IF lVldEntry
  lcSelect=SELECT()
  lcFieldName = PADR(MLINE(mlField_Nm,1),10)
  DIMENSION laVldEnt[1]
  SELECT cDiscrep FROM &gcData_Dir.codes ;
    WHERE cfld_Name = lcFieldName;
    INTO ARRAY laVldEnt
  SELECT(lcSelect)  
  IF cOperator='In List'
    DIMENSION laVldVal[1,1]
   =gfSubStr(ALLTRIM(mRightHead),@laVldVal,lcElmSep)
    DIMENSION laVldVal[ALEN(laVldVal,1),1]   
  ENDIF  
ELSE
  IF !EMPTY(mVEntries)  
    DIMENSION laVldEnt[1,1]
    lcVldEnt=SUBSTR(mvEntries,1,ATC(lcVldSep,mvEntries)-1)
      =gfSubStr(lcVldEnt,@laVldEnt,lcElmSep)
    DIMENSION laVldEnt[ALEN(laVldEnt,1),1]      
    IF cOperator='In List'
      DIMENSION laVldVal[1,1]
     =gfSubStr(ALLTRIM(mRightHead),@laVldVal,lcElmSep)
      DIMENSION laVldVal[ALEN(laVldVal,1),1]   
    ENDIF      
  ENDIF
ENDIF
IF (lVldEntry OR !EMPTY(mvEntries)) AND cOperator='In List'
  lnOldDim =ALEN(laVldVal,1)
  DECLARE laOldTarg[lnOldDim]
  IF ALEN(laVldVal,1) = 1 .AND. TYPE('laVldVal[1]')="L"
    laVldVal =' '
  ENDIF  

  DEFINE POPUP puSource  MARGIN RELATIVE SCROLL MARK CHR(16)
  FOR lnCount = 1 TO ALEN('laVldEnt',1)
    DEFINE BAR lnCount OF puSource PROMPT (ALLTRIM(laVldEnt[lnCount,1]))
    IF ASCAN('laVldVal',laVldEnt[lnCount]) > 0
      SET SKIP OF BAR lnCount OF puSource .T.
    ENDIF
  ENDFOR
  STORE 1  TO lsSource,lsTarget
  pbExit = 2
ENDIF
**********************************************************************



IF cOperator = 'Between'  && if the operator is Between the we have 
  DIMENSION laScrData[2]  && to initialize the variables needed for 
  =lfInitVal(mrField_Nm,'laScrData') && the get fields 
ENDIF


******** these initializations for the fields that does not have  ******
******              valid entries or valid codes only              ***** 
IF cOperator='In List' AND (!lVldEntry AND EMPTY(mVEntries))
   DIMENSION laVldVal[1,1]
   =lfInitVal(ALLTRIM(mRightHead),'laVldVal')
ENDIF

IF !INLIST(cOperator,'Between','In List') AND ;
   (lVldEntry OR !EMPTY(mVEntries))
  lnRow=6
  lnCol=35
ELSE
   IF !INLIST(cOperator,'Between','In List')
     lnRow=6
        lnCol=10+MAX(nWidth+4,25)  
   ELSE
    lnRow=20
    lnCol=77
  ENDIF  
ENDIF   
**********************************************************************

DO GetValue.SPR

IF llOk   && if the termination happend because the user select ok
  lcScrData=lfGetStr(IIF(cOperator='In List','laVldVal','laScrData'))
  REPLACE mRightHead WITH lcScrData,;
          mrField_NM WITH lfTrnsValue(mlField_Nm,lcScrData,'H'),;
          cRightDes WITH 'V'
  IF lfarngCurs('PUQUERY')        
    =lfPopFill('puquery',lcArray)          
  ENDIF  
  =lfChangeBar(lsQuery,'puquery')          
ENDIF

*!*************************************************************************
*!
*!              Procedure : pop_choice
*!
*!*************************************************************************
*  this procedure to save the selection from a popup to the get field
* it is used for the combo box fields for the fields that have valid 
* entries or codes
PROCEDURE pop_choice
PARAMETERS m.value,m.popup,m.var_name,m.dbf_field
PRIVATE m.value,m.popup,m.var_name,m.dbf_field
&var_name=PROMPT()
DEACTIVATE POPUP (m.popup)
RETURN


*!*************************************************************************
*!
*!              Function: lfGetPic
*!
*!*************************************************************************
*  this function to return the picture of a field using the currunt record
*  fields of the cursor file
FUNCTION lfGetPic
IF EMPTY(cPict_str)
  DO CASE 
    CASE  cLeftType='C' 
      RETURN REPLICATE('X',nWidth)
    CASE  cLeftType='{  /  /  }' 
      RETURN '@ D'
    CASE  cLeftType='N' 
      RETURN '@BL$ '+REPLICATE('9',NWIDTH-NDECIMAL-1)+IIF(NDECIMAL>0,'.','');
             +IIF(NDECIMAL>0,REPLICATE('9',NDECIMAL),'')
    CASE  cLeftType='M'
      RETURN  REPLICATE('X',165)
  ENDCASE
ENDIF
RETURN '@'+cPict_Str   


*!*************************************************************************
*!
*!              Function: lfGetSize
*!
*!*************************************************************************
*  this function to return the size of a get field using the current record
* of the cursor file information
FUNCTION lfGetSize
PARAMETERS lnSize
IF EMPTY(NWIDTH)
  RETURN lnSize
ENDIF
RETURN IIF(cLeftType='D',10,nWidth)


*!*************************************************************************
*!
*!              Function: lfGetBox
*!
*!*************************************************************************
*  this function is used for the windows version only to return
*  size to be used in the box around the get field
FUNCTION lfGetBox
PARAMETERS lnSize
IF EMPTY(NWIDTH)
  RETURN lnSize
ENDIF
RETURN IIF(cLeftType='D',10,IIF(nWidth<lnSize,lnSize,nWidth+1))



*!*************************************************************************
*!
*!              Function: lfvOk
*!
*!*************************************************************************
*  valid function for the ok push button
FUNCTION lfvOk
** check if the field have a valid codes or a valid entries
** AND the operator <> In List then check if it is one one
** the defined value in the popup or not if not return to 
** the get field again
IF (lVldEntry OR !EMPTY(mvEntries)) AND cOperator<>'In List'
  IF ASCAN(laVldEnt,laScrData[1])=0
    _CUROBJ=OBJNUM(laScrData[1])
     KEYBOARD CHR(24)
    RETURN
  ENDIF
ENDIF
** if the operator is Between check if the from value is less than or equal
** to the TO value  "the range start is less than or equal the range end" 
IF cOperator='Between'  
  IF laScrData[2]<laScrData[1]
    WAIT '" From " Value must be Less than or equal to "To" value' WINDOW NOWAIT
    _CUROBJ=OBJNUM(laScrData[2])
    RETURN
  ENDIF
ENDIF
llOk=.T.
CLEAR READ








*!*************************************************************************
*!
*!              Function: lfShow
*!
*!*************************************************************************
*  the show function for the screen 
FUNCTION lfShow
IF (lVldEntry OR !EMPTY(mvEntries)) AND cOperator='In List'
  IF ALEN('laVldVal',1) = ALEN('laVldEnt',1)  AND !EMPTY(laVldVal[1]);
    OR EMPTY(laVldEnt[1])
    SHOW GET lsSource     DISABLE
    SHOW GET pbMove       DISABLE
    SHOW GET pbAll        DISABLE
  ELSE
    SHOW GET lsSource     ENABLE
    SHOW GET pbMove       ENABLE
    SHOW GET pbAll        ENABLE
  ENDIF  
  
  IF EMPTY(laVldVal[1])
    SHOW GET lsTarget    DISABLE
    SHOW GET pbRemove    DISABLE
    SHOW GET pbRAll      DISABLE
  ELSE
    SHOW GET lsTarget    ENABLE
    SHOW GET pbRemove    ENABLE
    SHOW GET pbRAll      ENABLE
  ENDIF  
ENDIF  




*********   the validation functions to the screen objects   ********
*!*************************************************************************
*!
*!              Function: lfvSource
*!
*!*************************************************************************
*
FUNCTION lfvSource

IF lsSource <= ALEN('laVldEnt',1) AND lsSource <> 0
  SET SKIP OF BAR lsSource OF puSource .T.

  IF !EMPTY(laVldVal[1]) 
    DIMENSION laVldVal[ALEN(laVldVal)+1]
  ENDIF
  laVldVal[ALEN(laVldVal)]= ALLTRIM(laVldEnt[lsSource])
ENDIF  

lnStart  = lsSource
lsSource = 0

FOR lnCount = lnStart TO CNTBAR('puSource')
  IF !SKPBAR('puSource',lnCount)
    lsSource = lnCount 
    EXIT
  ENDIF  
ENDFOR

IF lsSource = 0
  FOR lnCount = 1 TO CNTBAR('puSource')
    IF !SKPBAR('puSource',lnCount)
      lsSource = lnCount 
      EXIT
    ENDIF  
  ENDFOR
ENDIF  

_CUROBJ = OBJNUM(lsSource)
SHOW GETS

*!*************************************************************************
*!
*!              Function: lfvTarget
*!
*!*************************************************************************
*
FUNCTION lfvTarget

IF lsTarget <= ALEN('laVldVal',1) AND lsTarget <> 0
  lsSource  = ASUBSCRIPT('laVldEnt',;
                   ASCAN('laVldEnt',laVldVal[lsTarget]),1)
  SET MARK OF POPUP puSource .F.
  SET SKIP OF BAR lsSource OF puSource .F.

  =ADEL(laVldVal,lsTarget)
  IF ALEN(laVldVal,1) > 1
    DIMENSION laVldVal[ALEN(laVldVal)-1]
  ELSE
    laVldVal[1] =' '
  ENDIF  
ENDIF


_CUROBJ = OBJNUM(lsTarget)
SHOW GETS


*!*************************************************************************
*!
*!              Function: lfvMovmnts
*!
*!*************************************************************************
*
FUNCTION lfvMovmnts

PARAMETERS lnMovmnts

DO CASE
  CASE lnMovmnts = 1
    _CUROBJ = OBJNUM(lsSource)
    KEYBOARD "{ENTER}"
  CASE lnMovmnts = 2
    DECLARE laVldVal[ALEN('laVldEnt',1)]
    =ACOPY(laVldEnt,laVldVal)
    SET SKIP OF POPUP puSource .T.
    SHOW GETS
  CASE lnMovmnts = 3
    _CUROBJ = OBJNUM(lsTarget)
    KEYBOARD "{ENTER}"
  CASE lnMovmnts = 4
    DECLARE laVldVal[1]        
    laVldVal =' '
    SET SKIP OF POPUP puSource .F.
    SHOW GETS
ENDCASE

*!*************************************************************************
*!
*!              Function: lfvCancel
*!
*!*************************************************************************
*
FUNCTION lfvCancel

DECLARE laVldVal[lnOldDim]
=ACOPY(laOldTarg,laVldVal)


*!*************************************************************************
*!
*!              Function: lfvVal_Str
*!
*!*************************************************************************
*   this function for the fields that have its own validation
* like the key fields or the forgen fields
FUNCTION lfvVal_Str
lcVarName=ALLTRIM(VARREAD())
llValid=.T.
IF lcFilter<>'R'
IF LASTKEY()=13 
  IF !EMPTY(&lcVarName)
    IF !EMPTY(ALLTRIM(mvald_str))
      lcToProceed=ALLTRIM(mvald_str)
      lcToProceed=STRTRAN(lcToProceed,'*VALUE*','"'+lcVarName+'"')
      lcToProceed=STRTRAN(lcToProceed,'*OLDVAL*','lcOld')
      llValid=&lcToProceed
    ENDIF
  ENDIF  
ELSE
 &lcVarName=lcOld
 SHOW GET &lcVarName
ENDIF
ENDIF
RETURN llValid


*!*************************************************************************
*!
*!              Function: lfvAdd
*!
*!*************************************************************************
*   valid function to add  a new value to the list of values
FUNCTION lfvAdd
laScrData[1]=lfInitialize()
SHOW GET laScrData[1] ENABLE
llAdd=.T.
_CUROBJ=OBJNUM(laScrData[1])


*!*************************************************************************
*!
*!              Function: lfvChange
*!
*!*************************************************************************
*  valid function for the push button change to change one of the values
*  in the list
FUNCTION lfvChange
SHOW GET laScrData[1] ENABLE
_CUROBJ=OBJNUM(laScrData[1])


*!*************************************************************************
*!
*!              Function: lfvRem
*!
*!*************************************************************************
*  this function to remove one value from the list of values
FUNCTION lfvRem
IF ALEN(laVldVal,1)>1
  =ADEL(laVldVal,lsSource)
  DIMENSION laVldVal(ALEN(laVldVal,1)-1,ALEN(laVldVal,2))
ELSE
 STORE '' TO laVldVal
ENDIF
SHOW GET lsSource
_CUROBJ=OBJNUM(lsSource)



*!*************************************************************************
*!
*!              Function: lfvAddToL
*!
*!*************************************************************************
*  this function to add the value to the list after it have been check
* that it is a valid value
FUNCTION lfvAddToL
lcVarName=VARREAD()
IF llAdd
  llAdd=.F.
  IF ASCAN(laVldVal,&lcVarName)=0
    IF !EMPTY(laVldVal[1,1])  
      DIMENSION laVldVal[ALEN(laVldVal,1)+1,ALEN(laVldVal,2)]    
    ENDIF  
    laVldVal[ALEN(laVldVal,1),1]=&lcVarName
  ENDIF
ELSE
  IF ASCAN(laVldVal,&lcVarName)=0
    laVldVal[lsSource,1]=&lcVarName
  ENDIF
ENDIF  
SHOW GET &lcVarName DISABLE
SHOW GET lsSource
_CUROBJ=OBJNUM(lsSource)


*!*************************************************************************
*!
*!              Function: lfwVldList
*!
*!*************************************************************************
*  this function is the when function of the list of values
*  to change the displaying of the values on the screen
FUNCTION lfwVldList
IF !EMPTY(laVldVal[1,1])
  SHOW GET pbChange ENABLE
  SHOW GET pbRemove ENABLE
ELSE 
  SHOW GET pbChange DISABLE
  SHOW GET pbRemove DISABLE
ENDIF
laScrData[1]=laVldVal[lsSource,1]
SHOW GET laScrData[1]

*************************************************************************



**********  tools functions to change the data types for and to characters
FUNCTION lfInitialize
DO CASE
  CASE cLeftType $ 'CM'
    RETURN SPACE(nwidth)
  CASE cLeftType='N'
    RETURN 0
  CASE cLeftType='D'
    RETURN CTOD('')
ENDCASE  


FUNCTION lfInitVal
PARAMETERS lcStrToUse,lcArrName
DIMENSION lcTemName[1]
=gfSubStr(lcStrToUse,@lcTemName,lcElmSep)
DIMENSION &lcArrName[ALEN(lcTemName,1)]   
STORE ''  TO &lcArrName
=ACOPY(lcTemName,&lcArrName)
DIMENSION &lcArrName[ALEN(&lcArrName,1),1]   
 FOR lnCount=1 TO ALEN(&lcArrName,1)
      &lcArrName[lnCount,1] = lfTrnsStr(&lcArrName[lnCount,1] )
 ENDFOR
 IF cOperator='Between'
  IF ALEN(&lcArrName,1)<2
    DIMENSION &lcArrName[2,1]
    STORE lfTrnsStr('') TO  &lcArrName[2,1]
  ENDIF
ENDIF

FUNCTION lfTrnsStr
PARAMETER lcValue
DO CASE
  CASE cLeftType $ 'CM'
    RETURN lcValue
  CASE cLeftType = "N"
    RETURN VAL(lcValue)     
  CASE cLeftType = 'D'
    RETURN CTOD(lcValue)     
  CASE cLeftType = 'L'
    RETURN lcValue  
ENDCASE  

FUNCTION lfGetStr
PARAMETERS lcArrName
lcRetVal=''
 FOR lnCount=1 TO ALEN(&lcArrName,1)
     lcRetVal = lcRetVal+IIF(lnCount=1,'',lcElmSep)+;
                IIF(cOperator<>'In List',lfChngToStr(&lcArrName[lnCount,1]),;
                    lfChngToStr(&lcArrName[lnCount]))
 ENDFOR
RETURN lcRetVal

FUNCTION lfChngToStr
PARAMETER lcValue
DO CASE
  CASE cLeftType $ 'CM'
    RETURN lcValue
  CASE cLeftType = "N"
    RETURN STR(lcValue,nWidth,nDecimal)     
  CASE cLeftType = 'D'
    RETURN DTOC(lcValue)     
  CASE cLeftType = 'L'
    RETURN lcValue
ENDCASE  

*************************************************************************