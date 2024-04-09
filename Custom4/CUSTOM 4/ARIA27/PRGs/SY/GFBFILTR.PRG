*:************************************************************************
*: Program file  : GFBFILTR.PRG
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
* this is a global function used to build a filter for the set filter to
* or for the SQL command
*B601025,1 Hesham El-Sheltawi 04/09/96
*B601025,1 change the popup size to 41 if we are calling the program from
*B601025,1 the report generator so we can see the full description
*E300869,1  HS  04/22/98   Allow expressions in filter option headers
*E301053,1 AMM 11/04/98 Fix the bug of calling Codes file even if there is no active company
*E301077,78 Hesham 02/25/1999
*E301077,78 Controlling opening and clossing files.
*E301077,78 to make the program open the files when ever it need it
*E300789,1 Hesham 03/04/99
*E300789,1 remove field ccomp_id from files ACCOD,FISHD,FSPRD,FSHLD,CODES
*E300789,1 Change Any seeking in these files
*B802649,1 AMM 09/26/1999 Fix the bug of not filtering of any code in the global browse
*E037351,1 HBG 23/12/2003 Add new field to SYREPUVR to differentiate between records belong to
*E037351,1                Aria27 and records belong to Aria4.
*B038612,1 MHM 10/13/2004 Fix the bug of not filtering Correct
*:************************************************************************

*FUNCTION gfBldFlter
PARAMETERS lcArray,laField_H,laField_N,lcFilter,lcRepID,lcFileToUse
*lcArray   Name  of array from outside to deal with
*laField_H Array of the availiable for filter headers
*laField_N Array of the availiable for filter Names

*lcFilter  a character to determine whether we are running the 
*program for building a FILTER or a REPORT or for the AT RUN TIME

** Define all the private variables so the outside vaiables won't get affected
PRIVATE lnListNo,lsQuery,Inv,lnLastSelect,pbdum,lcFields,lcCurObj,;
        lcLastVal,lcNewData,lcToProceed,llAdd,llIsAdded,lcLastVal,;
        llTerminat,llNotOr,laOper,lcRightHand,laRightHand,;
        lcFilter,lcCurDbf,llCanDisp,lcSeekName,lcUsedFlt,Inv,laOper,;
        Pbdum,laRightHand,llFldOpen,lcFldFlt,llCodeOpen,;
        lcCodeFlt,lcUsedFlt,laObjScrP,lcPrompt,llCanVald
IF TYPE('laVldOper[1]')<> 'C'
  PRIVATE laVldOper
ENDIF
* Declaration Part for the variables and arrays which will be Use
**********************************************************************************        
DECLARE Inv[7,4],laOper[8,1],PBDUM[3],;
        laRightHand[IIF((lcfilter='F'),2,3),1] ,laObjScrP[1,5]
IF TYPE('laVldOper[1]')<> 'C'
  DECLARE laVldOper[8,1]
ENDIF
llCanVald = .T.
IF TYPE('laScrMode')='L'
  llCanVald = laScrMode[3] OR laScrMode[4]
ENDIF
lcVldSep='~'
lcElmSep='|'


lcCurDbf=SELECT()
lcSeekName=ALIAS() &&+'.DBF'
IF lcFilter='F' 
  STORE lcCurFilter TO lcUsedFlt
ENDIF  
llCanClear=!EMPTY(&lcArray[1,1])
STORE 0 TO lnListNo,lsquery,Inv,lnLastSelect,PBDUM
STORE '' TO lcFields,lcCurObj,lcLastVal,lcNewData,lcToProceed,lcFldFlt,lcCodeFlt
STORE .F. TO llAdd,llIsAdded,llTerminat,llNotOr,llFldOpen,llCodeOpen
STORE .T. TO llCanDisp
PRIVATE lcToProceed
lcExact = SET('EXACT')
SET EXACT ON


laOper[1]='Like'
laOper[2]='Greater Than'
laOper[3]='Less Than'
laOper[4]='Greater Or Equal'
laOper[5]='Less Or Equal'
laOper[6]='Between'
laOper[7]='Contains'
laOper[8]='In List'

IF TYPE('laVldOper[1]')<> 'C'
  laVldOper[1]='CMLND'
  laVldOper[2]='CMND'
  laVldOper[3]='CMND'
  laVldOper[4]='CMND'
  laVldOper[5]='CMND'
  laVldOper[6]='CND'
  laVldOper[7]='CM'
  laVldOper[8]='CND'
ENDIF  
lcRightHand = 'VF'+IIF((lcfilter='F'),'','E')
laRightHand[1,1] = 'Value'
IF (lcFilter='R') OR (lcFilter='E')
  laRightHand[2,1] = 'Another Data Item'
ENDIF  
IF (lcfilter='R') OR (lcFilter='E')
  laRightHand[3,1] = 'Expression'
ENDIF  
**********************************************************************************        

** Opening the files that will be used to define the fields data
IF USED('SYDFIELD')
  llFldOpen=.F.
  SELECT SYDFIELD
  SET ORDER TO 1
ELSE
 llFldOpen=.T.
 SELECT SELECT(1)
 USE &gcSysHome.SYDFIELD ORDER 1
ENDIF


IF USED('codes')
  SELECT codes
  SET ORDER TO 1
ELSE
  *E301053,1 AMM Open the codes file only if there is an active company .
  *llCodeOpen=.T.
  *SELECT SELECT(1)
  *USE &gcDataDir.codes ORDER 1
  IF !EMPTY(gcAct_Comp)
    llCodeOpen = gfOpenFile(gcDataDir+'CODES','Idrltfname','SH')
  ENDIF
  *E301053,1 AMM end
ENDIF
*E301077,78 Hesham (Start)
llFiltUse = .F.
*E301077,78 Hesham (End)
IF lcFilter='F'
*  IF USED('SYUFILTR')
*    SELECT syufiltr
*    SET ORDER TO 1
*  ELSE
*   SELECT SELECT(1)
*   USE &gcsysHome.syufiltr ORDER 1
*  ENDIF
  *E301077,78 Hesham (Start)
  llFiltUse = gfSysOpen(gcSysHome+'SYUFILTR','Cflter_id')
  *E301077,78 Hesham (End)

  ****************************************************************
  lnCanLoad=0
  COUNT  FOR ((cUser_ID=gcUser_ID)  OR lAllUSERS=.T.) AND cFile_Nam=lcSeekName AND;
         cComp_ID=gcAct_Comp TO lnCanLoad
ENDIF  
****************************************************************
** Creating a cursor to be use to hold all the information about the filter
** so there is no need to search for the field informations buts once
CREATE CURSOR TEMCursor ( mHeader M,MLFIELD_NM M,LISNOT L,COPERATOR C(20),;
                              CLEFTTYPE C(1),CLEFTDES C(1),MRFIELD_NM M,;
                              MRIGHTHEAD M,CRIGHTTYPE C(1);
                              ,CRIGHTDES C(1),NWIDTH N(5,0),NDECIMAL N(5,0),MVAld_Str M;
                              ,cpict_str C(35),lvldEntry L,mventries M,nfieldPos N(5,0))


** fields to be gathered in the cursor
lcFields='MLField_Nm,cleftdes,cleftType,lisnot,coperator,';
         +'Mrfield_Nm,crightdes'
****************************************************************
=lfFillFile(lcArray)  && fill the cursor file using the passed array
lsQuery=IIF(RECCOUNT()>0,1,0)   && initialize the list
****************************************************************
*B601025,1 Hesham (Start)
*B601025,1 change the popup size to 41 if we are calling the program from
*B601025,1 the report generator so we can see the full description
*DEFINE POPUP puField FROM 1,1 TO IIF(_DOS,10,15),31 SCROLL  && Define the fields popup
DEFINE POPUP puField FROM 1,1 TO IIF(_DOS,10,15),IIF(lcfilter='R' OR lcFilter='E',41,31) SCROLL  && Define the fields popup
*B601025,1 Hesham (End)
=ACOPY(laField_H,laBarCont)
** if this program is running to build a report so add Expression... to the fields popup
** so we can make the lefthand an expression
*IF lcfilter='R'   
*  DIMENSION laBarCont[ALEN(laField_H,1)+2,ALEN(laField_H,2)]
*  laBarCont[ALEN(laBarCont,1)-1,1]='\-'
*  laBarCont[ALEN(laBarCont,1),1]='< Expression... >'
*ELSE
*  DIMENSION laBarCont[ALEN(laField_H,1),ALEN(laField_H,2)]  
  DIMENSION laBarCont[ALEN(laField_H,1),1]  
*ENDIF  


=lfFillPop('puField','laBarCont')

** Define the list with a mover and scroll bar
DEFINE POPUP PUQUERY MOVER SCROLL MARGIN MARK ''
=lfPopFill('puquery',lcArray)

** Define the operator popup
DEFINE POPUP TTT FROM 1,1 TO 12,20 SCROLL
=lfFillPop('TTT','laOper')
** Define the righthand popup
DEFINE POPUP puRightHand FROM 1,1 TO 12,20 SCROLL
=lfFillPop('puRightHand','laRightHand')
****************************************************************

PUSH KEY
** repeat while no terminate is needed so we can terminate the
** read to use only one level of the read
DO WHILE !llTerminat  
  DO SYBFiltr.SPR
  IF !EMPTY(lcToProceed)
*    lcToProceed=IIF(LEFT(ALLTRIM(lcToProceed),3)<>'DO ','='+ALLTRIM(lcToProceed),lcToProceed)
    &lcToProceed
    lcToProceed=''
  ENDIF
ENDDO  

POP KEY
RELEASE POPUP puField
RELEASE POPUP TTT
RELEASE POPUP PUQUERY
SET EXACT &lcExact

IF llFldOpen
  USE IN SYDFIELD
ENDIF

IF llCodeOpen
  USE IN codes
ENDIF

*E301077,78 Hesham (Start)
USE IN IIF(llFiltUse,'SYUFILTR',0)
*E301077,78 Hesham (End)


USE IN temCursor

SELECT(lcCurDbf)

*!*************************************************************************
*!
*!              Function: lfShow
*!
*!*************************************************************************
*  This function is a show function for the screen

FUNCTION lfShow
IF lcFilter='R' AND !llCanVald
  SHOW GET pbAdd DISABLE
  RETURN
ENDIF
llCanDisp=.F.
IF lcFilter='F'
   SHOW GET pbOk,1 PROMPT 'Set \!\<Filter'
  IF lnCanLoad>0
    SHOW GET pbLoad ENABLE
  ELSE
    SHOW GET pbLoad DISABLE
  ENDIF

  IF !llCanClear
    SHOW GET PbClrFlt DISABLE
  ELSE
    SHOW GET PbClrFlt ENABLE
  ENDIF

  IF RECCOUNT()=0
    SHOW GET pbSave DISABLE
    SHOW GET pbOk   DISABLE
  ELSE
    SHOW GET pbSave ENABLE  
    SHOW GET pbOk ENABLE
  ENDIF
ENDIF  

IF RECCOUNT()=0
  SHOW GET pbSeeFlt DISABLE
ELSE
  SHOW GET pbSeeFlt ENABLE
  SHOW GET pbOk Enable
ENDIF
IF lsquery<=0 
  SHOW GET PBRemove DISABLE
*  =IIF(_DOS,lfClear(),lfClearW())
ENDIF
IF CNTBAR('PUQUERY')>=1
   lnRecord=IIF(RECNO()>RECCOUN(),0,RECNO())
   LOCATE FOR nfieldpos=GETBAR('PUQUERY',CNTBAR('PUQUERY'))
  IF MlField_Nm = '.OR.'
    SHOW GET PBOr DISABLE
  ELSE
    SHOW GET PBOr ENABLE      
  ENDIF
  IF lnRecord<>0
    GO lnRecord
  ENDIF
ELSE  
  SHOW GET PBOr DISABLE  
ENDIF  

*!*************************************************************************
*!
*!              Function: lfwList
*!
*!*************************************************************************
*  when function for the list
FUNCTION lfwList
IF lcFilter='R' AND !llCanVald
  =lfRGWList()
  RETURN
ENDIF
IF RECCOUNT()=0
  SHOW GET pbSave DISABLE
ELSE
  SHOW GET pbSave ENABLE  
ENDIF

IF lsquery>0
  IF lsQuery<=RECCOUNT() &&GETBAR('PUQUERY',lsQuery)<=RECCOUNT()
      LOCATE FOR nfieldPos=lsquery &&GETBAR('PUQUERY',lsQuery)
    IF EMPTY(mlfield_nm)
      SHOW GET PBRemove DISABLE
    ELSE
      SHOW GET PBRemove ENABLE      
    ENDIF
  ELSE
   SHOW GET PBRemove DISABLE
  ENDIF  
ELSE
  SHOW GET PBRemove DISABLE
ENDIF
IF lsQuery>0
   LOCATE FOR nFieldPos=GETBAR('PUQUERY',lsQuery) 
  IF mlField_Nm='.OR.'
      SHOW GETS
      IF _WINDOWS
        SHOW GET INV[1,2],1 PROMPT ' '  
      ENDIF  
    =lfShowInv('DISABLE')    
    llNotOr=.F.     
  ELSE
    IF _WINDOWS
      lcPrompt=IIF(lisnot,'IS','IS NOT')
      SHOW GET INV[1,2],1 PROMPT lcPrompt  
    ENDIF  
   SHOW GETS     
  =lfShowInv('ENABLE')
  ENDIF 

ELSE
   SHOW GETS
   =lfShowInv('DISABLE')     
*   =IIF(_DOS,lfClear(),lfClearW())
   llNotOr=.F. 
ENDIF
IF CNTBAR('PUQUERY')>=1
   lnRecord=IIF(RECNO()>RECCOUNT(),0,RECNO())
   LOCATE FOR nfieldpos=GETBAR('PUQUERY',CNTBAR('PUQUERY'))  &&CNTBAR('PUQUERY')
  IF MlField_Nm = '.OR.'
    SHOW GET PBOr DISABLE
  ELSE
    SHOW GET PBOr ENABLE      
  ENDIF
  IF lnRecord<>0
    GO lnRecord
  ENDIF
ELSE  
  SHOW GET PBOr DISABLE  
ENDIF  



FUNCTION lfRGWList
IF lsQuery>0
   LOCATE FOR nFieldPos=GETBAR('PUQUERY',lsQuery) 
  IF mlField_Nm='.OR.'
      SHOW GETS
      IF _WINDOWS
        SHOW GET INV[1,2],1 PROMPT ' '  
      ENDIF  
    =lfShowInv('DISABLE')    
    llNotOr=.F.     
  ELSE
    IF _WINDOWS
      lcPrompt=IIF(lisnot,'IS','IS NOT')
      SHOW GET INV[1,2],1 PROMPT lcPrompt  
    ENDIF  
   SHOW GETS     
  =lfShowInv('DISABLE')
  ENDIF 

ELSE
   SHOW GETS
   =lfShowInv('DISABLE')     
*   =IIF(_DOS,lfClear(),lfClearW())
   llNotOr=.F. 
ENDIF
IF CNTBAR('PUQUERY')>=1
   lnRecord=IIF(RECNO()>RECCOUNT(),0,RECNO())
   LOCATE FOR nfieldpos=GETBAR('PUQUERY',CNTBAR('PUQUERY'))  &&CNTBAR('PUQUERY')
  IF MlField_Nm = '.OR.'
    SHOW GET PBOr DISABLE
  ELSE
    SHOW GET PBOr DISABLE      
  ENDIF
  IF lnRecord<>0
    GO lnRecord
  ENDIF
ELSE  
  SHOW GET PBOr DISABLE  
ENDIF  




*!*************************************************************************
*!
*!              Function: lfShowInv
*!
*!*************************************************************************
*  Function to control the invisible buttons Enabling or disabling
FUNCTION lfShowInv
PARAMETER lcStat,lnStart,lnEnd
lnStart = IIF(TYPE('lnStart')='N',lnStart,1)
lnEnd   = IIF(TYPE('lnEnd')='N',lnEnd,4)
DO CASE
 CASE _DOS
    FOR lnCount=lnStart to lnEnd
      SHOW GET INV[1,lnCount]  &lcStat
    ENDFOR
  CASE _WINDOWS
    FOR lnCount=lnStart to lnEnd
      SHOW GET INV[1,lnCount]  &lcStat
      SHOW GET PBDUM[lnCount]  &lcStat
    ENDFOR    
ENDCASE      


*!*************************************************************************
*!
*!              Function: lfvAdd
*!
*!*************************************************************************
*  Valid Function for the add push button to add a new condition to the list
FUNCTION lfvAdd
CLEAR TYPEAHEAD
lnLastSelect =lsQuery
llAdd=.T.
llIsAdded = .F.
*DEFINE BAR CNTBAR('PUQUERY')+1 OF PUQUERY PROMPT '   '
*lsQuery = CNTBAR('PUQUERY')
*lcLastVal=''
*SHOW GET lsQuery
=lfShowInv('DISABLE',2,4)
*=IIF(_DOS,lfClear(),lfClearw())
*IF _WINDOWS
*  =lfBox1()
*  llNotOr=.F.
*ENDIF

=lfShowInv('ENABLE',1,1)
KEYBOARD '{SPACEBAR}'
_CUROBJ=OBJNUM(INV[1,1])

*!*************************************************************************
*!
*!              Function: lfvOr
*!
*!*************************************************************************
*   valid function for the or push button  
FUNCTION lfvOr
PUSH KEY
ON KEY
lcprompt=REPLICATE(IIF(_DOS,'Í','_'),35)+' OR '+  REPLICATE(IIF(_DOS,'Í','_'),35)
DEFINE BAR CNTBAR('PUQUERY')+1 OF PUQUERY PROMPT lcPrompt
APPEND BLANK
REPLACE mlField_Nm WITH '.OR.',;
        nFieldPos WITH RECCOUNT() &&CNTBAR('puquery')+1
=lfarngCurs('PUQUERY')        
=lfPopFill('puquery',lcArray)
lsQuery=CNTBAR('PUQUERY') &&RECCOUNT()
POP KEY
_CUROBJ=OBJNUM(lsQuery)

*!*************************************************************************
*!
*!              Function: lfvRemove
*!
*!*************************************************************************
*  function to remove bar from the list
FUNCTION lfvRemove
RELEASE BAR GETBAR('PUQUERY',lsQuery) OF puquery
=lfarngCurs('puquery',.T.)
=lfpopfill('puquery')
IF RECCOUNT()=0
  lsQuery=0
ELSE 
  lsQuery=IIF(lsQuery<=RECCOUNT(),lsQuery,lsQuery-1)
ENDIF  
_CUROBJ=OBJNUM(lsQuery)




*!*************************************************************************
*!
*!              Function: lfvOK
*!
*!*************************************************************************
*  Valid function for the ok push button
FUNCTION lfvOk
llTerminat=.T.    && there is a need for terminating the screen
** return changes to the array passed to deal with
IF lcFilter='F'
 SELECT(lcCurDbf)
  lcGetFlt=ALLTRIM(gfGenFlt(lcArray,.T.))
  lccurFlt=ALLTRIM(FILTER())
  SET FILTER TO 
  IF !EMPTY(lcGetFlt)
    SET FILTER TO &lcGetFlt
  ENDIF
  lcGetFlt=FILTER()
  lnStrPos=RAT(UPPER(lcGetFlt),UPPER(lcCurFlt),1)-1
  lcCurFlt=IIF(lnStrPos=0,'',SUBSTR(lcCurFlt,1,IIF(lnStrPos>0,lnStrPos,LEN(lcCurFlt))))
  lcCurFlt=ALLTRIM(lcCurFlt)
    IF RIGHT(ALLTRIM(lcCurFlt),1)='.'
      lcRight='.AND.'
    ELSE
      lcRight='AND'  
    ENDIF
    IF RIGHT(ALLTRIM(lcCurFlt),LEN(lcRight))=lcRight
      lcCurFlt=LEFT(lcCurFlt,LEN(lcCurFlt)-LEN(lcRight))
    ENDIF
  SELECT(lcCurDbf)
  SET FILTER TO  
  IF !EMPTY(ALLTRIM(lcCurFlt))
     SET FILTER TO &lcCurFlt
  ENDIF
ENDIF  
SELECT TemCursor
=lfarngCurs('puquery')  && rearange the file throught the list
SELECT mlField_nm,cleftdes,clefttype,lisNot,coperator,mrfield_nm,crightdes;
       FROM TemCursor INTO ARRAY &lcArray
*lcSaveName=GETFILE('','Select File To Save To','Ok')
*IF !EMPTY(lcSaveName)
*  SELECT * FROM TemCursor INTO TABLE &lcSaveName
*ENDIF
** if the filter is empty show initialize the array with one row and clear it       
IF RECCOUNT('TEMCURSOR')>0       
 DIMENSION &lcArray[RECCOUNT('TEMCURSOR'),7]
ELSE
 DIMENSION &lcArray[1,7]
 STORE '' TO &lcArray
ENDIF 
=lfInitArr(lcArray)
IF lcFilter='F'
    SELECT(lcCurDbf)
    lcUsrDefFlt=gfGenFlt(lcArray,.T.)
    IF !EMPTY(lcUsrDefFlt)
       lcCurFlt=IIF(EMPTY(ALLTRIM(lcCurFlt)),ALLTRIM(lcUsrDefFlt),lcCurFlt+' AND '+;
                    ALLTRIM(lcUsrDefFlt))   
    ENDIF
    SET FILTER TO  
    IF !EMPTY(lcCurFlt)
      SET FILTER TO &lcCurFlt
    ENDIF
*   IF !EMPTY(lcUsedFlt) 
     lcCurFilter=lcUsedFlt
*   ENDIF
ENDIF
*!*************************************************************************
*!
*!              Function: lfvOptions
*!
*!*************************************************************************
*  valid function for the options push button
* take care that this push button is a terminate push button
* so we can keep using only one level of read
FUNCTION lfvOptions
IF lfarngCurs('PUQUERY')  && rearrange the file records through the list
 =lfPopFill('PUQUERY')
ENDIF
lcToProceed='DO FltOpts.SPR'
 
FUNCTION lfvSave
IF lfArngCurs('PUQUERY') 
  =lfPopFill('PUQUERY')
ENDIF
lcToProceed='DO FLTSAVE.PRG'
CLEAR READ

FUNCTION lfvLoading
IF lfArngCurs('PUQUERY') 
  =lfPopFill('PUQUERY')
ENDIF
lcToProceed='DO FLTLOAD.PRG'
CLEAR READ

*!*************************************************************************
*!
*!              Function: lfVldOper
*!
*!*************************************************************************
*   Function to chech if a specific operator is valid for a specific data type
FUNCTION lfVldOper
PARAMETERS lcOperator,lcType
llVldOper=.F.
llVldOper=(lcType $ laVldOper[ASCAN(laOper,lcOperator),1])
RETURN llVldOper




****************************  the left hand side functions *********************
*!*************************************************************************
*!
*!              Function: lfSelectfd
*!
*!*************************************************************************
*  This function used to return the selected value from an activated popup
* selected value will be stored in the variable defined its name in lcToChange
FUNCTION lfSelectfd
PARAMETERS lcPopName,lcSelect,lcToChange

IF lcSelect='< Expression... >'   && check if the selected bar is the get expression
 m.expr=''
  GETEXP TO m.Expr DEFAULT MLINE(mlField_Nm,1)
  lcType='E' 
  IF !EMPTY(m.Expr) AND M.EXPR<>MLFIELD_NM
    lcSelect=m.expr
  ELSE
    lcSelect=mlField_Nm
    lcType=cleftdes
  ENDIF  
ELSE
  lcType='F'  && the selected bar is a field
ENDIF 
IF llAdd  && check if we are in the add mode
  IF !EMPTY(lcSelect)
    llIsAdded=.T.
    APPEND BLANK
    REPLACE cOperator WITH 'Like',;
            lIsNot WITH .T.,;
            nFieldPos WITH CNTBAR('PUQUERY');
            crightdes WITH 'V';
            cleftdes WITH lcType           
  ENDIF          
ENDIF

IF !EMPTY(lcSelect)
  IF lcSelect<>lcLastVaL   && check if there is modifications happend or not
    lcSelect=IIF(lcType='F',lfTrnsField('laField_H','laField_N',lcSelect,'H'),lcSelect)
    REPLACE &lcToChange WITH lcSelect,;
          cLeftDes WITH lcType,;
          mHeader WITH IIF(lcType='E',lfTrnsExpr('laField_H','laField_N',lcSelect,'N'),;
          lfTrnsField('laField_H','laField_N',lcSelect,'N'))
    DO CASE
      CASE lcType='F'
        DIMENSION laFieldDes[1,7]
        lcfieldDes = 'cData_Typ,nFld_Wdth,nFld_dec,MVAld_Str,cPict_str,';
                     +'lVldentry,mventries' 
        lcGather ='cLeftType,nWidth,ndecimal,MVAld_Str,cPict_str,';
                    +'lVldentry,mventries'     
*lfFieldDes(MLINE(mlField_Nm,1),lcfieldDes,'laFieldDes')                            
        IF lfFieldDes(mlField_Nm,lcfieldDes,'laFieldDes')        
               laFieldDes[1,7]=IIF(laFieldDes[1,1]='L','Yes'+lcElmSep+'No'+lcVldSep+'.T.'+;
                                   lcElmSep+'.F.',laFieldDes[1,7])
                       REPLACE cOperator WITH IIF(laFieldDes[1,6] OR !EMPTY(laFieldDes[1,7]) ;
                               AND !INLIST(cOperator,'Like','In List'),'Like',cOperator)
                       REPLACE coperator WITH IIF(lfVldOper(coperator,laFieldDes[1,1]) ;
                       AND lfVldOper(coperator,cLeftType),coperator,'Like'),;
                       MRightHead WITH '',;
                       MRField_Nm WITH '';
                       cRightDes WITH 'V'
           GATHER FIELDS &lcGather FROM laFieldDes MEMO  
        ENDIF                  
      CASE lcType='E'
    ENDCASE             
  ENDIF
ELSE
  llIsAdded=.F.  
ENDIF
DEACTIVATE POPUP (lcPopName)

*!*************************************************************************
*!
*!              Function: lfActPop
*!
*!*************************************************************************
* This function is to activate a popup and seek for the selected
* value in the variable stored in lcToChange and define the in selection
* of this popup it will use the previous function
FUNCTION lfActPop
PARAMETERS lcPopName,lnRowPos,lnColPos,lcBars

lcToChange='mlField_Nm'
IF !llAdd
  IF cleftdes='F'
    lnBarNo=ASCAN(&lcBars,MLINE(&lcTochange,1))
  ELSE
    lnBarNo=CNTBAR(lcpopname)  
  ENDIF  
ELSE
DEFINE BAR CNTBAR('PUQUERY')+1 OF PUQUERY PROMPT '   '
lsQuery = CNTBAR('PUQUERY')
lcLastVal=''
SHOW GET lsQuery
 lnBarNo=1  
ENDIF  
llIsAdded=.F.
ON SELECTION POPUP (lcPopName) DO lfSelectfd with lcPopName,PROMPT(),lcTochange
ACTIVATE POPUP (lcPopName) AT lnRowPos,lnColPos BAR IIF(lnBarNo>0,lnBarNo,1)
DEACTIVATE POPUP (lcPopName)
IF !llAdd
  IF lfarngCurs('PUQUERY')        
    =lfPopFill('puquery',lcArray)          
  ENDIF  
  =lfChangeBar(lsQuery,'puquery')
ELSE
  IF !llIsAdded
    RELEASE BAR CNTBAR('puquery') OF PUQUERY
    =lfarngCurs('PUQUERY')
    =lfPopFill('PUQUERY')
    lsQuery=lnLastSelect
  ELSE
    IF lfarngCurs('PUQUERY')        
      =lfPopFill('puquery',lcArray)          
    ENDIF    
    =lfChangeBar(lsQuery,'puquery')     
  ENDIF  
ENDIF  
SHOW GETS
STORE .F. TO llAdd,llIsAdded
_CUROBJ=OBJNUM(lsQuery)

**************************************************************************


****************************  the Get is/isnot functions *********************
*!*************************************************************************
*!
*!              Function: lfIsNot
*!
*!*************************************************************************
*  this function is a valid function to change the IS to IS NOT and vica
* for the dos version
FUNCTION lfIsNot
lcToChange='lIsNot'
REPLACE &lcToChange WITH !&lcToChange
IF lfarngCurs('PUQUERY')        
  =lfPopFill('puquery',lcArray)          
ENDIF  
=lfChangeBar(lsQuery,'puquery')  
_CUROBJ=OBJNUM(lsQuery)


*!*************************************************************************
*!
*!              Function: lfIsNotW
*!
*!*************************************************************************
*  this function is a valid function to change the IS to IS NOT and vica
* for Windows Version
FUNCTION lfVINNotW
lcToChange='lIsNot'
REPLACE &lcToChange WITH !&lcToChange
IF lfarngCurs('PUQUERY')        
  =lfPopFill('puquery',lcArray)          
ENDIF  
=lfChangeBar(lsQuery,'puquery')  
lcPrompt=IIF(lisnot,'IS','IS NOT')
SHOW GET INV[1,2],1 PROMPT lcPrompt
_CUROBJ=OBJNUM(lsQuery)  
*************************************************************************


****************************  the Get Operator functions *********************
*!*************************************************************************
*!
*!              Function: lfSelect
*!
*!*************************************************************************
*  This function used to return the selected value from an activated popup
* selected value will be stored in the variable defined its name in lcToChange
FUNCTION lfSelect
PARAMETERS lcPopName,lcSelect,lcToChange
IF lcSelect<>lcLastVal
  IF (coperator='In List') OR ( cOperator='Between' AND lcSelect<>'In List') OR;
    (!INLIST(cOperator,'Between','In List') AND INLIST(lcSelect,'Between','In List');
       AND cRightDes='F')
    REPLACE MRightHead WITH '',;
            MRField_Nm WITH '';
            cRightDes WITH 'V'
  ENDIF
     REPLACE &lcToChange WITH lcSelect
ENDIF  
DEACTIVATE POPUP (lcPopName)


*!*************************************************************************
*!
*!              Function: lfGetOper
*!
*!*************************************************************************
*
FUNCTION lfGetOper
PARAMETERS lcPopName,lnRowPos,lnColPos,lcBars
lcToChange='coperator'
PUSH POPUP (lcPopName)
SET SKIP OF POPUP (lcPopName) .f.

IF !lvldentry AND Empty(mventries)
  FOR lnCount=1 TO CNTBAR(lcPopName)
    IF !(CLEFTTYPE $ laVldOper[lnCount])
       RELEASE BAR lnCount of (lcPopName)
    ENDIF
  ENDFOR
ELSE
  FOR lnCount=2 TO CNTBAR(lcPopName)-IIF(cLeftType='L',0,1)
    RELEASE BAR lnCount of (lcPopName)  
  ENDFOR
ENDIF  
  lnBarNo=ASCAN(&lcBars,MLINE(&lcTochange,1))    

*IF (!lvldentry AND Empty(mventries) AND lcFilter='F') OR (lcFilter<>'F')
  ON SELECTION POPUP (lcPopName) DO lfSelect with lcPopName,PROMPT(),lcTochange
  ACTIVATE POPUP (lcPopName) AT lnRowPos,lnColPos BAR IIF(lnBarNo>0,lnBarNo,1)
  DEACTIVATE POPUP (lcPopName)
*ENDIF  
POP POPUP (lcPopName)
IF lfarngCurs('PUQUERY')        
   =lfPopFill('puquery',lcArray)          
ENDIF  
=lfChangeBar(lsQuery,'puquery')
_CUROBJ=OBJNUM(lsQuery)
*************************************************************************



****************************  the right hand functions *********************
*!*************************************************************************
*!
*!              Function: lfSelectRgt
*!
*!*************************************************************************
*
FUNCTION lfSelectRgt
PARAMETERS lcPopName,lcSelect,lcToChange,lcDes
IF lcSelect<>lcLastVal
  REPLACE cRightDes WITH lcDes ,;
          mRField_NM WITH lfTrnsField('laField_H','laField_N',lcSelect,'H'),;
          mRightHead WITH lcSelect
ENDIF
DEACTIVATE POPUP (lcPopName)

*!*************************************************************************
*!
*!              Function: lfGetVal
*!
*!*************************************************************************
*
FUNCTION lfGetVal
PARAMETERS lcPopName,lcSelect,lcToChange,lnRow,lnCol
DO CASE
  CASE lcSelect='Value' 
      lcToProceed='DO lfdoval WITH '+ALLTRIM(STR(lnRow))+','+ALLTRIM(STR(lnCol))
      CLEAR READ
  CASE lcSelect='Another Data Item' OR (lcSelect='Expression' AND lcFilter='R')
     DEFINE POPUP puRight FROM 1,1 TO IIF(_DOS,10,15),31 SCROLL
     =lfFillPop('puright','laField_H')
      lnBarNo=0
     IF cLeftDes='F'
       lnBarNo=ASCAN(laField_N,MLINE(mlField_Nm,1))      
     ENDIF  
     SET SKIP OF BAR lnBarNo OF puRight .T.
     IF cRightdes $ 'FE'
       lnBarNo=ASCAN(laField_N,MLINE(mrField_Nm,1))
     ENDIF       
     DIMENSION laDesc[1]
     FOR lnCount=1 TO ALEN(laField_H,1)
       =lfFieldDes(laField_N[lnCount,1],'cData_Typ','laDesc')
       IF (laDesc[1]<>cLeftType) 
         SET SKIP OF BAR lnCount OF puRight .T.
       ENDIF        
     ENDFOR
     ON SELECTION POPUP puright DO lfSelectRgt with 'puright',PROMPT(),'mRField_Nm',;
           IIF(lcSelect='Another Data Item','F','E')
     ACTIVATE POPUP puRight AT lnRow,lnCol BAR IIF(lnBarNo>0,lnBarNo,1)
     DEACTIVATE POPUP puright
     RELEASE POPUP puright
  CASE lcSelect='Expression' AND lcFilter='E'
    *lcExp,lcFile,lcExpPrmpt,la_Field,laField,llAddAlias,lcType,laFiles,la_Var    

    lcExpr = GetExpr(mRField_Nm,'SYDFLFLD';
                        ,'Set Report Expression','laField_H',@laField_N,.f.,'U',.f.)

      REPLACE cRightDes WITH 'E' ,;
              mRField_NM WITH lcExpr ,;
              mRightHead WITH lcExpr 
     
ENDCASE
DEACTIVATE POPUP (lcPopName)

*!*************************************************************************
*!
*!              Function: lfGetRight
*!
*!*************************************************************************
*  to activate the popup Value,Another data item,expression
FUNCTION lfGetRight
PARAMETERS lcPopName,lnRowPos,lnColPos
lcToChange = 'mRightHead'  
IF lcFilter<>'O'    
  IF (lvldentry OR !EMPTY(mvEntries)  OR ;    
     (INLIST(cOperator,'Between','In List')) AND lcFilter='F') 
     lcToProceed='DO lfdoval WITH '+ALLTRIM(STR(lnRowPos))+','+ALLTRIM(STR(lnColPos))
      CLEAR READ     
  ELSE
    IF lcFilter='F'
     lcToProceed='DO lfdoval WITH '+ALLTRIM(STR(lnRowPos))+','+ALLTRIM(STR(lnColPos))
      CLEAR READ
    ELSE
    PUSH POPUP (lcPopName)
    IF INLIST(cOperator,'Between','In List') AND (lcfilter='F')
      RELEASE BAR 2 OF (lcPopName)
    ENDIF
    lnBarNo=ATC(cRightDes,lcRightHand)
    ON SELECTION POPUP (lcPopName) DO lfGetVal with lcPopName,PROMPT(),lcTochange,lnRowPos,lnColPos
    ACTIVATE POPUP (lcPopName) AT lnRowPos,lnColPos BAR IIF(lnBarNo>0,lnBarNo,1)
    DEACTIVATE POPUP (lcPopName)
    POP POPUP (lcPopName)
  ENDIF
  ENDIF
  IF lfarngCurs('PUQUERY')        
    =lfPopFill('puquery',lcArray)          
  ENDIF  
  =lfChangeBar(lsQuery,'puquery')
  _CUROBJ=OBJNUM(lsQuery)
ENDIF  
*************************************************************************



*!*************************************************************************
*!
*!              Function: lfField
*!
*!*************************************************************************
*   function used to display the values of the selected bar of the list
FUNCTION lfField  
PARAMETERS lnPos
lcReturn = ''
IF lsQuery>0
  IF mlfield_Nm='.OR.'
    RETURN IIF(lnPos=3,PADC(' OR ',16,IIF(_WINDOWS,' ',' '));
               ,PADC('',25,IIF(_WINDOWS,' ',' ')))
  ENDIF    
  DO CASE
    CASE lnPos=1
      lcReturn = IIF(llAdd,SPACE(23)+IIF(_DOS,'',''),;
                 PADR(SUBSTR(mHeader,1,23),23)+IIF(_DOS,'',''))
    CASE lnPos=2
      lcReturn = IIF(llAdd,SPACE(10),IIF(lisnot,'IS','IS NOT'))  
    CASE lnPos=3
      lcReturn = IIF(llAdd,SPACE(15),PADR(SUBSTR(coperator,1,15),15)+IIF(_DOS,'',''))
    CASE lnPos=4
      lcReturn = IIF(llAdd,SPACE(23),STRTRAN(PADR(SUBSTR(mrighthead,1,22),22),lcElmSep,',')+IIF(_DOS,'',''))
  ENDCASE
ENDIF
RETURN lcReturn



*!*************************************************************************
*!
*!              Function: lfChangeBar
*!
*!*************************************************************************
* FUNCTION to change the prompt of a bar from the list when needed
FUNCTION lfChangeBar
PARAMETERS lnCount,lcPopName
  LOCATE FOR nFieldPos=lnCount
  IF !EMPTY(MLFIELD_Nm)
     lcPrompt=''
   IF mlField_Nm<>'.OR.'  
    lcPrompt=PADR(SUBSTR(mheader,1,22),22)+PADR(IIF(lisnot,' IS ',' IS Not '),8)+;
             PADR(COPERATOR,16)+STRTRAN(SUBSTR(mRightHead,1,25),lcElmSep,',') 
  ELSE
    lcprompt=REPLICATE(IIF(_DOS,'Í','_'),35)+' OR '+  REPLICATE(IIF(_DOS,'Í','_'),35)
  ENDIF
      DEFINE BAR lnCount OF (lcPopName) PROMPT lcPrompt
  ENDIF    

*!*************************************************************************
*!
*!              Function: lfFillFill
*!
*!*************************************************************************
*  fenctaof to fill the cursor whith the array pas{ed to the program
FUNCTION lfFillFile
PARAMETERS lcArray,lnBegin
PRIVATE lcSelect
lnBegin=IIF(TYPE('lnBegin')='N',lnBegin,0)
DIMENSION laTempArr[1,ALEN(&lcArray,2)],laFieldDes[1,7],laVldEnt[1]
lcfieldDes = 'cfld_Head,nFld_Wdth,nFld_dec,MVAld_Str,cPict_str,';
              +'lVldentry,mventries' 
lcGather ='mHeader,nWidth,ndecimal,MVAld_Str,cPict_str,';
              +'lVldentry,mventries' 

lcSelect = SELECT()
FOR lnCount = 1 TO ALEN(&lcArray,1)
  IF !EMPTY(&lcArray[lnCount,1])
    APPEND BLANK
    =ACOPY(&lcArray,laTempArr,AELEMENT(&lcArray,lnCount,1),ALEN(&lcArray,2))
    GATHER FIELDS &lcFields FROM laTempArr MEMO
    REPLACE nfieldPos WITH lnCount+lnBegin
    lcToSeek=MLINE(mlField_Nm,1)
    DO CASE 
      CASE cLeftDes='F'  && if the left hand side is a fields
        IF lfFieldDes(lcToSeek,lcfieldDes,'laFieldDes') && get this field information       
          laFieldDes[1,1]=IIF(EMPTY(ALLTRIM(laFieldDes[1,1])),lcToSeek,;
          IIF(lcFilter='R' OR lcFilter='E',IIF(ATC('.',lcToSeek)>0,SUBSTR(lcToSeek,1,ATC('.',lcToSeek)),''),'');
          +laFieldDes[1,1])        
          
          *E300869,1 Add this line to allow expressions in filter option
          *headers [Begin]
          
          *E300869,1 If this header is an expression
          IF LEFT(laFieldDes[1,1] , 1) = '~'
            laFieldDes[1,1] = EVALUATE(ALLTRIM(SUBSTR(laFieldDes[1,1] , 2)))
          ENDIF    && End of IF LEFT(laFieldDes[1,1] , 1) = '~'
          *E300869,1 Add this line to allow expressions [End]
          
          GATHER FIELDS &lcGather FROM laFieldDes MEMO  && store the field information
        ENDIF  
      CASE cLeftDes='E'  && if the left hand side is an expression
        REPLACE MHEADER WITH lfTrnsExpr('laField_H','laField_N',mlField_Nm,'N')                         
    ENDCASE     
    DO CASE    && this part to validate the right hand side
      CASE cRightDes='V'  && if it is a value
        REPLACE MRIGHTHEAD WITH lfTrnsValue(lcToSeek,mRField_Nm,'N')
      CASE cRightDes='E'   && if it is an expression
        REPLACE MRIGHTHEAD WITH lfTrnsExpr('laField_H','laField_N',mRField_Nm,'N')                
      CASE cRightDes='F'   && if it is another data item
        REPLACE MRIGHTHEAD WITH lfTrnsField('laField_H','laField_N',mRField_Nm,'N')          
    ENDCASE  
  ENDIF  
ENDFOR
SELECT (lcSelect)


*!*************************************************************************
*!
*!              Function: lfFieldDes
*!
*!*************************************************************************
* FUNCTION to get information about a specific field from the dictionary
FUNCTION lfFieldDes
PARAMETERS lcToSeek,lcFieldDes,lcFieldSct

PRIVATE lcSelect
lcSelect=SELECT()  
llFound=.F.
llSeek=.F.
IF lcFilter $ 'ROE'
  SELECT(lcFileToUse) 
  SET ORDER TO 1
  IF SEEK(ALLTRIM(lcRepID))
    *E037351,1 HBG 23/12/2003 If this file is SYREPUVR Locate for records belong to Aria27 only [Begin]
    IF UPPER(lcFileToUse) = 'SYREPUVR'
      LOCATE WHILE cRep_ID=lcRepID FOR (UPPER(ALLTRIM(mFld_name))=UPPER(ALLTRIM(lcToSeek)) AND (EMPTY(CVER) OR CVER = 'A27'))
    ELSE
    *E037351,1 [End]
      LOCATE WHILE cRep_ID=lcRepID FOR (UPPER(ALLTRIM(mFld_name))=UPPER(ALLTRIM(lcToSeek)))
    *E037351,1 HBG End  If this file is SYREPUVR [Begin]
    ENDIF
    *E037351,1 [End]  
    llFound=FOUND()
  ENDIF
ENDIF  
IF !llFound
  lcToSeek=IIF(ATC('.',lcToSeek)>0,SUBSTR(lcToSeek,ATC('.',lcToSeek)+1),lcToSeek)
  lcToSeek=PADR(lcToSeek,10)
   SELECT SYDFIELD
   lcFldFlt=FILTER()
   SET FILTER TO
 SET ORDER TO 1
 IF SEEK(UPPER(lcToSeek))
    SCATTER FIELDS &lcFieldDes TO &lcFieldSct MEMO
    llSeek=.T.
 ENDIF 
 SET FILTER TO &lcFldFlt
ELSE
  SCATTER FIELDS &lcFieldDes TO &lcFieldSct MEMO 
ENDIF 
SELECT(lcSelect)  
 RETURN llSeek OR llFound

*!*************************************************************************
*!
*!              Function: lfTrnsExpr
*!
*!*************************************************************************
*  FUNCTION to translate an expression from an english to fox expressions
* and vica versa using the dictionary . it change all the fields in the 
* expression to its header or change all the headers to the corrisponding
* field name acording to the lcDirction variable
FUNCTION lfTrnsExpr
PARAMETERS lcField_N,lcField_H,lcExprsion,lcDirction
PRIVATE lcSelect,lnRecno
lcDirction=IIF(TYPE('lcDirction')$'LUN','N',lcDirction)
lcUsedArr=IIF(lcDirction='N',lcField_H,lcField_N)
lcTrnsArr=IIF(lcDirction='N',lcField_N,lcField_H)
lcTrnsStr=lcExprsion
FOR lnArrElment = 1 TO ALEN(&lcUsedArr,1)
  IF ALLTRIM(&lcUsedArr[lnArrElment,1]) $ lcTrnsStr
    FOR lnPos= 1 TO OCCURS(ALLTRIM(&lcUsedArr[lnArrElment,1]),lcExprsion)
      lnCharPos=1
      lnDec=1
      DO WHILE SUBSTR(lcTrnsStr,ATC(ALLTRIM(&lcUsedArr[lnArrElment,1]),lcTrnsStr)-lnDec,1)=' ';
               AND (ATC(ALLTRIM(&lcUsedArr[lnArrElment,1]),lcTrnsStr)-lnDec)!=0
       lnDec=lnDec+1
      ENDDO  
      IF !( SUBSTR(lcTrnsStr,ATC(ALLTRIM(&lcUsedArr[lnArrElment,1]),lcTrnsStr)-lnDec,1)$ "'"+'"')
        lcTrnsStr = STRTRAN(lcTrnsStr,ALLTRIM(&lcUsedArr[lnArrElment,1]),;
                           ALLTRIM(&lcTrnsArr[lnArrElment,1]),1,1) 
      ENDIF                     
    ENDFOR                         
  ENDIF
ENDFOR

RETURN lcTrnsStr





*!*************************************************************************
*!
*!              Function: lfTrnsField
*!
*!*************************************************************************
*  FUNCTION to translate a field from a field to its header
* and vica versa using the dictionary .
FUNCTION lfTrnsField
PARAMETERS lcField_N,lcField_H,lcExprsion,lcDirction
lcDirction=IIF(TYPE('lcDirction')$'LUN','N',lcDirction)
lcUsedArr=IIF(lcDirction='N',lcField_H,lcField_N)
lcTrnsArr=IIF(lcDirction='N',lcField_N,lcField_H)
*lcTrnsStr=IIF(lcDirction='N',UPPER(lcExprsion),lcExprsion)
lcTrnsStr=IIF(lcDirction='N',lcExprsion,lcExprsion)
lnArrPos = ASCAN(&lcUsedArr,lcTrnsStr)
IF lnArrPos>0
  lnArrPos=ASUBSCRIPT(&lcUsedArr,lnArrPos,1)
  IF lnArrPos>0
    *B038612,1 MHM 10/13/2004 Fix the bug of not filtering correct [Start]
    *lcTrnsStr=&lcTrnsArr[lnArrPos,1]
    lcTrnsStr=&lcTrnsArr[lnArrPos]
    *B038612,1 MHM [End]
  ENDIF  
ENDIF  
RETURN lcTrnsStr


*!*************************************************************************
*!
*!              Function: lfTrnsValue
*!
*!*************************************************************************
*  a funnction to get the value of a specific field it check if the
* field have valid entries or valid codes and get the corrisponding
* valid values to this field from the codes file or from the field 
* dictionary and return the Values or the discreption acording to
* the direction 
FUNCTION lfTrnsValue
PARAMETERS lcFieldNam,lcValue,lcDirection
PRIVATE lcSelect,lnRecno,laVldEnt

DECLARE lavldEnt[1]
PRIVATE lcSelect
lcSelect=SELECT()  
llFound=.F.
llSeek=.F.
IF lcFilter $ 'ROE'
  SELECT(lcFileToUse) 
  SET ORDER TO 1
  IF SEEK(lcRepID)
    *E037351,1 HBG 23/12/2003 If this file is SYREPUVR Locate for records belong to Aria27 only [Begin]
    IF UPPER(lcFileToUse) = 'SYREPUVR'
      LOCATE WHILE cRep_ID=lcRepID FOR (mFld_name=lcfieldName AND (EMPTY(CVER) OR CVER = 'A27'))
    ELSE
    *E037351,1 [End]
      LOCATE WHILE cRep_ID=lcRepID FOR mFld_name=lcfieldName
    *E037351,1 HBG 23/12/2003 End If this file is SYREPUVR [Begin]
    ENDIF
    *E037351,1 [End] 
    llFound=FOUND()
  ENDIF
ENDIF  
lcDirction=IIF(TYPE('lcDirction')$'LUN','N',lcDirction)
lcRetVal=lcValue
*lcSelect=SELECT()
lnRecNO=0
IF !llFound
  SELECT SYDFIELD 
  SET ORDER TO 1
  lcFieldNam=IIF(ATC('.',lcFieldNam)>0,SUBSTR(lcFieldNam,ATC('.',lcFieldNam)+1),lcFieldNam)
  lcFieldNam=PADR(lcFieldNam,10)
  lnRecNo=IIF(RECNO()>RECCOUNT(),0,RECNO())
  llFound=SEEK(lcFieldNam)
ENDIF
IF llFound  
  llVldEnt   = lVldEntry
  lcVldEntry = IIF(cData_Typ='L','Yes'+lcElmSep+'No'+lcVldSep+'.T.'+lcElmSep+'.F.',ALLTRIM(mVEntries))
  IF lnRecNo<>0
    GO lnRecNO
  ENDIF
  IF llVldEnt
     =gfSubStr(lcValue,@lavldEnt,lcElmSep)
     SELECT codes
     lnRecNo=IIF(RECNO()>RECCOUNT(),0,RECNO())
     *B802649,1 AMM Set the apropriate index
     *lnOrderNo=IIF(lcDirection='N',2,1)
     *SET ORDER TO lnOrderNo
     SET ORDER TO CCODE_NO  
     lcFSetEx = SET('EXACT')
     SET EXACT OFF
     *B802649,1 AMM end
     lcRetVal=''
     FOR lnVldStr= 1 TO ALEN(lavldEnt)
       *B802649,1 AMM Adjust the seek condition
       *IF SEEK (lcFieldNam+PADR(laVldEnt[lnVldStr],IIF(lcDirection='N',LEN(cCode_No),LEN(CDISCREP))))
       IF lcDirection='N'
         =SEEK ('N'+lcFieldNam+PADR(laVldEnt[lnVldStr],LEN(cCode_No)) )
       ELSE
         =SEEK ('N'+lcFieldNam)
         LOCATE REST WHILE CDEFCODE+CFLD_NAME='N'+lcFieldNam FOR CDISCREP = PADR(laVldEnt[lnVldStr],LEN(CDISCREP))
       ENDIF
       IF !EOF()
       *B802649,1 AMM end
         lcRetVal=lcRetVal+IIF(lnVldStr>1,lcElmSep,'')+;
         IIF(lcDirection='N',ALLTRIM(cdiscrep),ALLTRIM(cCode_No))
       ENDIF
     ENDFOR 
     *B802649,1 AMM Restore old exact setting
     SET EXACT &lcFSetEx
     *B802649,1 AMM end
     IF lnRecNo<>0
       GO lnRecNO
     ENDIF     
  ELSE
    IF !EMPTY(lcVldEntry)
      DIMENSION laVldEnt[1,1],laVldVal[1,1],laSerchVal[1,1]   
       =gfSubStr(SUBSTR(lcVldEntry,1,ATC(lcVldSep,lcVldEntry)-1),@laVldEnt,lcElmSep)
       =gfSubStr(SUBSTR(lcVldEntry,ATC(lcVldSep,lcVldEntry)+1),@laVldVal,lcElmSep)
       =gfSubStr(lcValue,@laSerchVal,lcElmSep)
       DIMENSION laVldEnt[ALEN(laVldEnt,1),1]
       DIMENSION laVldVal[ALEN(laVldVal,1),1]
       DIMENSION laSerchVal[ALEN(laSerchVal,1),1]       
       lcRetVal=''
       FOR lnVldStr=1 TO ALEN(laSerchVal,1)
         lnArrPos=IIF(lcDirection='N',ASCAN(laVldVal,laSerchVal[lnVldStr,1]),;
                      ASCAN(laVldEnt,laSerchVal[lnVldStr,1]))   
         IF lnArrPos>0             
           lnArrPos=IIF(lcDirection='N',ASUBSCRIPT(laVldVal,lnArrPos,1),;
                        ASUBSCRIPT(laVldEnt,lnArrPos,1))
           IF lnArrPos<>0             
             lcRetVal=lcRetVal+IIF(lnVldStr>1,lcElmSep,'')+;
               IIF(lcDirection='N',ALLTRIM(laVldEnt[lnArrPos,1]),;
                   ALLTRIM(laVldVal[lnArrPos,1]))
           ENDIF      
         ENDIF  
       ENDFOR
    ENDIF
  ENDIF
ELSE
  lcRetVal=''
ENDIF
SELECT (lcSelect)

RETURN lcRetVal

*!*************************************************************************
*!
*!              Function: lfTrnsValue
*!
*!*************************************************************************
*  a funnction to get the value of a specific field it check if the
* field have valid entries or valid codes and get the corrisponding
* valid values to this field from the codes file or from the field 
* dictionary and return the Values or the discreption acording to
* the direction 
FUNCTION lfTrns1Value
PARAMETERS lcFieldNam,lcValue,lcDirection
PRIVATE lcSelect,lnRecno,laVldEnt
DECLARE lavldEnt[1]
lcDirction=IIF(TYPE('lcDirction')$'LUN','N',lcDirction)
lcRetVal=lcValue
lcSelect=SELECT()
SELECT SYDFIELD 
SET ORDER TO 1
lcFieldNam=IIF(ATC('.',lcFieldNam)>0,SUBSTR(lcFieldNam,ATC('.',lcFieldNam)+1),lcFieldNam)
lcFieldNam=PADR(lcFieldNam,10)
lnRecNo=IIF(RECNO()>RECCOUNT(),0,RECNO())
IF SEEK(lcFieldNam)
  llVldEnt   = lVldEntry
  lcVldEntry = IIF(cData_Typ='L','Yes'+lcElmSep+'No'+lcVldSep+'.T.'+lcElmSep+'.F.',ALLTRIM(mVEntries))
  IF lnRecNo<>0
    GO lnRecNO
  ENDIF
  IF llVldEnt
     =gfSubStr(lcValue,@lavldEnt,lcElmSep)
     SELECT codes
     lnRecNo=IIF(RECNO()>RECCOUNT(),0,RECNO())
     lnOrderNo=IIF(lcDirection='N',2,1)
     SET ORDER TO lnOrderNo
     lcRetVal=''
     FOR lnVldStr= 1 TO ALEN(lavldEnt)
       IF SEEK (lcFieldNam+PADR(laVldEnt[lnVldStr],IIF(lcDirection='N',LEN(cCode_No),LEN(CDISCREP))))
         lcRetVal=lcRetVal+IIF(lnVldStr>1,lcElmSep,'')+;
         IIF(lcDirection='N',ALLTRIM(cdiscrep),ALLTRIM(cCode_No))
       ENDIF
     ENDFOR 
     IF lnRecNo<>0
       GO lnRecNO
     ENDIF     
  ELSE
    IF !EMPTY(lcVldEntry)
      DIMENSION laVldEnt[1,1],laVldVal[1,1],laSerchVal[1,1]   
       =gfSubStr(SUBSTR(lcVldEntry,1,ATC(lcVldSep,lcVldEntry)-1),@laVldEnt,lcElmSep)
       =gfSubStr(SUBSTR(lcVldEntry,ATC(lcVldSep,lcVldEntry)+1),@laVldVal,lcElmSep)
       =gfSubStr(lcValue,@laSerchVal,lcElmSep)
       DIMENSION laVldEnt[ALEN(laVldEnt,1),1]
       DIMENSION laVldVal[ALEN(laVldVal,1),1]
       DIMENSION laSerchVal[ALEN(laSerchVal,1),1]       
       lcRetVal=''
       FOR lnVldStr=1 TO ALEN(laSerchVal,1)
         lnArrPos=IIF(lcDirection='N',ASCAN(laVldVal,laSerchVal[lnVldStr,1]),;
                      ASCAN(laVldEnt,laSerchVal[lnVldStr,1]))   
         IF lnArrPos>0             
           lnArrPos=IIF(lcDirection='N',ASUBSCRIPT(laVldVal,lnArrPos,1),;
                        ASUBSCRIPT(laVldEnt,lnArrPos,1))
           IF lnArrPos<>0             
             lcRetVal=lcRetVal+IIF(lnVldStr>1,lcElmSep,'')+;
               IIF(lcDirection='N',ALLTRIM(laVldEnt[lnArrPos,1]),;
                   ALLTRIM(laVldVal[lnArrPos,1]))
           ENDIF      
         ENDIF  
       ENDFOR
    ENDIF
  ENDIF
ELSE
  lcRetVal=''
ENDIF
SELECT (lcSelect)

RETURN lcRetVal


*********************************************************************





*!*************************************************************************
*!
*!              Function: lfGetQuery
*!
*!*************************************************************************
*  this function to return a memo variable contain the filter generated
* using the headers "in English language" to display it the user
FUNCTION lfGetQuery
lcQuery=''
lcLineFeed=CHR(10)+CHR(13)   &&CHR(32) &&CHR(10)+CHR(13)
GO TOP
SCAN WHILE mlField_Nm='.OR.' 
ENDSCAN
lnRecStart=IIF(RECNO()>RECCOUNT(),0,RECNO())
GO BOTTOM
DO WHILE mlField_Nm='.OR.' AND !BOF()
  IF !BOF()
    SKIP -1
  ENDIF  
ENDDO 
lnRecEnd=IIF(RECNO()>RECCOUNT(),0,RECNO())
IF lnRecStart<>0
  GO lnRecStart
ENDIF
lnCount=0
IF lnRecStart=0
  RETURN ''
ENDIF
IF lnRecEnd=RECCOUNT() 
  IF mlField_Nm='.OR.'
    RETURN ''
  ENDIF
ENDIF
SCAN WHILE !EOF() AND RECNO()<=lnRecEnd
  IF mlField_Nm<>'.OR.'
    lcQuery=lcQuery+ALLTRIM(mHeader)+' '+IIF(lIsNot,'IS ','IS NOT ')+ALLTRIM(COPERATOR)+' '+;
      lfrightGet()+lcElmSep+' .AND. '+lcElmSep
  ELSE
lcQuery= IIF(RIGHT(lcQuery,9)=lcElmSep+' .AND. '+lcElmSep,SUBSTR(lcQuery,1,LEN(lcQuery)-9),lcQuery)
    IF lnCount>0
     lcQuery=lcQuery+' ) '
    ENDIF    
    lcQuery=ALLTRIM(lcQuery)+' '+lcElmSep+' OR '+lcElmSep+'( '
     lnCount=1
  ENDIF
ENDSCAN
lcQuery= IIF(RIGHT(lcQuery,9)=lcElmSep+' .AND. '+lcElmSep,SUBSTR(lcQuery,1,LEN(lcQuery)-9),lcQuery)
 IF lnCount>0
   lcQuery=lcQuery+' ) '
 ENDIF    
 lcQuery=STRTRAN(lcQuery,lcElmSep+' .AND. '+lcElmSep,lcLineFeed+' AND '+lcLineFeed)
 lcQuery=STRTRAN(lcQuery,lcElmSep+' OR '+lcElmSep,lcLineFeed+' OR '+lcLineFeed) 
RETURN lcQuery


*!*************************************************************************
*!
*!              Function: lfRightGet
*!
*!*************************************************************************
* to get the right hand side for any line of the list
FUNCTION lfRightGet
lcRetVal=mRightHead
DO CASE
  CASE cRightDes='V'
    DO CASE
      CASE cLeftType $ 'CM'
            IF INLIST(COPERATOR,'Between','In List')
               lcRetVal='"'+STRTRAN(ALLTRIM(mRightHead),lcElmSep,'",'+'"')+'"'
            ELSE
               RETURN '"'+ALLTRIM(mrightHead)+'"'   
            ENDIF
      CASE cLeftType = 'N'
         lcRetVal=STRTRAN(mRightHead,lcElmSep,',')
      CASE cLeftType = 'D'
            IF INLIST(COPERATOR,'Between','In List')
               lcRetVal='{'+STRTRAN(ALLTRIM(mRightHead),lcElmSep,'},{')+'}'
            ELSE    
              lcRetVal='{'+ALLTRIM(MRIGHTHEAD)+'}'
           ENDIF   
    ENDCASE  
  CASE cRightDes='F'
    lcRetVal=STRTRAN(ALLTRIM(lcRetVal),lcElmSep,',')    
ENDCASE
IF INLIST(cOperator,'Between','In List') AND EMPTY(ALLTRIM(mRightHead))
    lcRetVal=lcRetVal+','+lcRetVal
ENDIF
RETURN lcRetVal




FUNCTION lfvClearflt
SELECT(lcCurDbf)
lcGetFlt=ALLTRIM(gfGenFlt(lcArray,.T.))
lccurFlt=ALLTRIM(FILTER())
SET FILTER TO 
IF !EMPTY(lcGetFlt)
  SET FILTER TO &lcGetFlt
ENDIF
lcGetFlt=FILTER()
*lnStrPos=RAT(UPPER(lcGetFlt),UPPER(lcCurFlt),1)-1
*lcCurFlt=IIF(lnStrPos>0,SUBSTR(lcCurFlt,1,IIF(lnStrPos>0,lnStrPos,LEN(lcCurFlt))),'')
*lcCurFlt=ALLTRIM(lcCurFlt)
IF OCCURS(lcGetFlt,lcCurFlt)>0
  lcCurFlt=STRTRAN(lcCurFlt,lcGetFlt,'',OCCURS(lcGetFlt,lcCurFlt))
ENDIF
IF RIGHT(ALLTRIM(lcCurFlt),1)='.'
  lcRight='.AND.'
ELSE
  lcRight='AND'  
ENDIF
IF RIGHT(ALLTRIM(lcCurFlt),LEN(lcRight))=lcRight
  lcCurFlt=LEFT(lcCurFlt,LEN(lcCurFlt)-LEN(lcRight))
ENDIF
  SELECT(lcCurDbf)
  SET FILTER TO  
  IF !EMPTY(ALLTRIM(lcCurFlt))
    SET FILTER TO &lcCurFlt
  ENDIF  
DIMENSION &lcArray[1,7]
STORE '' TO &lcArray
llTerminat=.T.
lcCurFilter=''
CLEAR READ

*!*************************************************************************
*!
*!              Function: lfobjBox
*!
*!*************************************************************************
*  this function to draw a 3D Box 
FUNCTION lfObjBox
PARAMETERS lnYStart,lnXStart,lnYEnd,lnXend,lcType
lcType=UPPER(IIF(TYPE('lcType')='C',lcType,'3DUP'))

  lcColor=IIF(lcType='3DUP','RGB(128,128,128,128,128,128)',;
	                               'RGB(255,255,255,255,255,255)')           
  lcColor1=IIF(lcType!='3DUP','RGB(128,128,128,128,128,128)',;
	                               'RGB(255,255,255,255,255,255)')



        @ lnYStart-0.077*2,lnXStart-0.167*2;
          TO lnYEnd+0.077*2,lnXEnd+0.167*2;
		  PEN 1, 8 ;
		  STYLE "T";
          COLOR RGB(,,,192,192,192)

        @ lnYStart-0.077,lnXStart-0.167;        
           TO lnYEnd+0.077,lnXEnd+0.167;
       	   PEN 2, 8 ;
       	   STYLE "T";
	       COLOR &lcColor
		 
        @ lnYStart-0.077,lnXStart-0.167;		 
          TO lnYStart-0.077,lnXEnd+0.167;
		  PEN 2, 8 ;
		  STYLE "1" ;
  	      COLOR &lcColor1
  	    
        @ lnYStart-0.077,lnXStart-0.167;		          
          TO lnYEnd+0.077,lnXStart-0.167;
		  PEN 2, 8 ;
		  STYLE "1" ;
  	      COLOR &lcColor1







*!*************************************************************************
*!
*!              Function: lfPopFill
*!
*!*************************************************************************
*  this function to define the bars of the list of the filter
FUNCTION lfPopFill
PARAMETERS lcPopName,lcArrName
RELEASE BAR ALL OF (lcpopname)
GO TOP
FOR lnCount=1 to RECCOUNT()
     GO lnCount
     lcPrompt=''
   IF mlField_Nm<>'.OR.'  
    lcPrompt=PADR(SUBSTR(mheader,1,22),22)+PADR(IIF(lisnot,' IS ',' IS Not '),8)+;
             PADR(COPERATOR,16)+STRTRAN(SUBSTR(mRightHead,1,25),lcElmSep,',') 
  ELSE
    lcprompt=REPLICATE(IIF(_DOS,'Í','_'),35)+' OR '+  REPLICATE(IIF(_DOS,'Í','_'),35)
  ENDIF
      DEFINE BAR lnCount OF (lcPopName) PROMPT lcPrompt  &&lnCount
*      REPLACE nFieldPos WITH lnCount
ENDFOR


*!*************************************************************************
*!
*!              Function: lfarngCurs
*!
*!*************************************************************************
*  function to modify the file from the popup if there is arranging happend
FUNCTION lfarngCurs
PARAMETERS lcPopUp,llHaveToDoIt
llSwapped = .f.
lnDel=0
lnCount=1
IF !llHaveToDoIt
DO WHILE lnCount<=CNTBAR(lcPopup) AND !llSwapped  && check if there is any arranging
     llSwapped=IIF(GETBAR(lcpopup,lnCount)<>lnCount,.T.,llSwapped)
     lnCount=lnCount+1     
ENDDO
ENDIF
IF llSwapped OR llHaveToDoIt  && if there is any arranging or forcing to make the arranging
  =AFIELDS(laFields)
  CREATE CURSOR TEMP FROM ARRAY laFields  && create another cursor
  SELECT 'TEMCursor'
  DIMENSION laCont[1]
  GO TOP
  FOR lnCount = 1 TO CNTBAR(lcPopUp)  && Start coping the records needed in its order in the list
       llSwapped=IIF(GETBAR(lcpopup,lnCount)<>lnCount,.T.,llSwapped)
       LOCATE FOR nfieldpos = GETBAR(lcpopup,lnCount)
       IF FOUND()
          SCATTER TO laCont MEMO
          SELECT 'TEMP'
          APPEND BLANK
          GATHER FROM laCont MEMO
          REPLACE nFieldPos WITH lnCount
          SELECT 'TEMCursor'
       ENDIF
  ENDFOR
  USE IN TEMCURSOR
  CREATE CURSOR TEMCURSOR FROM ARRAY laFields  
  SELECT 'Temp'
  GO TOP
  SCAN FOR !EOF()
    SCATTER TO laCont MEMO
    SELECT 'TEMCURSOR'
    APPEND BLANK
    GATHER FROM laCont MEMO
    SELECT 'TEMP'
  ENDSCAN
  USE IN TEMP
  SELECT 'TEMCURSOR'
ENDIF            
RETURN llSwapped





*!*************************************************************************
*!
*!              Function: lfFillPop
*!
*!*************************************************************************
*
FUNCTION lfFillPop
PARAMETERS lcPopName,lcArrName
RELEASE BAR ALL OF (lcpopname)
FOR lnCount=1 to ALEN(&lcArrName,1)
  IF !EMPTY(&lcArrName[lnCount,1])
    DEFINE BAR lnCount OF (lcPopName) PROMPT &lcArrName[lnCount,1]
  ENDIF
ENDFOR

FUNCTION lfDoVal
PARAMETERS lnRow,lnCol
IF INLIST(cOperator,'Between','In List') 
   DIMENSION laScrData[1]
   DO GetValue.prg
ELSE
  DIMENSION laScrData[1],laVldEnt[1]
  STORE IIF(cRightDes='V',IIF(cLeftType='D',CTOD(mrightHead),mrighthead),'') TO laScrData[1]
  lcOld=''
  IF cLeftType ='N'
    STORE VAL(mRightHead) TO laScrData[1]
  ENDIF
IF lVldEntry OR !EMPTY(mVEntries)
IF lVldEntry
  lcSelect=SELECT()
  lcFieldName = PADR(MLINE(mlField_Nm,1),10)
  DIMENSION laVldEnt[1]
  *E300789,1 Hesham (Start)
  *SELECT cDiscrep FROM &gcDataDir.codes ;
    WHERE cfld_Name = lcFieldName AND CCOMP_ID=GCACT_COMP;
    INTO ARRAY laVldEnt

  SELECT cDiscrep FROM &gcDataDir.codes ;
    WHERE cfld_Name = lcFieldName AND cdefcode='N';
    INTO ARRAY laVldEnt

  *E300789,1 Hesham (Start)    
  SELECT(lcSelect)  
ELSE
    DIMENSION laVldEnt[1,1]
    lcVldEnt=SUBSTR(mvEntries,1,ATC(lcVldSep,mvEntries)-1)
      =gfSubStr(lcVldEnt,@laVldEnt,lcElmSep)
    DIMENSION laVldEnt[ALEN(laVldEnt,1),1]      
ENDIF  
     DEFINE POPUP p_popup FROM 1,1;
                         TO 4,30
    =lfFillPop('p_popup','laVldEnt')
    ON SELECTION POPUP p_popup DO pop_choice WITH PROMPT(),POPUP(),;
                       'laScrData[1]',.T.
    ACTIVATE POPUP p_popup AT lnRow,lnCol
    RELEASE POPUP p_popup
    lcScrData=lfGetStr('laScrData')    
        REPLACE mRightHead WITH lcScrData,;
              mrField_NM WITH lfTrnsValue(mlField_Nm,lcScrData,'H'),;
              cRightDes WITH 'V'
        IF lfarngCurs('PUQUERY')        
          =lfPopFill('puquery',lcArray)          
        ENDIF  
        =lfChangeBar(lsQuery,'puquery')              
  ELSE
   **** TAKE CARE ABOUT WINDOWS VER
     lcGenVal='lfGenVal'+IIF(_DOS,'D','W')
    DO (lcGenVal) IN SYBFiltr.SPR 
    IF READKEY() != 268 
      IF !EMPTY(laScrData[1])
        lcScrData=lfGetStr('laScrData')
        REPLACE mRightHead WITH lcScrData,;
              mrField_NM WITH lfTrnsValue(mlField_Nm,lcScrData,'H'),;
              cRightDes WITH 'V'
        IF lfarngCurs('PUQUERY')        
          =lfPopFill('puquery',lcArray)          
        ENDIF  
        =lfChangeBar(lsQuery,'puquery')          
      ENDIF  
    ENDIF
  ENDIF  
ENDIF  



lcScrData=lfGetStr('laScrData')





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
RETURN IIF(cLeftType='D',10,MIN(nWidth,lnSize))



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
IF ALLTRIM(cOperator)<>'Contains'
   IF !EMPTY(ALLTRIM(MVAld_Str))
    IF LASTKEY()=13 
      IF !EMPTY(&lcVarName)
        lcToProceed=ALLTRIM(MVAld_Str)
        lcToProceed=STRTRAN(lcToProceed,'*VALUE*','"'+lcVarName+'"')
        lcToProceed=STRTRAN(lcToProceed,'*OLDVAL*','lcOld')
        llValid=&lcToProceed
      ENDIF
    ELSE
  *   &lcVarName=lcOld
  *   SHOW GET &lcVarName
    ENDIF
  ENDIF  
ENDIF 
RETURN llValid



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


FUNCTION lfvSeeFlt
PRIVATE lcquery
*lnCurDbf=SELECT()
*lcTextName=SYS(3)+'.TXT'
lcquery = lfGetQuery()
*lnLowLFile=FCREATE(lcTextName)
*lcWinTitl=IIF(TYPE('lcWinTitl')<>'C','Filter Expression',lcWinTitl)
*IF lnLowLFile<0
*  WAIT 'ERROR CAN NOT CREATE THE FILTER' WINDOW
*ELSE
*  =FWRITE(lnLowLFile,lcQuery)
*  =FCLOSE(lnLowLFile)
*  PUSH KEY
*  ON KEY
*  ON KEY LABEL "CTRL+F1"  lLogical=.F.
*  ON KEY LABEL "ESCAPE" KEYBOARD CHR(23)+'C'
*   DO dispfile.spr
*  POP KEY
*   ERASE &lcTextName
*ENDIF
*SELECT(lnCurDbf)
DIMENSION laOGTmpMode[5]
=ACOPY(laScrMode,laOGTmpMode)
STORE .F. TO laScrMode
laScrMode[2]=.t.
=gfZoom('lcQuery')
=ACOPY(laOGTmpMode,laScrMode)




FUNCTION lfwdispfile
MODIFY FILE &lcTextName WINDOW lwDispFltr NOEDIT NOWAIT SAVE IN WINDOW gwdDispF




FUNCTION lfCloseFile
RELEASE WINDOW FILTER EXPRESSION 
RELEASE WINDOW lwDispFltr
CLEAR READ


FUNCTION lfInitArr
PARAMETERS lcOGArrName
lnOGCount=1
DO WHILE &lcOGArrName[lnOGCount,1]='.OR.'
   lnOGCount=lnOGCount+1
ENDDO
IF (lnOGCount-1)>0
   lnOGEnd=lnOGCount-1
   FOR lnOGCount=1 TO lnOGEnd
     =ADEL(&lcOGArrName,1)
     IF ALEN(&lcOGArrName,1)>1
       DIMENSION &lcOGArrName[ALEN(&lcOGArrName,1)-1,ALEN(&lcOGArrName,2)]
     ELSE
       STORE '' TO &lcOGArrName
     ENDIF   
  ENDFOR  
ENDIF
lnOGCount=ALEN(&lcOGArrName,1)
DO WHILE &lcOGArrName[lnOGCount,1]='.OR.'
   lnOGCount=lnOGCount-1
ENDDO
IF lnOGCount<ALEN(&lcOGArrName,1)
   lnOGStart=lnOGCount+1
   FOR lnOGCount=lnOGStart TO ALEN(&lcOGArrName,1)
     =ADEL(&lcOGArrName,ALEN(&lcOGArrName,1))
     IF ALEN(&lcOGArrName,1)>1
       DIMENSION &lcOGArrName[ALEN(&lcOGArrName,1)-1,ALEN(&lcOGArrName,2)]
     ELSE
       STORE '' TO &lcOGArrName
     ENDIF   
  ENDFOR     
ENDIF
lnOGCount=ASCAN(&lcOGArrName,'.OR.')
IF lnOGCount>0
  lnOGCount=ASUBSCRIPT(&lcOGArrName,lnOGCount,1)
  DO WHILE lnOGCount < ALEN(&lcOGArrName,1)
    IF &lcOGArrName[lnOGCount,1]='.OR.'
      IF &lcOGArrName[lnOGCount+1,1]='.OR.'
        =ADEL(&lcOGArrName,lnOGCount)
         DIMENSION &lcOGArrName[ALEN(&lcOGArrName,1)-1,ALEN(&lcOGArrName,2)]      
      ENDIF
    ENDIF  
    lnOGCount=lnOGCount+1
  ENDDO
ENDIF


*!*************************************************************************
*!
*!              Function: lfObjPos
*!
*!*************************************************************************
*  This function to return an object position in the array that defined in
* the screen layout 'setup' to hold the position of the screen objects and
* the names to and this function is to return the row # that have the data
* of this object
FUNCTION lfObjPos
PARAMETERS lcOGObjNam
PRIVATE lnCount
llOGFound=.f.
FOR lnCount=1 TO ALEN(laObjScrP,1)
  IF UPPER(laObjScrP[lnCount,5])=UPPER(lcOGObjNam)
    llOGFound=.t.
    EXIT  
  ENDIF
ENDFOR
RETURN IIF(llOGFound,lnCount,0)

