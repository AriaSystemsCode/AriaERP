*:************************************************************************
*: Program file  : GFGENFLT.PRG
*: Program desc. : 
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
*E300857,1  HS  04/22/98   Add a new screen (In range) to the In list
*E300857,1                 operator.
*B602035,1  HS  06/11/98   Include a check for non character type fields.
*B602208,1  MAB 11/16/1998 Last item in expression cut trail spaces. 
*B801961,1  HS   02/18/99 1) add a new parameter to the function to let it
*B801961,1 				     know if it can use the filter array in the
*B801961,1 				     filter expression or not. And if so use the
*B801961,1 				     $ function instead of the INLIST() function,
*B801961,1 				     Because the INLIST() function has a maximum
*B801961,1 				     number of arguments.
*B602931,1  MAN 05/24/1999 Enhance Performance of In List
*B602931,4  HS  05/26/1999 Enhance Performance of In Range
*B603015,1  MAB 07/18/1999 Contains operator does not work good.
*B603427,1  SSH 30/01/00  Support the function for Y2K problem.
*B119888,1  ABD 10/22/2003 Fix bug that Evalute the "|" in case less than 24 not 25
*B119888,1  ABD            that because the inlist can evaluate 24 Expration only.
*:************************************************************************
*
*B801961,1 Change this line to add a new parameter to the function to let it
*B801961,1 know if it can use the filter array in the filter expression
*B801961,1 or not [Begin]
*PARAMETERS lcArray,llFilter
PARAMETERS lcArray , llFilter , llUseArray
*B801961,1 Change this line to add a new parameter to the function [End]

*B801961,1 Add this line to add a new parameter to the function to let it
*B801961,1 know if it can use the filter array in the filter expression
*B801961,1 or not [Begin]
llUseArray = IIF(TYPE('llUseArray') <> 'L' , .F. , llUseArray)
*B801961,1 Add this line to add a new parameter to the function [End]

lcquery=''
lcElmSep='|'
lcValSep='~'

lcLineFeed=' ' &&+CHR(10)+CHR(13)
lnFltStart=1
DO WHILE (&lcArray[lnFltStart,1]='.OR.' OR EMPTY(&lcArray[lnFltStart,1]));
        AND !lnFltStart=ALEN(&lcArray,1)
  lnFltStart=lnFltStart+1
ENDDO 
lnFltEnd=ALEN(&lcArray,1)
DO WHILE (&lcArray[lnFltEnd,1]='.OR.' OR EMPTY(&lcArray[lnFltEnd,1]));
         AND lnFltEnd>1
  lnFltEnd=lnFltEnd-1
ENDDO 

lnOr=0       
IF lnFltStart>ALEN(&lcArray,1)
  RETURN ''
ENDIF
IF lnFltEnd=ALEN(&lcArray,1)
   lcWhichElm=lcArray+'['+ALLTRIM(STR(ALEN(&lcArray,1)))+',1]'
  IF TYPE(lcWhichElm)<>'C'
    RETURN ''
  ENDIF
  IF &lcArray[ALEN(&lcArray,1),1]='.OR.'
    RETURN ''
  ENDIF
ENDIF
lnCount=lnFltStart

DO WHILE  lnCount<=lnFltEnd
*!EMPTY(ALLTRIM(STRTRAN(STRTRAN(&lcArray[lnCount,6],lcElmSep,'');
   ,IIF(&lcArray[lnCount,3]='D','/  /',''),'')))
  IF &lcArray[lnCount,3]='N' AND EMPTY(VAL(&lcArray[lnCount,6])) ;
      AND &lcArray[lnCount,7]='V'
    lnCount=lnCount+1
    LOOP
  ENDIF
  
  *B602035,1 Change this line to include a check for non character type
  *fields [Begin]
  
  *IF !EMPTY(ALLTRIM(STRTRAN(STRTRAN(&lcArray[lnCount,6],lcElmSep,'');
  *   ,IIF(&lcArray[lnCount,3]='D','/',''),''))) OR &lcArray[lnCount,1]='.OR.'
  
  IF IIF(TYPE(ALLTRIM(lcArray) + "[lnCount,6]") = 'C' ,;
         !EMPTY(ALLTRIM(STRTRAN(STRTRAN(&lcArray[lnCount,6] , lcElmSep ,;
         '') , IIF(&lcArray[lnCount,3] = 'D' ,'/' , '') , ''))) ,;
         !EMPTY(&lcArray[lnCount,6])) .OR. &lcArray[lnCount,1] = '.OR.'
  
  *B602035,1 Change this line to include a check for non character type [End]
  
    IF !EMPTY(&lcArray[lnCount,1])
      IF &lcArray[lnCount,1]<>'.OR.'
        lcQuery=lcQuery+lfGetQCond(lnCount,lcArray,llFilter)
      ELSE
        lcQuery= IIF(RIGHT(lcQuery,9)=lcElmSep+' .AND. '+lcElmSep,SUBSTR(lcQuery,1,LEN(lcQuery)-9),lcQuery)
        IF lnOr>0
         lcQuery=lcQuery+' ) '
         lnOr=0
        ENDIF
        ** THIS CONDITION IS ADDED BY HESHAM 3 AUG.    
        DO WHILE lnCount<lnFltEnd-1 AND (EMPTY(ALLTRIM(STRTRAN(STRTRAN(&lcArray[lnCount+1,6],lcElmSep,'');
           ,IIF(&lcArray[lnCount+1,3]='D','/  /',''),''))) OR &lcArray[lnCount+1,1]='.OR.')
          lnCount=lnCount+1
        ENDDO
        IF !EMPTY(ALLTRIM(STRTRAN(&lcArray[lnCount+1,6],lcElmSep,''))) AND !EMPTY(ALLTRIM(lcQuery))
           lcQuery=ALLTRIM(lcQuery)+' '+lcElmSep+' OR '+lcElmSep+'( '
          lnOr=1
        ENDIF   
      ENDIF  
    ENDIF 
  ENDIF
  lnCount=lnCount+1   
ENDDO
lcQuery= IIF(RIGHT(lcQuery,9)=lcElmSep+' .AND. '+lcElmSep,SUBSTR(lcQuery,1,LEN(lcQuery)-9),lcQuery)
 IF lnOr>0
   lcQuery=lcQuery+' ) '
 ENDIF    
 lcQuery=STRTRAN(lcQuery,lcElmSep+' .AND. '+lcElmSep,lcLineFeed+' AND '+lcLineFeed)
 lcQuery=STRTRAN(lcQuery,lcElmSep+' OR '+lcElmSep,lcLineFeed+' OR '+lcLineFeed) 
RETURN lcQuery


FUNCTION lfGetQCond
PARAMETERS lnCount,lcArray,llFilter
lcFiltExp=''
DO CASE
  CASE &lcArray[lnCount,5] = 'Contains'
  
       lcFiltExp=IIF(&lcArray[lnCount,4],'','!(')+;
                 lfrightGet(&lcArray[lnCount,6],&lcArray[lnCount,3],;
                 &lcArray[lnCount,5],lcElmSep,&lcArray[lnCount,7])+;
                 ' $ '+ALLTRIM(&lcArray[lnCount,1])+' '+;
                 IIF(&lcArray[lnCount,4],'',' ) ')+lcElmSep+' .AND. '+lcElmSep                       
                 
  CASE &lcArray[lnCount,5] = 'Like' OR &lcArray[lnCount,5] = 'Exactly Like'
  
       lcFiltExp=IIF(&lcArray[lnCount,3]='D',"DTOS(",'')+ALLTRIM(&lcArray[lnCount,1]);
                  +IIF(&lcArray[lnCount,3]='D',")",'')+' '+IIF(&lcArray[lnCount,4],;
                   IIF(&lcArray[lnCount,5] = 'Like','=','=='),'<>')+' '+;
                  lfrightGet(&lcArray[lnCount,6],&lcArray[lnCount,3],;
                 &lcArray[lnCount,5],lcElmSep,&lcArray[lnCount,7])+lcElmSep+' .AND. '+lcElmSep                
               
  CASE INLIST(&lcArray[lnCount,5],'Greater Than','Less Than','Greater Or Equal',;
              'Less Or Equal')     
        lcOperator=lfGetOper(ALLTRIM(&lcArray[lnCount,5]),&lcArray[lnCount,4])              
        lcFiltExp=IIF(&lcArray[lnCount,4],'','!(')+;
                IIF(&lcArray[lnCount,3]='D',"DTOS(",'')+ALLTRIM(&lcArray[lnCount,1]);
                  +IIF(&lcArray[lnCount,3]='D',")",'')+' '+lcOperator+' '+;
                  lfrightGet(&lcArray[lnCount,6],&lcArray[lnCount,3],;
                 &lcArray[lnCount,5],lcElmSep,&lcArray[lnCount,7])+IIF(&lcArray[lnCount,4],'',' ) ')+;
                 lcElmSep+' .AND. '+lcElmSep                              
  CASE &lcArray[lnCount,5] = 'Between'
    IF llFilter
       
       lcFiltExp=IIF(&lcArray[lnCount,4],'BETWEEN(','!BETWEEN(')+;
               IIF(&lcArray[lnCount,3]='D',"DTOS(",'')+ALLTRIM(&lcArray[lnCount,1]);
                   +IIF(&lcArray[lnCount,3]='D',")",'')+','+;
                   lfrightGet(&lcArray[lnCount,6],&lcArray[lnCount,3],;
                   &lcArray[lnCount,5],lcElmSep,&lcArray[lnCount,7])+;
                   ')'+lcElmSep+' .AND. '+lcElmSep
    ELSE
         lcFiltExp= IIF(&lcArray[lnCount,4],'','!(')+;
                    IIF(&lcArray[lnCount,3]='D',"DTOS(",'')+ALLTRIM(&lcArray[lnCount,1]);
                   +IIF(&lcArray[lnCount,3]='D',")",'')+' BETWEEN '+;
                   lfrightGet(&lcArray[lnCount,6],&lcArray[lnCount,3],;
                   &lcArray[lnCount,5],lcElmSep,&lcArray[lnCount,7])+;
                   IIF(&lcArray[lnCount,4],'',')')+lcElmSep+' .AND. '+lcElmSep    
    ENDIF  
  CASE &lcArray[lnCount,5] = 'In List'
    
    *E300857,1 Add this if condition to add a new screen (In range) to
    *the In list operator [Begin]
    
    *E300857,1 If this filter option is not using the In range screen
    IF &lcArray[lnCount,7] <> 'R'
    
    *E300857,1 Add this if condition to add a new screen (In range) [End]
      
      *B801961,1 Add this line to add a new parameter to the function to let
      *B801961,1 it know if it can use the filter array in the filter
      *B801961,1 expression or not And if so use the $ function instead of
      *B801961,1 the INLIST() function, Because the INLIST() function has
      *B801961,1 a maximum number of arguments [Begin]
      
      *B801961,1 If the function cannot use the filter array in the filter expression
      
      *B602931,1 MAN Start Use Inlist instead of $ if Applicable
      *IF !llUseArray
      *B119888,1 ABD - Evalute the "|" in case less than 24 not 25 that because the inlist can evaluate 24 Expration only. [Begin]
      *IF !llUseArray OR OCCURS("|",&lcArray[lnCount,6]) < 25
      IF !llUseArray OR OCCURS("|",&lcArray[lnCount,6]) < 24
      *B119888,1 ABD - [End]
      *B602931,1 MAN End Use Inlist instead of $ if Applicable
      
      *B801961,1 Add this line to add a new parameter to the function [End]
        
        IF llFilter
          lcFiltExp=IIF(&lcArray[lnCount,4],'INLIST(','!INLIST(')+;
                    IIF(&lcArray[lnCount,3]='D',"DTOS(",'')+ALLTRIM(&lcArray[lnCount,1]);
                       +IIF(&lcArray[lnCount,3]='D',")",'')+','+;
                       lfrightGet(&lcArray[lnCount,6],&lcArray[lnCount,3],;
                       &lcArray[lnCount,5],lcElmSep,&lcArray[lnCount,7])+;
                       ')'+lcElmSep+' .AND. '+lcElmSep
        ELSE
          lcFiltExp= IIF(&lcArray[lnCount,4],'','!(')+;
                        IIF(&lcArray[lnCount,3]='D',"DTOS(",'')+ALLTRIM(&lcArray[lnCount,1]);
                       +IIF(&lcArray[lnCount,3]='D',")",'')+' IN('+;
                       lfrightGet(&lcArray[lnCount,6],&lcArray[lnCount,3],;
                       &lcArray[lnCount,5],lcElmSep,&lcArray[lnCount,7])+')'+;
                       IIF(&lcArray[lnCount,4],'',')')+lcElmSep+' .AND. '+lcElmSep    
        ENDIF    
        
      *B801961,1 Add this line to add a new parameter to the function to let
      *B801961,1 it know if it can use the filter array in the filter
      *B801961,1 expression or not And if so use the $ function instead of
      *B801961,1 the INLIST() function, Because the INLIST() function has
      *B801961,1 a maximum number of arguments [Begin]
      ELSE    && Else [If the function can use the filter array in the filter expression]
        DO CASE
          
          *B801961,1 Case of Numeric field (Or expression)
          CASE &lcArray[lnCount,3] = 'N'
            *-- Variable to hold the Numeric field Picture
            lcPict    = IIF(ATC(lcElmSep , &lcArray[lnCount,6]) = 0 ,;
                            &lcArray[lnCount,6] ,;
                            SUBSTR(&lcArray[lnCount,6] , 1 ,;
                                   ATC(lcElmSep , &lcArray[lnCount,6]) - 1))
            
            lcPict    = STUF(REPLICATE('9' , LEN(lcPict)) ,;
                             ATC('.' , lcPict) ,;
                             IIF(ATC('.' , lcPict) = 0 , 0 , 1) ,;
                             IIF(ATC('.' , lcPict) = 0 , '' , '.'))
            
            *-- Variable to hold the Numeric field Length (In character
            *-- string)
            lcLength  = ALLTRIM(STR(LEN(lcPict)))
            
            *-- Variable to hold the Numeric field Decimal Length (In
            *-- character string)
            lcDecimal = ALLTRIM(STR(IIF(AT('.' , lcPict) = 0 , 0 ,;
                                        LEN(lcPict) - AT('.' , lcPict))))
            
            lcFiltExp = IIF(&lcArray[lnCount,4] , '(' , '!(') +;
                        'STR(' + ALLTRIM(&lcArray[lnCount,1]) +;
                        ' , ' + lcLength + ' , ' + lcDecimal + ')' +;
                        ' $ ' + ALLTRIM(lcArray) +;
                        '[' + ALLTRIM(STR(lnCount)) + ',6]' + ')' +;
                        lcElmSep + ' .AND. ' + lcElmSep
            
          *B801961,1 Case of Date field (Or expression)
          CASE &lcArray[lnCount,3] = 'D'
            lcFiltExp = IIF(&lcArray[lnCount,4] , '(' , '!(') +;
                        'DTOC(' + ALLTRIM(&lcArray[lnCount,1]) + ')' +;
                        ' $ ' + ALLTRIM(lcArray) +;
                        '[' + ALLTRIM(STR(lnCount)) + ',6]' + ')' +;
                        lcElmSep + ' .AND. ' + lcElmSep
            
          *B801961,1 Otherwise
          OTHERWISE
            lcFiltExp = IIF(&lcArray[lnCount,4] , '(' , '!(') +;
                        ALLTRIM(&lcArray[lnCount,1]) +;
                        ' $ '+ ALLTRIM(lcArray) +;
                        '[' + ALLTRIM(STR(lnCount)) + ',6]' + ')' +;
                        lcElmSep + ' .AND. ' + lcElmSep
            
        ENDCASE
      ENDIF    && End of IF !llUseArray
      *B801961,1 Add this line to add a new parameter to the function [End]
      
    *E300857,1 Add these lines to add a new screen (In range) to the
    *In list operator [Begin]
    ELSE    && Else [If this filter option is using the In range screen]
      
      *E300857,1 If the In Range cursor is opened and if it has one or
      *more records
      IF USED(&lcArray[lnCount,6]) .AND. SEEK('' , &lcArray[lnCount,6])
        
        *B602931,4 Change this line to create an optimize-able expression
        *          if applicable [Begin]
        *lcFiltExp = IIF(&lcArray[lnCount,4] , '' , '!') + 'SEEK(' + ;
        *            ALLTRIM(&lcArray[lnCount,1]) + ' , "' + ;
        *            &lcArray[lnCount,6] + '")' +;
        *            lcElmSep + ' .AND. ' + lcElmSep
        
        PRIVAT lnSavAls , lnSelCnt , lcInlstExp , lcOptExp , lcKey
        
        lnSavAls   = SELECT(0)        && Save the old alias.
        lnSelCnt   = 0                && Variable to be used to count the user selection
        lcInlstExp = ""               && Variable to hold the user selection
        lcKey      = ''
        
        *-- Get an optimize-able expression, if applicable.
        lcOptExp   = lfGetOptEx(lcArray , lnCount , @lcKey)
        
        SELECT (&lcArray[lnCount,6])
        
        *-- Scan loop to count and get the user selection as far as it
        *-- doesn't exceeds 24 selection.
        SCAN WHILE lnSelCnt <= 25
          *-- count the user selection
          lnSelCnt   = lnSelCnt   + 1
          *-- Accumulate the key - if there is one - and the user selection
          lcInlstExp = lcInlstExp + ',"' + lcKey + EVAL(KEY()) + '"'
        ENDSCAN    && End of SCAN WHILE lnSelCnt <= 25
        
        *-- If the user selected more than 24 selection
        IF lnSelCnt > 24
          
          *-- Use BETWEEN() function, if applicable.
          IF &lcArray[lnCount,4]
            GO TOP
            lcFiltExp = 'BETWEEN(' + lcOptExp +;
                        ',"' + lcKey + EVAL(KEY()) + '",'
            
            GO BOTTOM   
            lcFiltExp = lcFiltExp + '"' + lcKey + EVAL(KEY()) + '")' +;
                        lcElmSep + ' .AND. ' + lcElmSep
            
          ENDIF    && End of IF &lcArray[lnCount,4]
          
          *-- Use the SEEK() function
          lcFiltExp = lcFiltExp + IIF(&lcArray[lnCount,4] , '' , '!') + 'SEEK(' + ;
                      ALLTRIM(&lcArray[lnCount,1]) + ' , "' + ;
                      &lcArray[lnCount,6] + '")' +;
                      lcElmSep + ' .AND. ' + lcElmSep
        
        *-- Use INLIST when the selection less than 24              
        ELSE    && Else (If the user selected 24 selection or less.)
          lcFiltExp = 'INLIST(' + lcOptExp + lcInlstExp + ;
                      ')' + lcElmSep + ' .AND. ' + lcElmSep
        ENDIF                         
        SELECT (lnSavAls)
        *B602931,4 Change this line to create an optimize-able [End]
        
      ELSE    && Else [If the In Range cursor is not opened or if it has no records]
        lcFiltExp = ''
      ENDIF    && End of IF USED(&lcArray[lnCount,6]) .AND. SEEK('' , &lcArray[lnCount,6])
    ENDIF    && End of IF &lcArray[lnCount,7] <> 'R'
    *E300857,1 Add these lines to add a new screen (In range) [End]
    
ENDCASE

RETURN lcFiltExp


FUNCTION lfGetOper
PARAMETER lcOperator,llisnot
DO CASE
  CASE lcOperator = 'Greater Than'
     RETURN '>'
  CASE lcOperator = 'Less Than'
     RETURN '<'
  CASE lcOperator = 'Greater Or Equal'
    RETURN '>='
  CASE lcOperator = 'Less Or Equal'
    RETURN '<='
ENDCASE  


*B603427,1  SSH 30/01/00 Support the function for Y2K problem.
FUNCTION lfRightGet
PARAMETERS mRightHead,cLeftType,cOperator,lcElmSep,cRightType

lcRetVal=mRightHead
DO CASE
  CASE cRightType='V'
    DO CASE      
      CASE cLeftType $ 'CM'
          IF INLIST(COPERATOR,'Between','In List')
            lcSeper=IIF(!llFilter AND cOperator='Between',' AND ',',')
            *B602208,1 MAB There is no need to alltrim because variable pass by its picture.
            *lcRetVal='"'+STRTRAN(ALLTRIM(mRightHead),lcElmSep,'"'+lcSeper+'"')+'"'
            lcRetVal='"'+STRTRAN(mRightHead,lcElmSep,'"'+lcSeper+'"')+'"'
          ELSE
            *B602208,1 MAB There is no need to alltrim because variable pass by its picture.
            *RETURN '"'+IIF(cLeftType='C',mrightHead,ALLTRIM(mrightHead))+'"'   &&'"'+ALLTRIM(mrightHead)+'"'   

            *B603015,1 Do not truncate Contains operator [Begin]
            *RETURN '"'+mrightHead+'"'
            RETURN '"'+IIF(cOperator='Contains',ALLTRIM(mrightHead),mrightHead)+'"'
            *B603015,1 Do not truncate Contains operator [End  ]

          ENDIF
      CASE cLeftType = 'N'
            lcSeper=IIF(COPERATOR='Between' AND !llFilter,' AND ',',')
            lcRetVal=STRTRAN(mRightHead,lcElmSep,lcSeper)
         IF EMPTY(lcRetVal)
           lcRetVal='0'
         ENDIF
      CASE cLeftType = 'D'
            *B603427,1  SSH 30/01/00 (Start) Support the function for Y2K problem.
            lcOldCen = SET('CENT')
            SET CENTURY ON
            *B603427,1  SSH 30/01/00 (End)
            IF INLIST(COPERATOR,'Between','In List')
            lcSeper=IIF(!llFilter AND cOperator='Between',' AND ALLTRIM(DTOS(',',ALLTRIM(DTOS(')            
               lcRetVal='ALLTRIM(DTOS({  '+STRTRAN(ALLTRIM(mRightHead),lcElmSep,'  }))'+lcSeper+'{  ')+'  }))'
            ELSE
            *B603427,1  SSH 30/01/00 (Start) Support the function for Y2K problem.
              IF BETWEEN(YEAR(CTOD(MRIGHTHEAD)) ,1900,1950)
                lcTmpYear  = ALLTRIM(STR(YEAR(CTOD(MRIGHTHEAD)) + 100))
                MRIGHTHEAD = SUBSTR(MRIGHTHEAD,1,6)+lcTmpYear
              ENDIF
            *B603427,1  SSH (End)
              lcRetVal='ALLTRIM(DTOS({  '+ALLTRIM(MRIGHTHEAD)+'  }))'
           ENDIF   
           *B603427,1  SSH 30/01/00 (Start) Support the function for Y2K problem.
           SET CENTURY &lcOldCen
           *B603427,1  SSH 30/01/00 (End)
      CASE cLeftType = 'L'
           RETURN ' '+lcRetVal+' '
    ENDCASE  
  CASE cRightType='F'
    *B602208,1 MAB There is no need to alltrim because variable pass by its picture.
    *lcRetVal=STRTRAN(ALLTRIM(mRightHead),lcElmSep,',')

    *B603015,1 Do not truncate Contains operator [Begin]
    *lcRetVal=STRTRAN(mRightHead,lcElmSep,',')
    lcRetVal=STRTRAN(IIF(cOperator='Contains',ALLTRIM(mrightHead),mrightHead),lcElmSep,',')
    *B603015,1 Do not truncate Contains operator [End  ]
    
ENDCASE  
IF INLIST(cOperator,'Between','In List') AND EMPTY(ALLTRIM(mRightHead))
    lcSeper=IIF(!llFilter AND cOperator='Between',' AND ',',')
    lcRetVal=lcRetVal+lcSeper+lcRetVal
 ENDIF
RETURN lcRetVal


FUNCTION lfGetQ1
PARAMETERS lnCount,lcArray,llFilter
lcFiltExp=''
lcOperator=lfGetOper(ALLTRIM(&lcArray[lnCount,5]),&lcArray[lnCount,4])
IF ATC('(',lcOperator)<>0
   IF &lcArray[lnCount,5]<>'Contains'
     IF !llFilter  
       lcFiltExp=IIF(&lcArray[lnCount,3]='D',"DTOS(",'')+ALLTRIM(&lcArray[lnCount,1]);
                +IIF(&lcArray[lnCount,3]='D',")",'')+' '+lcOperator+;
                 lfrightGet(&lcArray[lnCount,6],&lcArray[lnCount,3],;
                 &lcArray[lnCount,5],lcElmSep,&lcArray[lnCount,7])+' ) '+lcElmSep+' .AND. '+lcElmSep              
     ELSE
       lcFiltExp=lcOperator+IIF(&lcArray[lnCount,3]='D',"DTOS(",'')+;
                 ALLTRIM(&lcArray[lnCount,1])+IIF(&lcArray[lnCount,3]='D',")",'')+','+;
                 lfrightGet(&lcArray[lnCount,6],&lcArray[lnCount,3],;
                 &lcArray[lnCount,5],lcElmSep,;
                 &lcArray[lnCount,7])+' ) '+lcElmSep+' .AND. '+lcElmSep                
     ENDIF
  ELSE
       lcFiltExp=lcOperator+;
                 lfrightGet(&lcArray[lnCount,6],&lcArray[lnCount,3],;
                 &lcArray[lnCount,5],lcElmSep,&lcArray[lnCount,7])+;
                 ' $ '+ALLTRIM(&lcArray[lnCount,1])+' '+;
                 ' ) '+lcElmSep+' .AND. '+lcElmSep                     
  ENDIF    
ELSE
  IF &lcArray[lnCount,5]='In List'
     lcFiltExp=IIF(&lcArray[lnCount,3]='D',"DTOS(",'')+ALLTRIM(&lcArray[lnCount,1]);
                +IIF(&lcArray[lnCount,3]='D',")",'')+' '+lcOperator+'( '+;
                lfrightGet(&lcArray[lnCount,6],&lcArray[lnCount,3],;
               &lcArray[lnCount,5],lcElmSep,&lcArray[lnCount,7])+' )'+lcElmSep+' .AND. '+lcElmSep                  
  ELSE
    lcFiltExp=IIF(&lcArray[lnCount,3]='D',"DTOS(",'')+ALLTRIM(&lcArray[lnCount,1]);
                +IIF(&lcArray[lnCount,3]='D',")",'')+' '+lcOperator+' '+;
                lfrightGet(&lcArray[lnCount,6],&lcArray[lnCount,3],;
               &lcArray[lnCount,5],lcElmSep,&lcArray[lnCount,7])+lcElmSep+' .AND. '+lcElmSep              
  ENDIF             
ENDIF 


FUNCTION lfGetOper1
PARAMETER lcOperator,llisnot
DO CASE
  CASE lcOperator = 'Like'
    RETURN IIF(llIsNot,'=','<>')
  CASE lcOperator = 'Exactly Like'
    RETURN IIF(llIsNot,'==','<>')
  CASE lcOperator = 'Greater Than'
     RETURN IIF(llIsNot,'>','!(')  &&+'>'
  CASE lcOperator = 'Less Than'
     RETURN IIF(llIsNot,'<','!(') &&+'<'
  CASE lcOperator = 'Greater Or Equal'
    RETURN '>='
  CASE lcOperator = 'Less Or Equal'
    RETURN '<='
  CASE lcOperator = 'Between'
    RETURN IIF(llIsNot,'','Not ')+IIF(llFilter,'BETWEEN(','BETWEEN')
  CASE lcOperator = 'In List'    
    RETURN IIF(llIsNot,'','Not ')+IIF(!llFilter,'IN ','INLIST(')
  CASE lcOperator = 'Contains'    
    RETURN IIF(llIsNot,'(','!(')    
ENDCASE  

*!*************************************************************
*! Name      : lfGetOptEx
*! Developer : HS (Haytham El_Sheltawi)
*! Date      : 05/25/1999
*! Purpose   : create an optimize-able expression - if applicable -
*!             for the In Range options.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfGetOptEx()
*!*************************************************************
*B602931,4 this function was added by HS for the bug entry B602931,4.
*!*************************************************************
FUNCTION lfGetOptEx

PARAMETERS lcArray , lnRow , lcKey

lcKey = ''

PRIVATE lnArrayRow , lcFile , lcField , lcKeyExpr , lcOptExp , lnSelect ,;
        lnCount , laOptExp
 
lnArrayRow = ASCAN(laBrFldFlt , lcArray + ':' + ALLTRIM(STR(lnRow)))
lnArrayRow = ASUBSCRIPT(laBrFldFlt , lnArrayRow , 1)

*-- If the filter option is of type expression.
IF TYPE('laBrFldFlt[lnArrayRow,4]') <> 'C'
  RETURN ALLTRIM(&lcArray[lnRow,1])
ENDIF

*-- Get the file of the filter option
lcFile = SUBSTR(ALLTRIM(&lcArray[lnRow,1]) ,;
                1 , AT('.' , ALLTRIM(&lcArray[lnRow,1])) - 1)

*-- If the file of the filter option is not open, we will not be able to
*-- get the indexes
IF !USED(lcFile)
  RETURN ALLTRIM(&lcArray[lnRow,1])
ENDIF

*-- Get the field of the filter option
lcField = UPPER(SUBSTR(ALLTRIM(&lcArray[lnRow,1]) ,;
                       AT('.' , ALLTRIM(&lcArray[lnRow,1])) + 1))

*-- Get the key expression that was passed to the In Range function
lcKeyExpr = IIF(AT('FOR' , laBrFldFlt[lnArrayRow,3]) = 0 ,;
                laBrFldFlt[lnArrayRow,3] ,;
                SUBSTR(laBrFldFlt[lnArrayRow,3] , 1 ,;
                       AT('FOR' , laBrFldFlt[lnArrayRow,3]) - 1))

lnSelect = SELECT(0)        && Save the alias
SELECT (lcFile)

lcOptExp = ''
lnCount  = 0

*-- DO WHILE loop to search for an index that starts with the field of the
*-- filter option.
DO WHILE lcOptExp <> lcField
  lnCount  = lnCount + 1
  lcOptExp = KEY(lnCount)
  IF EMPTY(lcOptExp)
    EXIT
  ENDIF
ENDDO    && End of DO WHILE lcOptExp <> lcField

*-- If we didn't find an index that starts with the field of the filter
*-- option we are going to search for one that starts with the same key
*-- used with the In Range function
IF EMPTY(lcOptExp) .AND. TYPE(laBrFldFlt[lnArrayRow,4]) = 'C' .AND.;
   TYPE(lcKeyExpr) = 'C'
  
  IF LEN(&lcKeyExpr) = LEN(&laBrFldFlt[lnArrayRow,4])
    lnCount  = 0
    DO WHILE lcOptExp <> laBrFldFlt[lnArrayRow,4] + '+' + lcField
     lnCount  = lnCount + 1
      lcOptExp = KEY(lnCount)
      IF EMPTY(lcOptExp)
        EXIT
      ENDIF    && End of IF EMPTY(lcOptExp)
    ENDDO    && End of DO WHILE lcOptExp <> laBrFldFlt[lnArrayRow,4] + ...
    
    *-- If we found an index that can be used
    IF !EMPTY(lcOptExp)
      *-- Get the key that will be used with this index expression
      lcKey = &lcKeyExpr
    ENDIF    && End of IF !EMPTY(lcOptExp)
  ENDIF    && End of IF LEN(&lcKeyExpr) = LEN(&laBrFldFlt[lnArrayRow,4])
ENDIF    && End of EMPTY(lcOptExp) .AND. TYPE(laBrFldFlt[lnArrayRow,4]) ...

SELECT (lnSelect)

*-- If we did not find an index that can be used
IF EMPTY(lcOptExp)
  RETURN ALLTRIM(&lcArray[lnRow,1])
ELSE    && Else (If we found an index that can be used)
  DIMENSION laOptExp[1,1]
  =gfSubStr(lcOptExp , @laOptExp , '+')
  lcOptExp = ''
  
  *-- FOR loop to add the alias to the index expression
  FOR lnCount = 1 TO ALEN(laOptExp , 1)
    lcOptExp = lcOptExp + IIF(EMPTY(lcOptExp) , '' , '+') +;
               IIF(OCCURS('(' , laOptExp[lnCount,1]) = 0 ,;
                   lcFile + '.' + laOptExp[lnCount,1] ,;
                   STUFF(laOptExp[lnCount,1] ,;
                         RAT('(' , laOptExp[lnCount,1]) + 1 , 0 ,;
                         lcFile + '.'))
    
  ENDFOR    && End of FOR lnCount = 1 TO ALEN(laOptExp , 1)
  RETURN lcOptExp
ENDIF    && End of IF EMPTY(lcOptExp)
