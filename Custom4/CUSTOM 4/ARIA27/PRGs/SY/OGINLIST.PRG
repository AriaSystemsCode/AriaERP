*:************************************************************************
*: Program file  : OGINLIST.PRG
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
*B601173,1 Hesham El-Sheltawi On 07/22/96
*B601173,1 change the defaulting value function to intialize the get field
*B601173,1 with only one space in case of getting values for character fields
*B602010,1 Hesham 06/09/98
*B602010,1 change the select query to ignore the related fields of
*B602010,1 the codes in the codes popup
*E300900,1  HS  07/21/98   Give the capability to make valid entries
*E300900,1                 from file or from array.
*E300900,1                 Note: 
*E300900,1                       I made some changes in the screen
*E300900,1                       INLIST.SCX
*E300900,1 Change this line to change the type of the parameter (llOGVld)
*E300900,1 from logic to character [I also changed the name to (lcVldEntT)]
*E300900,1 to add the capability to make valid entries from file or 
*E300900,1 from array [Begin]
*B602328,1 MAB 12/09/1998 Fix inlist source and target for valid codes in second time you press
*B602328,1                it to keep old selected values in mind.
*B602413,1 MAB 01/04/1999 1- Fix inlist source and target for valid codes in second time you press
*B602413,1                   it if you have closeset codes. 
*B602413,1                2- Fill array filter with true code(Not Trimed code) 
*B801961,1  HSS  02/18/99 1) take into consideration the case of Numeric
*B801961,1 				     and Date fields (Or expressions).
*B801961,1 				  2) If the field has a valid entries of
*B801961,1 				     type (Code):
*B801961,1 				     a) Display the code + code description in the
*B801961,1 				        case of editable codes and to sort the
*B801961,1 				        displayed values by code.
*B801961,1 				     b) Display the code description in the
*B801961,1 				        case of not editable codes and to sort the
*B801961,1 				        displayed values by code description.
*B801961,1 				  3) Since we are going to use the $ function
*B801961,1 				     instead of the INLIST() function in the filter
*B801961,1 				     expression we need to pad the user selection
*B801961,1 				     with the field length, which may have different
*B801961,1 				     length than the picture length and if so the
*B801961,1 				     filter will not get any records because the
*B801961,1 				     $ function will always return .F. if the user
*B801961,1 				     selection didn't have the same length as
*B801961,1 				     the field.
*E300789,1 Hesham 03/04/99
*E300789,1 remove field ccomp_id from files ACCOD,FISHD,FSPRD,FSHLD,CODES
*E300789,1 Change Any seeking in these files
*E301170,1 MAB 03/15/1999 Detect changes to OG Filter.
*B801980,1 Hesham 03/25/99
*B801980,1 change the list to use array insted of popup to make the mover
*B801980,1 faster in huge numbr of records
*B802166,1 HDM 04/21/1999 Check If Code not found in Codes File Then Return
*:************************************************************************

*PARAMETERS lcOGDataTyp,lcOGPicture,lnOGSize,lcOGVld,llOGVld,lcOGVldEnt,lcOGVar

*B801961,1 Change this line to add a new parameter to path the
*          field length to the function OGINLIST
*          Note: If the filter option is an expression we will
*                path 0 as the field length.
*PARAMETERS lcOGDataTyp , lcOGPicture , lnOGSize , lcOGVld , lcVldEntT ,;
*           lcOGVldEnt  , lcOGVar
PARAMETERS lcOGDataTyp , lcOGPicture , lnOGSize , lcOGVld , lcVldEntT ,;
           lcOGVldEnt  , lcOGVar , lnFldlenth
*B801961,1 Change this line to add a new parameter to path [End]

*E300900,1 Change this line to change the type of the parameter [End]

*B801961,1 Add these lines to add some new private variables [Begin]
*E301170,1 MAB 03/15/1999 Detect changes to OG Filter. [Begin
*PRIVATE lcValue , lcNmrcPict , lnLength , lnDecimal , llIsEdtble
PRIVATE lcValue , lcNmrcPict , lnLength , lnDecimal , llIsEdtble, lcOldVal
lcOldVal = EVALUATE(lcOGVar)
*E301170,1 MAB 03/15/1999 Detect changes to OG Filter. [End

*-- lcNmrcPict    Variable to hold the Numeric field Picture
*-- lnLength      Variable to hold the Numeric field Length
*-- lnDecimal     Variable to hold the Numeric field Decimal Length

*B801961,1 If Numeric field (Or expression)
IF lcOGDataTyp = 'N'
  lcNmrcPict = IIF(TYPE('lcOGPicture') = 'C' ,;
                   ALLTRIM(SUBSTR(lcOGPicture , AT('9' , lcOGPicture))) , '')
  
  lnLength   = LEN(lcNmrcPict)
  lnDecimal  = IIF(AT('.' , lcNmrcPict) = 0 ,;
                   0 , LEN(lcNmrcPict) - AT('.' , lcNmrcPict))

ENDIF   && End of IF lcOGDataTyp = 'N'
*B801961,1 Add these lines to add some new private variables [End]

lnOGSize=IIF(lcOGDataType='D',10,lnOGSize)
STORE .F. TO llOk,llAdd


lcElmSep='|'
lcValSep='~'

lcOGWhen= 'VALID '+IIF(EMPTY(lcOGVld),'lfvAddToL()',;
                   IIF(ATC(' WHEN ',lcOGVld)=0,ALLTRIM(lcOGVld)+'AND lfvAddToL()';
                    ,SUBSTR(lcOGVld,1,ATC(' WHEN ',lcOGVld)-1)+'AND lfvAddToL()';
                    +SUBSTR(lcOGVld,ATC(' WHEN ',lcOGVld))))

*E300900,1 Change this line to add the capability to make valid entries
*E300900,1 from file or from array [Begin]

*IF !llOGVld AND EMPTY(lcOGVldEnt)

*E300900,1 If not valid entries

IF EMPTY(lcOGVldEnt)

*E300900,1 Change this line to add the capability [End]
   
   DIMENSION laOGVldVal[1,1]
   =lfInitVal(ALLTRIM(&lcOGVar),'laOGVldVal')
ELSE
  
  *E300900,1 Change this line to add the capability to make valid entries
  *E300900,1 from file or from array [Begin]
  
  *IF llOGVld
  
  *E300900,1 If the valid entries type is Codes
  IF lcVldEntT = 'C'
  
  *E300900,1 Change this line to add the capability [End]
    
    lcSelect=SELECT()
    lcFieldName = PADR(MLINE(lcOGVldEnt,1),10)
    DIMENSION laOGVldEnt[1,1],laOGVld[1,1]
   *B602010,1 Hesham (Start)
   *B602010,1 change the select query to ignore the related fields of
   *B602010,1 the codes 

*    SELECT cDiscrep FROM &gcDataDir.codes ;
      WHERE cfld_Name = lcFieldName AND CCOMP_ID=GCACT_COMP;
      INTO ARRAY laOGVldEnt
*    SELECT cCode_No FROM &gcDataDir.codes ;
      WHERE cfld_Name = lcFieldName AND CCOMP_ID=GCACT_COMP;
      INTO ARRAY laOGVld
    
    *B801961,1 Change these lines to display the code + code description
    *B801961,1 in the case of editable codes and to sort the displayed
    *B801961,1 values (by code if the code is editable otherwise by code
    *B801961,1 description) [Begin]
    *SELECT cDiscrep FROM &gcDataDir.codes ;
    *  WHERE cfld_Name = lcFieldName AND CCOMP_ID=GCACT_COMP;
    *  AND CRLTFIELD = 'N';
    *  INTO ARRAY laOGVldEnt
    *SELECT cCode_No FROM &gcDataDir.codes ;
    *  WHERE cfld_Name = lcFieldName AND CCOMP_ID=GCACT_COMP;
    *  AND CRLTFIELD = 'N';
    *  INTO ARRAY laOGVld
    
    llIsEdtble = gfIsEdtble(lcFieldName)    && Check if the code is editable
    
    *B801961,1 If the code is editable
    IF llIsEdtble
    *E300789,1 Hesham (Start)
      *SELECT cCode_No + ' - ' + cDiscrep ;
        FROM &gcDataDir.codes ;
       WHERE cfld_Name = lcFieldName AND CCOMP_ID=GCACT_COMP;
             AND CRLTFIELD = 'N';
       ORDER BY cCode_No;
        INTO ARRAY laOGVldEnt
      
      *SELECT cCode_No FROM &gcDataDir.codes ;
       WHERE cfld_Name = lcFieldName AND CCOMP_ID=GCACT_COMP;
             AND CRLTFIELD = 'N';
       ORDER BY cCode_No;
        INTO ARRAY laOGVld
      SELECT cCode_No + ' - ' + cDiscrep ;
        FROM &gcDataDir.codes ;
       WHERE cfld_Name = lcFieldName AND cdefcode='N';
             AND CRLTFIELD = 'N';
       ORDER BY cCode_No;
        INTO ARRAY laOGVldEnt
        
       *--B802166,1 HDM[Start] Check If this code not found in Codes File Return
       IF _TALLY = 0
         =gfModalGen('TRM00350B00000','DIALOG')
         RETURN
       ENDIF
       *--B802166,1 HDM[End]
      SELECT cCode_No FROM &gcDataDir.codes ;
       WHERE cfld_Name = lcFieldName AND cdefcode='N';
             AND CRLTFIELD = 'N';
       ORDER BY cCode_No;
        INTO ARRAY laOGVld

    *E300789,1 Hesham (End)
    ELSE    && Else [If the code is not editable]
    *E300789,1 Hesham (Start)
      *SELECT ALLTRIM(cDiscrep) FROM &gcDataDir.codes ;
       WHERE cfld_Name = lcFieldName AND CCOMP_ID=GCACT_COMP;
             AND CRLTFIELD = 'N';
       ORDER BY cDiscrep;
        INTO ARRAY laOGVldEnt
      
      *SELECT cCode_No FROM &gcDataDir.codes ;
       WHERE cfld_Name = lcFieldName AND CCOMP_ID=GCACT_COMP;
             AND CRLTFIELD = 'N';
       ORDER BY cDiscrep;
        INTO ARRAY laOGVld

      SELECT ALLTRIM(cDiscrep) FROM &gcDataDir.codes ;
       WHERE cfld_Name = lcFieldName AND cdefcode='N';
             AND CRLTFIELD = 'N';
       ORDER BY cDiscrep;
        INTO ARRAY laOGVldEnt

       *--B802166,1 HDM[Start] Check If this code not found in Codes File Return
       IF _TALLY = 0
         =gfModalGen('TRM00350B00000','DIALOG')
         RETURN
       ENDIF
       *--B802166,1 HDM [End]
      
      SELECT cCode_No FROM &gcDataDir.codes ;
       WHERE cfld_Name = lcFieldName AND cdefcode='N';
             AND CRLTFIELD = 'N';
       ORDER BY cDiscrep;
        INTO ARRAY laOGVld

    *E300789,1 Hesham (End)
    ENDIF    && End of IF llIsEdtble
    *B801961,1 Change these lines to display the code + code description [End]
    
  *B602010,1 Hesham (End)      
    SELECT(lcSelect)  
    DIMENSION laOGVldVal[1,1]

*   =gfSubStr(ALLTRIM(&lcOGVar),@laOGVldVal,lcElmSep)
*    DIMENSION laOGVldVal[ALEN(laOGVldVal,1),1]   

  ELSE
    
    *E300900,1 Change these lines to add the capability to make valid entries
    *E300900,1 from file or from array [Begin]
    
    *IF !EMPTY(lcOGVldEnt)  
    *  DIMENSION laOGVldEnt[1,1],laOGVld[1,1]
    *  lcVldEnt=SUBSTR(lcOGVldEnt,1,ATC(lcValSep,lcOGVldEnt)-1)
    *  lcVld   =SUBSTR(lcOGVldEnt,ATC(lcValSep,lcOGVldEnt)+1)
    *    =gfSubStr(lcVldEnt,@laOGVldEnt,lcElmSep)
    *    =gfSubStr(lcVld,@laOGVld,lcElmSep)        
    *  DIMENSION laOGVldEnt[ALEN(laOGVldEnt,1),1],laOGVld[ALEN(laOGVld,1),1]      
    *  DIMENSION laOGVldVal[1,1]
*       =gfSubStr(ALLTRIM(&lcOGVar),@laOGVldVal,lcElmSep)
*        DIMENSION laOGVldVal[ALEN(laOGVldVal,1),1]   
    *ENDIF
    
    DO CASE
      
      *E300900,1 Case of string
      CASE lcVldEntT = 'E'
        
        DIMENSION laOGVldEnt[1,1] , laOGVld[1,1]
        lcVldEnt = SUBSTR(lcOGVldEnt , 1 , ATC(lcValSep , lcOGVldEnt) - 1)
        lcVld    = SUBSTR(lcOGVldEnt , ATC(lcValSep , lcOGVldEnt) + 1)
        =gfSubStr(lcVldEnt , @laOGVldEnt , lcElmSep)
        =gfSubStr(lcVld , @laOGVld , lcElmSep)        
        DIMENSION laOGVldEnt[ALEN(laOGVldEnt,1) , 1] ,;
                  laOGVld[ALEN(laOGVld , 1) , 1]      
        
        DIMENSION laOGVldVal[1,1]
        
      *E300900,1 Case of file
      CASE lcVldEntT = 'F'
      
        DIMENSION laOGVldFl[1]
        =gfSubStr(lcOGVldEnt , @laOGVldFl)
        
        *E300900,1 Get a temp. name to open the file with
        lcTmpFile = gfTempName()
        
        *E300900,1 Get the file path
        lcOGFlPath = IIF('\' $ laOGVldFl[1] , '' ,;
                         IIF(LEFT(laOGVldFl[1] , 2) = 'SY' , gcSysHome ,;
                             gcDataDir))
        
        lcSelect = SELECT()    && Save the work area
        DIMENSION laOGVldEnt[1,1] , laOGVld[1,1]
        
        lcOGCond = IIF(ALEN(laOGVldFl,1) > 3 , 'Where ' + laOGVldFl[4] , '')
        
        *E300900,1 Open the file with temp. name to prevent the SELECT-SQL
        *E300900,1 from opening the file with any unknown name
        USE &lcOGFlPath.&laOGVldFl[1] AGAIN ALIAS (lcTmpFile) IN 0
        
        SELECT &laOGVldFl[2] FROM &lcOGFlPath.&laOGVldFl[1] ;
          INTO ARRAY laOGVldEnt;
          &lcOGCond
        
        SELECT &laOGVldFl[3] FROM &lcOGFlPath.&laOGVldFl[1];
          INTO ARRAY laOGVld;
          &lcOGCond          
        
        *E300900,1 Close the file with the temp. alias (name)
        USE IN (lcTmpFile)
        
        SELECT(lcSelect)          && Restore the old work area
      
        DIMENSION laOGVldVal[1,1]
      
      *E300900,1 Case of array
      CASE lcVldEntT = 'A'
        
        *E300900,1 Get the 1st array name
        lcArrayNam = SUBSTR(lcOGVldEnt , 1 , ATC(lcValSep , lcOGVldEnt) - 1)
        DIMENSION laOGVldEnt[ALEN(&lcArrayNam , 1) , ALEN(&lcArrayNam , 2)]
        =ACOPY(&lcArrayNam , laOGVldEnt)
        
        *E300900,1 Get the 2nd array name
        lcArrayNam = SUBSTR(lcOGVldEnt , ATC(lcValSep,lcOGVldEnt) + 1)
        DIMENSION laOGVld[ALEN(&lcArrayNam , 1) , ALEN(&lcArrayNam , 2)]
        =ACOPY(&lcArrayNam , laOGVld)
        
        DIMENSION laOGVldVal[1,1]
        
    ENDCASE
    *E300900,1 Change these lines to add the capability [End]
    
  ENDIF   
ENDIF

*E300900,1 Change this line to add the capability to make valid entries
*E300900,1 from file or from array [Begin]

*IF llOGVld OR !EMPTY(lcOGVldEnt)

*E300900,1 If valid entries
IF !EMPTY(lcOGVldEnt)

  *E300900,1 Change this line to add the capability to make valid entries
  lnOldDim =ALEN(laOGVldVal,1)
  DECLARE laOldTarg[lnOldDim]
  IF ALEN(laOGVldVal,1) = 1 .AND. TYPE('laOGVldVal[1]')="L"
    laOGVldVal =' '
  ENDIF  

  *B602413,1 Set exact on [Begin
  PRIVATE lcExactSet
  lcExactSet = SET('EXACT')  && Save exact setting.
  SET EXACT ON               && Now exact is on.
  *B602413,1 Set exact on [End..
  *B801980,1 Hesham (Start)
  
*  DEFINE POPUP puOGSource  MARGIN RELATIVE SCROLL MARK CHR(16)
*  FOR lnCount = 1 TO ALEN('laOGVldEnt',1)
*    DEFINE BAR lnCount OF puOGSource PROMPT (ALLTRIM(laOGVldEnt[lnCount,1]))
*   *IF ASCAN('laOGVldVal',laOGVldEnt[lnCount]) > 0
*    
*    *If you have a mover screen that is displayed for a field taht has a valid codes and upon pressing MOVE --> OK, if you clicked again on the mover you'll NOT anything selected
*
*    *B602328,1 Fix inlist source and target for valid codes in second time you press [Begin]
*    *B602328,1 it to keep old selected values in mind.
*    *B602328,1 the following line does not work correct true.
*    *IF laOGVld[lnCount] $ &lcOGVar
*    *IF ALLTRIM(laOGVld[lnCount]) $ EVALUATE(lcOGVar)
*    
*    *B801961,1 Change this line to take into consideration the case of
*    *B801961,1 Numeric and Date fields (Or expressions) [Begin]
*    *IF laOGVld[lnCount] $ EVALUATE(lcOGVar)
*    
*    DO CASE
*      *B801961,1 Case of Numeric field (Or expression)
*      CASE lcOGDataTyp = 'N'
*        lcValue = STR(IIF(TYPE('laOGVld[lnCount]') = 'N' ,;
*                          laOGVld[lnCount] , EVALUATE(laOGVld[lnCount])) ,;
*                          lnLength , lnDecimal)
*        
*      *B801961,1 Case of Date field (Or expression)
*      CASE lcOGDataTyp = 'D'
*        lcValue = IIF(TYPE('laOGVld[lnCount]') = 'D' ,;
*                      DTOC(laOGVld[lnCount]) , laOGVld[lnCount])
*        
*      *B801961,1 Otherwise
*      OTHERWISE
*        
*        *B801961,1 pad the value that will be used in the search because we
*        *          have padded the user selection with the field
*        *          length, which may have length different than the length
*        *          of the values stored in the array [Begin]
*        lcValue = IIF(lnFldlenth > 0 ,;
*                      PADR(laOGVld[lnCount] , lnFldlenth) , laOGVld[lnCount])
*        
*    ENDCASE
*    
*    *B801961,1 If the value was selected
*    IF lcValue $ EVALUATE(lcOGVar)
*    *B801961,1 Change this line to take into consideration [End]
*    
*      *DIMENSION laOGVldVal[IIF(EMPTY(laOGVldVal[1,1]),1,ALEN(laOGVldVal,1)+1),1]      
*      IF !EMPTY(laOGVldVal[1,1])
*        DIMENSION laOGVldVal[ALEN(laOGVldVal,1)+1,1]      
*      ENDIF
*      *B602328,1 Fix inlist source and target for valid codes in second time you press [End  ]

*      *B801961,1 Change this line to take into consideration the case of
*      *B801961,1 Numeric and Date fields (Or expressions) [Begin]
*      *laOGVldVal[ALEN(laOGVldVal,1),1]=ALLTRIM(laOGVldEnt[lnCount,1])
*      laOGVldVal[ALEN(laOGVldVal , 1),1] = laOGVldEnt[lnCount,1]
*      *B801961,1 Change this line to take into consideration [End]
      
*      SET SKIP OF BAR lnCount OF puOGSource .T.
*    ENDIF
*  ENDFOR
 
   FOR lnCount = 1 TO ALEN('laOGVldEnt',1)
     
    DO CASE
       CASE lcOGDataTyp = 'N'
        lcValue = STR(IIF(TYPE('laOGVld[lnCount]') = 'N' ,;
                          laOGVld[lnCount] , EVALUATE(laOGVld[lnCount])) ,;
                          lnLength , lnDecimal)
        
       CASE lcOGDataTyp = 'D'
        lcValue = IIF(TYPE('laOGVld[lnCount]') = 'D' ,;
                      DTOC(laOGVld[lnCount]) , laOGVld[lnCount])
        
       OTHERWISE
        
        lcValue = IIF(lnFldlenth > 0 ,;
                      PADR(laOGVld[lnCount] , lnFldlenth) , laOGVld[lnCount])
        
    ENDCASE
    
    IF lcValue $ EVALUATE(lcOGVar)
      IF !EMPTY(laOGVldVal[1,1])
        DIMENSION laOGVldVal[ALEN(laOGVldVal,1)+1,1]      
      ENDIF
      laOGVldVal[ALEN(laOGVldVal , 1),1] = laOGVldEnt[lnCount,1]
      *B801980,1 Hesham (Start)
      laOGVldEnt[lnCount,1] = '\'+laOGVldEnt[lnCount,1]
      *B801980,1 Hesham (End)
    ENDIF
  ENDFOR

  *B602413,1 Restore old exact [Begin
  SET EXACT &lcExactSet
  *B602413,1 Restore old exact [End..

  STORE 1  TO lsOGSource,lsOGTarget
  pbExit = 2
ENDIF

DO inlist.spr

IF llOk
  
  *E300900,1 Change this line to add the capability to make valid entries
  *E300900,1 from file or from array [Begin]
  
  *IF !llOGVld AND EMPTY(lcOGVldEnt)
  
  *E300900,1 If not valid entries
  IF EMPTY(lcOGVldEnt)
  
  *E300900,1 Change this line to add the capability [End]
    
    &lcOGVar=lfGetStr('laOGVldVal')
    lcOGPrompt=IIF(EMPTY(ALLTRIM(&lcOGVar)),'In List',;
                    SUBSTR(STRTRAN(&lcOGVar,lcElmSep,','),1,20))
  ELSE
    
    *B602413,1 Set exact on [Begin
    PRIVATE lcExactSet
    lcExactSet = SET('EXACT')
    SET EXACT ON
    *B602413,1 Set exact on [End..
    
    STORE '' TO &lcOGVar,lcOGPrompt
    
    *B801961,1 Change these lines to take into consideration the case of
    *B801961,1 Numeric and Date fields (Or expressions) [Begin]
    *FOR lnOGCount=1 TO ALEN(laOGVldVal,1)
    *  
    *  *B602413,1 Alltrim expression to be null if empty  [Begin
    *  *lsOGSource=ASCAN(laOGVldEnt,laOGVldVal[lnOGCount,1])
    *  lsOGSource=ASCAN(laOGVldEnt,ALLTRIM(laOGVldVal[lnOGCount,1]))
    *  *B602413,1 Alltrim expression to be null if empty  [End..
    *  
    *  IF lsOGSource>0
    *    *B602413,1 Remove Alltrim from expression to fill array filter with true code [Begin
    *    *&lcOGVar=&lcOGVar+IIF(lnOGCount>1,lcElmSep,'')+ALLTRIM(laOGVld[lsOGSource,1])
    *    &lcOGVar=&lcOGVar+IIF(lnOGCount>1,lcElmSep,'') +laOGVld[lsOGSource,1]
    *    *lcOGPrompt=lcOGPrompt+IIF(lnOGCount>1,',','') +ALLTRIM(laOGVldEnt[lsOGSource,1])
    *    lcOGPrompt=lcOGPrompt+IIF(lnOGCount>1,',','')  +laOGVldEnt[lsOGSource,1]
    *    *B602413,1 Remove Alltrim from expression to fill array filter with true code [End..
    *  ENDIF
    *ENDFOR
    
    DO CASE
      
      *B801961,1 Case of Numeric field (Or expression)
      CASE lcOGDataTyp = 'N'
        
        *B801961,1 For loop to scan the selected entries array and get the
        *B801961,1 corresponding values
        FOR lnOGCount = 1 TO ALEN(laOGVldVal , 1)
          *B801980,1 Hesham (Start)
          lsOGSource = ASCAN(laOGVldEnt , ALLTRIM(laOGVldVal[lnOGCount,1]))
          lsOGSource = ASCAN(laOGVldEnt , '\'+ALLTRIM(laOGVldVal[lnOGCount,1]))
          *B801980,1 Hesham (End)
          IF lsOGSource>0
            
            lcValue    = STR(IIF(TYPE('laOGVld[lsOGSource,1]') = 'N' ,;
                                 laOGVld[lsOGSource,1] ,;
                                 EVALUATE(laOGVld[lsOGSource,1])) ,;
                             lnLength , lnDecimal)
            
            &lcOGVar   = &lcOGVar + IIF(lnOGCount > 1 , lcElmSep , '') +;
                         lcValue
            
            lcOGPrompt = lcOGPrompt + IIF(lnOGCount > 1 , ',' , '') +;
                         ALLTRIM(lcValue)
          ENDIF
        ENDFOR    && End of FOR lnOGCount = 1 TO ALEN(laOGVldVal , 1)
        
      *B801961,1 Case of Date field (Or expression)
      CASE lcOGDataTyp = 'D'
        
        *B801961,1 For loop to scan the selected entries array and get the
        *B801961,1 corresponding values
        FOR lnOGCount = 1 TO ALEN(laOGVldVal , 1)
          *B801980,1 Hesham (Start)
          *lsOGSource = ASCAN(laOGVldEnt , ALLTRIM(laOGVldVal[lnOGCount,1]))
          lsOGSource = ASCAN(laOGVldEnt , '\'+ALLTRIM(laOGVldVal[lnOGCount,1]))
          *B801980,1 Hesham (Start)
          IF lsOGSource>0
            lcValue    = IIF(TYPE('laOGVld[lsOGSource,1]') = 'D' ,;
                             DTOC(laOGVld[lsOGSource,1]) ,;
                             laOGVld[lsOGSource,1])
            
            &lcOGVar   = &lcOGVar + IIF(lnOGCount > 1 , lcElmSep , '') +;
                         lcValue
            
            lcOGPrompt = lcOGPrompt + IIF(lnOGCount > 1 , ',' , '') + lcValue
          ENDIF
        ENDFOR    && End of FOR lnOGCount = 1 TO ALEN(laOGVldVal , 1)
        
      *B801961,1 Otherwise
      OTHERWISE
        
        *B801961,1 For loop to scan the selected entries array and get the
        *B801961,1 corresponding values
        FOR lnOGCount = 1 TO ALEN(laOGVldVal , 1)
          *B801980,1 Hesham (Start)
          *lsOGSource = ASCAN(laOGVldEnt , ALLTRIM(laOGVldVal[lnOGCount,1]))
          lsOGSource = ASCAN(laOGVldEnt , '\'+ALLTRIM(laOGVldVal[lnOGCount,1]))
          *B801980,1 Hesham (End)
          IF lsOGSource>0
            
            *B801961,1 pad the returned value with the field length in order
            *          to pad the user selection with the field
            *          length [Begin]
            &lcOGVar   = &lcOGVar + IIF(lnOGCount > 1 , lcElmSep , '') +;
                         IIF(lnFldlenth > 0 ,;
                             PADR(laOGVld[lsOGSource,1] , lnFldlenth) ,;
                             laOGVld[lsOGSource,1])
            
            lcOGPrompt = lcOGPrompt + IIF(lnOGCount > 1 , ',' , '') +;
                         laOGVldEnt[lsOGSource,1]
          ENDIF
        ENDFOR    && End of FOR lnOGCount = 1 TO ALEN(laOGVldVal , 1)
        
    ENDCASE
    *B801961,1 Change these lines to take into consideration [End]
    
    *B602413,1 Restore old exact setting [Begin
    SET EXACT &lcExactSet
    *B602413,1 Restore old exact setting [End..
    
    lcOGPrompt=IIF(EMPTY(lcOGPrompt),'In List',lcOGPrompt)
  ENDIF                 
  
  * SHOW OBJECT _CUROBJ PROMPT lcOGPrompt

  *E301170,1 MAB 03/15/1999 Detect changes to OG Filter. [Begin
  IF !llOGFltCh
    llOGFltCh = !(lcOldVal == EVALUATE(lcOGVar))
  ENDIF
  *E301170,1 MAB 03/15/1999 Detect changes to OG Filter. [End

ENDIF

*-- end of main program.


*!*************************************************************************
*!
*!              Function: lfvAdd
*!
*!*************************************************************************
*   valid function to add  a new value to the list of values
FUNCTION lfvAdd
lcOGValue=lfInitialize()
SHOW GET lcOGValue ENABLE
llAdd=.T.
_CUROBJ=OBJNUM(lcOGValue)


*!*************************************************************************
*!
*!              Function: lfvChange
*!
*!*************************************************************************
*  valid function for the push button change to change one of the values
*  in the list
FUNCTION lfvChange
SHOW GET lcOGValue ENABLE
_CUROBJ=OBJNUM(lcOGValue)


*!*************************************************************************
*!
*!              Function: lfvRem
*!
*!*************************************************************************
*  this function to remove one value from the list of values
FUNCTION lfvRem
IF BETWEEN(lsOGSource,1,ALEN(laOGVldVal,1))
  IF ALEN(laOGVldVal,1)>1 
    =ADEL(laOGVldVal,lsOGSource)
    DIMENSION laOGVldVal(ALEN(laOGVldVal,1)-1,ALEN(laOGVldVal,2))
  ELSE
   STORE '' TO laOGVldVal
  ENDIF
ENDIF  
lsOGSource = MAX(lsOGSource-1,1)
SHOW GET lsOGSource
_CUROBJ=OBJNUM(lsOGSource)



*!*************************************************************************
*!
*!              Function: lfvAddToL
*!
*!*************************************************************************
*  this function to add the value to the list after it have been check
* that it is a valid value
FUNCTION lfvAddToL
lcOGVarName=VARREAD()
IF !EMPTY(&lcOGVarName)
  IF llAdd
    llAdd=.F.
    IF ASCAN(laOGVldVal,&lcOGVarName)=0
      IF !EMPTY(laOGVldVal[1,1])  
        DIMENSION laOGVldVal[ALEN(laOGVldVal,1)+1,ALEN(laOGVldVal,2)]    
      ENDIF  
      laOGVldVal[ALEN(laOGVldVal,1),1]=&lcOGVarName
    ENDIF
  ELSE
    IF ASCAN(laOGVldVal,&lcOGVarName)=0
      laOGVldVal[lsOGSource,1]=&lcOGVarName
    ENDIF
  ENDIF  
  SHOW GET &lcOGVarName DISABLE
  SHOW GET lsOGSource
ELSE
  SHOW GET &lcOGVarName DISABLE
  SHOW GET lsOGSource
  llAdd = .F.  
ENDIF  
  _CUROBJ=OBJNUM(lsOGSource)
  


*!*************************************************************************
*!
*!              Function: lfwVldList
*!
*!*************************************************************************
*  this function is the when function of the list of values
*  to change the displaying of the values on the screen
FUNCTION lfwVldList
IF !EMPTY(laOGVldVal[1,1])
  SHOW GET pbChange ENABLE
  SHOW GET pbRemove ENABLE
ELSE 
  SHOW GET pbChange DISABLE
  SHOW GET pbRemove DISABLE
ENDIF

IF lsOGSource>0 and lsOGSource <=alen(laOGVldVal,1)
  lcOGValue=laOGVldVal[lsOGSource,1]
ENDIF  

SHOW GET lcOGValue

*!*************************************************************************
*!
*!              Function: lfvOk
*!
*!*************************************************************************
*  valid function for the ok push button
FUNCTION lfvOk
llOk=.T.
CLEAR READ

*!*************************************************************************
*!
*!              Function: lfInitialize
*!
*!*************************************************************************
*
FUNCTION lfInitialize
DO CASE
  CASE lcOGDataTyp $ 'CM'
    *B601173,1 Hesham El-Sheltawi (Start)
    *B601173,1 change the defaulting value function to intialize the get field
    *B601173,1 with only one space in case of getting values for character fields
*    RETURN SPACE(lnOGSize)
    RETURN ' ' 
    *B601173,1 Hesham El-Sheltawi (End)    
  CASE lcOGDataTyp='N'
    RETURN 0
  CASE lcOGDataTyp='D'
    RETURN CTOD('')
ENDCASE  

*!*************************************************************************
*!
*!              Function: lfInitVal
*!
*!*************************************************************************
*  this function is to Initialize the array of the list from the variable
*  needed to be changed
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


*!*************************************************************************
*!
*!              Function: lfTrnsStr
*!
*!*************************************************************************
*  this function to change the values to string
*
FUNCTION lfTrnsStr
PARAMETER lcOGValue
DO CASE
  CASE lcOGDataTyp $ 'CM'
    
    *B801961,1 Change this line to apply the picture on the returned value
    *          because we have padded the user selection with the field
    *          length, which may have different length than the
    *          picture length [Begin]
    *lcOGPicture
    *RETURN lcOGValue
    RETURN IIF(TYPE('lcOGPicture') = 'C' .AND. !EMPTY(lcOGPicture) ,;
               TRANSFORM(lcOGValue , lcOGPicture) , lcOGValue)
    *B801961,1 Change this line to apply the picture on the returned [End]
    
  CASE lcOGDataTyp = "N"
    
    *B801961,1 Change this line to use the EVALUATE() function because the
    *B801961,1 VAL() function is effected by SET DECIMALS Command [Begin]
    *RETURN VAL(lcOGValue)     
    RETURN IIF(EMPTY(lcOGValue) , 0 , EVALUATE(lcOGValue))
    *B801961,1 Change this line to use the EVALUATE() function [End]
    
  CASE lcOGDataTyp = 'D'
    RETURN CTOD(lcOGValue)     
  CASE lcOGDataTyp = 'L'
    RETURN lcOGValue  
ENDCASE  



FUNCTION lfGetStr
PARAMETERS lcOGArrName
lcOGRetVal=''
FOR lnOGCount=1 TO ALEN(&lcOGArrName,1)
  lcOGRetVal = lcOGRetVal+IIF(lnOGCount=1,'',lcElmSep)+;
               lfChngToStr(&lcOGArrName[lnOGCount,1])
ENDFOR
RETURN lcOGRetVal

FUNCTION lfChngToStr
PARAMETER lcOGValue
DO CASE
  CASE lcOGDataTyp $ 'CM'
    
    *B801961,1 Change this line to pad the returned value with the field
    *          length inorder pad the user selection with the field
    *          length [Begin]
    *RETURN lcOGValue
    RETURN IIF(lnFldlenth > 0 , PADR(lcOGValue , lnFldlenth) , lcOGValue)
    *B801961,1 Change this line to pad the returned value [End]
    
  CASE lcOGDataTyp = "N"
    
    *B801961,1 Change this line to take into consideration Numeric
    *B801961,1 field Picture [Begin]
    *RETURN ALLTRIM(STR(lcOGValue))     
    RETURN STR(lcOGValue , ALLTRIM(STR(lnLength)) , ALLTRIM(STR(lnDecimal)))
    *B801961,1 Change this line to take into consideration [End]
    
  CASE lcOGDataTyp = 'D'
    RETURN DTOC(IIF(EMPTY(lcOGValue),CTOD("  /  /  "),lcOGValue))     
  CASE lcOGDataTyp = 'L'
    RETURN lcOGValue
ENDCASE  

*!*************************************************************************
*!
*!              Function: lfvCancel
*!
*!*************************************************************************
*  this function is the valid function of cancel push button
*
FUNCTION lfvcancel







*!*************************************************************************
*!
*!              Function: lfShow
*!
*!*************************************************************************
*  the show function for the screen 
FUNCTION lfShow

*E300900,1 Change this line to add the capability to make valid entries
*E300900,1 from file or from array [Begin]

*IF llOGVld OR !EMPTY(lcOGVldEnt)

*E300900,1 If valid entries
IF !EMPTY(lcOGVldEnt)

*E300900,1 Change this line to add the capability [End]
  
  IF ALEN('laOGVldVal',1) = ALEN('laOGVldEnt',1)  AND !EMPTY(laOGVldVal[1]);
    OR EMPTY(laOGVldEnt[1])
    SHOW GET lsOGSource     DISABLE
    SHOW GET pbMove       DISABLE
    SHOW GET pbAll        DISABLE
  ELSE
    SHOW GET lsOGSource     ENABLE
    SHOW GET pbMove       ENABLE
    SHOW GET pbAll        ENABLE
  ENDIF  
  
  IF EMPTY(laOGVldVal[1])
    SHOW GET lsOGTarget    DISABLE
    SHOW GET pbRemove    DISABLE
    SHOW GET pbRAll      DISABLE
  ELSE
    SHOW GET lsOGTarget    ENABLE
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

IF lsOGSource <= ALEN('laOGVldEnt',1) AND lsOGSource <> 0
  *SET SKIP OF BAR lsOGSource OF puOGSource .T.
  
  IF !EMPTY(laOGVldVal[1,1]) 
    DIMENSION laOGVldVal[ALEN(laOGVldVal)+1,1]
  ENDIF
  
  *B801961,1 Change this line to take into consideration the case of
  *B801961,1 Numeric and Date fields (Or expressions) [Begin]
  *laOGVldVal[ALEN(laOGVldVal),1]= ALLTRIM(laOGVldEnt[lsOGSource,1])
  laOGVldVal[ALEN(laOGVldVal),1] = laOGVldEnt[lsOGSource,1]
  *B801961,1 Change this line to take into consideration [End]
  *B801980,1 Hesham (Start)
  *B801980,1 disable the selected bar
  laOGVldEnt[lsOGSource,1] = '\'+laOGVldEnt[lsOGSource,1]
  *B801980,1 Hesham (End)
ENDIF  

lnStart  = lsOGSource
lsOGSource = 0

*B801980,1 Hesham (Start)
*B801980,1 deal with the array insted of the popup
*FOR lnCount = lnStart TO CNTBAR('puOGSource')
*  IF !SKPBAR('puOGSource',lnCount)
*    lsOGSource = lnCount 
*    EXIT
*  ENDIF  
*ENDFOR

*IF lsOGSource = 0
*  FOR lnCount = 1 TO CNTBAR('puOGSource')
*    IF !SKPBAR('puOGSource',lnCount)
*      lsOGSource = lnCount 
*      EXIT
*    ENDIF  
*  ENDFOR
*ENDIF  

FOR lnCount = lnStart TO ALEN(laOGVldEnt,1)
  IF LEFT(laOGVldEnt[lnCount,1],1)<>'\'
    lsOGSource = lnCount 
    EXIT
  ENDIF  
ENDFOR

IF lsOGSource = 0
  FOR lnCount = 1 TO ALEN(laOGVldEnt,1)
  IF LEFT(laOGVldEnt[lnCount,1],1)<>'\'
    lsOGSource = lnCount 
    EXIT
  ENDIF  
  ENDFOR
ENDIF  
*B801980,1 Hesham (End)

_CUROBJ = OBJNUM(lsOGSource)
SHOW GETS

*!*************************************************************************
*!
*!              Function: lfvTarget
*!
*!*************************************************************************
*
FUNCTION lfvTarget

IF lsOGTarget <= ALEN('laOGVldVal',1) AND lsOGTarget <> 0
*B801980,1 Hesham (Start)
*  lsOGSource  = ASUBSCRIPT('laOGVldEnt',;
                   ASCAN('laOGVldEnt',laOGVldVal[lsOGTarget,1]),1)
  lsOGSource  = ASUBSCRIPT('laOGVldEnt',;
                   ASCAN('laOGVldEnt','\'+laOGVldVal[lsOGTarget,1]),1)

*B801980,1 Hesham (End)
*B801980,1 Hesham (Start)
*  SET MARK OF POPUP puOGSource .F.
*  SET SKIP OF BAR lsOGSource OF puOGSource .F.
  laOGVldEnt[lsOGSource,1] = SUBSTR(laOGVldEnt[lsOGSource,1],2)
*B801980,1 Hesham (End)
  =ADEL(laOGVldVal,lsOGTarget)
  IF ALEN(laOGVldVal,1) > 1
    DIMENSION laOGVldVal[ALEN(laOGVldVal)-1,1]
  ELSE
    laOGVldVal[1,1] =' '
  ENDIF  
ENDIF


_CUROBJ = OBJNUM(lsOGTarget)
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
    _CUROBJ = OBJNUM(lsOGSource)
    KEYBOARD "{ENTER}"
  CASE lnMovmnts = 2
  *B801980,1 Hesham (Start)
    *DECLARE laOGVldVal[ALEN('laOGVldEnt',1),1]
    *=ACOPY(laOGVldEnt,laOGVldVal)
    *SET SKIP OF POPUP puOGSource .T.
    FOR lnMovement = 1 TO ALEN(laOGVldEnt,1)
      IF LEFT(laOGVldEnt[lnMovement,1],1)!='\'
        lsogSource = lnMovement
        =lfvSource()
      ENDIF
    ENDFOR
    *B801980,1 Hesham (End)
    SHOW GETS
  CASE lnMovmnts = 3
    _CUROBJ = OBJNUM(lsOGTarget)
    KEYBOARD "{ENTER}"
  CASE lnMovmnts = 4
    DECLARE laOGVldVal[1,1]        
    laOGVldVal =' '
    *B801980,1 Hesham (Start)
*    SET SKIP OF POPUP puOGSource .F.
    FOR lnMovement = 1 TO ALEN(laOGVldEnt,1)
      IF LEFT(laOGVldEnt[lnMovement,1],1)='\'
        laOGVldEnt[lnMovement,1] = SUBSTR(laOGVldEnt[lnMovement,1],2)
      ENDIF
    ENDFOR
    *B801980,1 Hesham (End)
    SHOW GETS
ENDCASE

