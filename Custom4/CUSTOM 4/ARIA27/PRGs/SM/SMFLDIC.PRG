*:************************************************************************
*:
*: Procedure file: SMFLDIC.PRG 
*:                 (File dictionary)
*:
*:         System: ARIA ADVANTAGE SYRESE
*:         Module: SYSTEM MANAGER
*:         Author: Reham Aly Alallamy
*:      Copyright (c) 
*:************************************************************************
* Last modified:  /  /
*
*B600655,1 Reham On 08/23/95
*B600655,1 1- Add the changing of tags names in the fixing array
*B600655,1    to prevent the error "Subscript out of bounds"
*B600655,1 2- Set default for the defined by radio button at the beginning.
*B601701,1 Hesham El-Sheltawi 04/08/97
*B601701,1 if the file name len is less than 8 char. the file fields
*B601701,1 does not work properly
*E300683,4 AHMED & RENEE 06/08/97 Add prgs directory path to the calling of programs
*E300632,4 HESHAM 04/23/97 Add 2 fields in File Field screen for the sequence
*E300632,4 				   number; if a field requires one, and initial value.
*B601910,1 Hesham 11/27/97
*B601910,1 some times the program can not recognize that the audit fields
*B601910,1 is already exist in the file structure so we have to 
*B601910,1 Change the audit fields array to be identical in structure
*B601910,1 like the array that hold the file fields so if we scan in
*B601910,1 the file fields array the ascan function work properly    
*E301077,78 Hesham 02/25/1999
*E301077,78 Controlling opening and clossing files.
*E301077,78 to make the program open the files when ever it need it
*B604857,1 ADEL 09/04/01 Let the user add his own fields to the browse ones set by Aria.
*B604857,4 ADEL 09/13/01 Fix some bugs.
*B040236,1 TMI 06/19/2006 Fixing the bug that after adding a user defined field to 
*B040236,1 			      a standard table the  cupgrdlvl field is changed to 'A'
*B607972,1 TMI 02/13/2007 Fix a bug that when adding a new custom field using the table screen from Data dictionary it updates the new field with wrong upgrdlvl
*:**************************************************************************************
*** Declaration of variables to be public on the gpSetup procedure
EXTERNAL ARRAY laData,laKeyField,laScrMode,laDefProc
DIMENSION laPhFld[1,1],laLogPhFld[1,1],;
          laLogFld[1,1],laIndxFld[1,4],;
          laAppl[1],laKeyField[1,4],;
          laTAudit[7,1],laWndObj[4,3],;
          laNewFiles[1,1],laTags[1,1],;
          laStruc[1,1],laFixTag[1,3]
          
laDefProc[7] = .F.     && Don't use the default delete procedure
laDefProc[9] = .F.     && Don't use the default save procedure

laKeyField[1,1] = 'laData[1]'
laKeyField[1,2] =.T.
laKeyField[1,3] = 'CFILE_NAM'
laKeyField[1,4] = 1

lcTFilFld  = "Fields"
lcTFilTag  = "Tags"
lcMstr_Tag = " "

lcNewTag   = SPACE(10)
*lcNewTag   = " "

lcIndx_ex  = " "
lcDesc     = " "
cbUniq     = .F.
rbIndxDir  = 1

lcShowRef  = ""

rbDfnBy    = IIF(EMPTY(GETENV('DEVELOP')),3,2)

lsField    = 1
lsTags     = 1
lnIndRecNo = 0
lcStatus   = " "
lcTag_Nam  = " "
lcFieldStr = " "
lcOldTag   = " "
lcAddNewFd = " "

llFixTag   = .F.
llNewTag   = .F.
llClnStatc = .F.
llChar     = .T.
llBrowse   = .F.

lcIndxTmp  = " "   && temp. file name that store assigned indexes
*lcDefData  = " "   && temp. file to hold the default data in a memo field
lcOldDfTmp = " "   && temp. file name for the old default data
lcNewDfTmp = " "   && temp. file name for the new default data
lcMemoTxt  = " "   && temp. text file for saving the memo filed
                   && of the default data.
IF !gfSetup()    
  RETURN
ENDIF  
*B604857,1 (Begin) Let the user add his own fields to the browse ones set by Aria.
lcScFields = STRTRAN(lcScFields,'MBROW_FLD','MUSR_BROW')
*B604857,1 (End)

laWndObj[1,1]  = gcBaseWind
laWndObj[1,2]  = 'IBBROWKEY'
laWndObj[1,3]  = 'LADATA[10]'

laWndObj[2,1]  = 'CWDSMFLFLD'
laWndObj[2,2]  = 'LSFIELD'
laWndObj[2,3]  = 'PBFLCLOSE'

laWndObj[3,1]  = 'CWDSMFLTAG'
laWndObj[3,2]  = 'LSTAGS'
laWndObj[3,3]  = 'PBTGCLOSE'

laWndObj[4,1]  = 'GWCCONTRL1'
laWndObj[4,2]  = 'PBTOP'
laWndObj[4,3]  = 'PBCLS'

DECLARE laTAudit[7,1]
laTAudit[1,1] = "CADD_USER"
laTAudit[2,1] = "CADD_TIME"
laTAudit[3,1] = "DADD_DATE"
laTAudit[4,1] = "LLOK_STAT"
laTAudit[5,1] = "CLOK_USER"
laTAudit[6,1] = "DLOK_DATE"
laTAudit[7,1] = "CLOK_TIME"

IF !WEXIST(lcBaseWind)    && If the window is already exist don't
                          && change any thing                                          
  SCATTER  FIELDS &lcScFields MEMO TO laData BLANK 
  lcAddNewFd = gfTempName()
  lcIndxTmp  = gfTempName()
  lcOldDfTmp = gfTempName()
  lcNewDfTmp = gfTempName()
  lcMemoTxt  = gfTempName()
*  lcDefData  = gfTempName()
  *E301077,78 Hesham (Start) 
  =gfOpenFile(gcSysHome+'SYDAPPL','CAPP_ID')
  *E301077,78 Hesham (End) 
  *B604857,4 (Begin) Fix the bug of 'alias Sydobjct not found' when saving.
  =gfOpenFile(gcSysHome+'sydObjct','Capp_id')
  *B604857,4 (End)
  SELECT sydField
  FOR lnCount = 1 TO 7
    IF SEEK(ALLTRIM(laTAudit[lnCount,1]))
    *B601910,1 Hesham (Start)
    *B601910,1 Change the audit fields array to be identical in structure
    *B601910,1 like the array that hold the file fields so if we scan in
    *B601910,1 the file fields array the ascan function work properly    
*      laTAudit[lnCount,1] = ALLTRIM(sydField.cFld_name) + ;
                            SPACE(11-LEN(RTRIM(sydField.cFld_name))) +;
                            IIF(EMPTY(sydField.cfld_head),;
                            sydField.cFld_Name,sydField.cfld_head)+PADR('~',10)+'F'
                            
                            
      laTAudit[lnCount,1] = RTRIM(sydField.cFld_name) +;
                            SPACE(11-LEN(RTRIM(sydField.cFld_name))) +;
                            IIF(EMPTY(sydField.cfld_head),;
                            sydField.cFld_Name+SPACE(34-LEN(sydField.cFld_Name)),;
                            sydField.cfld_head)+ALLT('~'+PADR(0,10)+'F')
    *B601910,1 Hesham (End)                            
    ENDIF
  ENDFOR

  DEFINE POPUP puField MARGIN MOVER RELATIVE SCROLL MARK CHR(16)

  STORE SPACE(1) TO laPhFld[1,1],laAppl[1],laLogPhFld[1,1],;
                    laLogFld[1,1],laTags[1,1],laFixTag

  SELECT SydIndex
  =AFIELDS(laIndxFld)
  lnIndxFld = ALEN(laIndxFld,1)
  DIMENSION laIndxFld[lnIndxFld+2,4]
 
  laIndxFld[lnIndxFld+1,1] = 'nRecNo'
  laIndxFld[lnIndxFld+1,2] = 'N'
  laIndxFld[lnIndxFld+1,3] = 10
  laIndxFld[lnIndxFld+1,4] = 0

  laIndxFld[lnIndxFld+2,1] = 'cStatus'
  laIndxFld[lnIndxFld+2,2] = 'C'  
  laIndxFld[lnIndxFld+2,3] = 1
  laIndxFld[lnIndxFld+2,4] = 0

  CREATE TABLE (gcWorkDir+lcIndxTmp) FROM ARRAY laIndxFld
  
  lcFieldStr = "&lcIndxTmp..cFile_Tag"

  *** This cursor will be used in case of ***
  *** branching to the field dictionary.. ***
  CREATE CURSOR (lcAddNewFd) ;
         (cFld_name C(10),cFld_head C(25),;
          nScrMode N(1),cFile_nam C(8),mFil_Strc M(10),lAdd_New L(1))
  APPEND BLANK
  
*  CREATE TABLE (gcWorkDir+lcDefData) (mDefData M)
*  APPEND BLANK
ELSE
  IF !EMPTY(lcAddNewFd)
    SELECT (lcAddNewFd)
    IF &lcAddNewFd..lAdd_New
      IF !EMPTY(laLogPhFld[1,1])
        DIMENSION laLogPhFld [ALEN(laLogPhFld,1)+1,1]
      ENDIF
      laLogPhFld [ALEN(laLogPhFld,1),1] = ;
                 RTRIM(&lcAddNewFd..cFld_name) + ;
                 SPACE(11-LEN(RTRIM(&lcAddNewFd..cFld_name))) + ;
                 RTRIM(&lcAddNewFd..cFld_head)
      =gfFillpop("puField","laLogPhFld")
      lsField = ALEN(laLogPhFld,1)
      REPLACE lAdd_New WITH .F.
    ENDIF
  ENDIF
ENDIF

*B600655,1 Reham On 08/23/95   *** Begin ***
*B600655,1 Set default for the defined by radio button at the beginning.
*rbDfnBy = IIF(EMPTY(GETENV('DEVELOP')),3,AT(laData[7],'SAU'))
rbDfnBy = IIF(EMPTY(GETENV('DEVELOP')),3,IIF(!EMPTY(laData[7]),AT(laData[7],'SAU'),2))
*B600655,1 Reham On 08/23/95   *** End   ***

SELECT sydFiles

*E300683,4 Call *.SPR from screens directory
* DO SMFlDic.SPR 
DO (gcScrDir + gcWinAppl + '\SMFlDic.SPR')
*E300683,4 end   
IF glQuitting
  IF WEXIST('CWDSMFLFLD')
    RELEASE WINDOW CWDSMFLFLD
  ENDIF
  
  IF WEXIST('CWDSMFLTAG')
    RELEASE WINDOW CWDSMFLTAG
  ENDIF
  
  IF USED(lcAddNewFd)
    USE IN &lcAddNewFd
  ENDIF  
  ERASE (gcWorkDir+lcAddNewFd+'.DBF')

  IF USED(lcIndxTmp)
    USE IN &lcIndxTmp
  ENDIF  
  ERASE (gcWorkDir+lcIndxTmp+'.DBF')
  ERASE (gcWorkDir+lcIndxTmp+'.CDX')
  ERASE (gcWorkDir+lcIndxTmp+'.FPT')

  IF USED(lcNewDfTmp)
    USE IN &lcNewDfTmp
  ENDIF  
  ERASE (gcWorkDir+lcNewDfTmp+'.DBF')
  ERASE (gcWorkDir+lcNewDfTmp+'.FPT')

  IF USED(lcOldDfTmp)
    USE IN &lcOldDfTmp
  ENDIF  
  ERASE (gcWorkDir+lcOldDfTmp+'.DBF')
  ERASE (gcWorkDir+lcOldDfTmp+'.FPT')

*  IF USED(lcDefData)
*    USE IN &lcDefData
*  ENDIF  
*  ERASE (gcWorkDir+lcDefData+'.DBF')
*  ERASE (gcWorkDir+lcDefData+'.FPT')

  IF USED(lcMemoTxt)
    USE IN &lcMemoTxt
  ENDIF  
  ERASE (gcWorkDir+lcMemoTxt+'.DBF')

ELSE
  IF !EMPTY(laLogPhFld[1,1])
    =gfPopArang("puField",@laLogPhFld)
  ENDIF  
ENDIF

*!***************************************************************
*!
*!      Procedure: lpShow
*!
*!****************************************************************
*
PROCEDURE lpShow

EXTERNAL ARRAY laScrMode,laPhFld,laData,laLogPhFld,laLogFld,laTags,laFixTag

lcMstr_Tag = laData[8]
llClnStatc = .F.
llFixTag   = .F.

DO CASE
  CASE laScrMode[1]
    rbDfnBy    = IIF(EMPTY(GETENV('DEVELOP')),3,2)
    lcNewTag   = SPACE(10)
    *lcNewTag   = " "
    cbUniq     = .F.
    rbIndxDir  = 1
    lcIndx_ex  = " "
    lcDesc     = " "

    DIMENSION laPhFld[1,1],laAppl[1],laLogPhFld[1,1],;
              laLogFld[1,1],laStruc[1,1],laTags[1,1],laFixTag[1,3]

    STORE SPACE(1) TO laPhFld,laAppl,laLogPhFld,laLogFld,;
                      laStruc,laTags,laFixTag

    RELEASE BAR ALL OF puField

    SELECT (lcIndxTmp)
    lsTags     = 1
    SHOW GET lsTags ENABLE
    ZAP
    lcSaveErr = ON("ERROR")
    ON ERROR lnDumm = 1
    SHOW GET lsTags
    ON ERROR &lcSaveErr

  CASE laScrMode[2]  .OR. laScrMode[3]       && View or Edit mode 
    
    rbDfnBy = AT(laData[7],'SAU')
        
    STORE SPACE(1) TO laPhFld,laAppl,laLogPhFld,laLogFld,laStruc
    
    *** Get phisical & logical names for fields ***
    *** assigned to this file. ***
    *B601701,1 Hesham (Start)
    *B601701,1 change the where condition to select the file with complete
    *B601701,1 8 char. name
*    SELECT DISTINCT RTRIM(sydField.cFld_name) +;
                    SPACE(11-LEN(RTRIM(sydField.cFld_name))) +;
                    IIF(EMPTY(sydField.cfld_head),;
                    sydField.cFld_Name+SPACE(34-LEN(sydField.cFld_Name)),;
                    sydField.cfld_head);
           FROM (gcSysHome+"sydField"),(gcSysHome+"sydFlfld");
           WHERE sydFlfld.cFld_name = sydField.cFld_name;
                 AND sydFlfld.cFile_nam = ALLTRIM(laData[1]);
           ORDER BY sydFlfld.nFld_pos;
           INTO ARRAY laLogPhFld

    SELECT DISTINCT RTRIM(sydField.cFld_name) +;
                    SPACE(11-LEN(RTRIM(sydField.cFld_name))) +;
                    IIF(EMPTY(sydField.cfld_head),;
                    sydField.cFld_Name+SPACE(34-LEN(sydField.cFld_Name)),;
                    sydField.cfld_head)+ALLT('~'+PADR(sydflfld.ndef_seq,10)+;
                    IIF(sydflfld.lenumerate,'T','F'));
           FROM (gcSysHome+"sydField"),(gcSysHome+"sydFlfld");
           WHERE sydFlfld.cFld_name = sydField.cFld_name;
                 AND sydFlfld.cFile_nam = laData[1];
           ORDER BY sydFlfld.nFld_pos;
           INTO ARRAY laLogPhFld
           
    *B601701,1 (End)
    *** Get the phisical names of the fields. ***
    DECLARE laPhFld [ALEN(laLogPhFld,1),1]
    DECLARE laLogFld[ALEN(laLogPhFld,1),1]
    FOR lnCount = 1 TO ALEN(laLogPhFld,1)
      laPhFld [lnCount,1] = ALLTRIM(SUBSTR(laLogPhFld[lnCount,1],1,11))
      laLogFld[lnCount,1] = ALLTRIM(SUBSTR(laLogPhFld[lnCount,1],13))
    ENDFOR
    
    *** Hold this tell we decide something for the default data. ***
*    DECLARE laStruc[ALEN(laPhFld,1),1]
*    =ACOPY(laPhFld,laStruc)
    
    IF !EMPTY(laLogPhFld[1,1])
      =gfFillpop("puField","laLogPhFld")
    ENDIF

    *** List of tags. ***
    SELECT cFile_tag,cFile_tag," " ;
           FROM (gcSysHome+"SydIndex") ;
           INTO ARRAY laFixTag;
           WHERE cFile_Nam = laData[1]
    SELECT cFile_tag ;
           FROM (gcSysHome+"SydIndex") ;
           INTO ARRAY laTags;
           WHERE cFile_Nam = laData[1]
    
    IF EMPTY(laTags[1,1])
      laFixTag    = " "
      laTags[1,1] = " "
    ELSE
      DIMENSION laTags[ALEN(laTags,1)+1,1]
      =AINS(laTags,1)
      laTags[1,1] = " "
    ENDIF
    
    SELECT *,RECNO() AS 'nRecNo' , "S" AS 'cStatus' ;
           FROM (gcSysHome+"SydIndex") ;
           INTO DBF (gcWorkDir+lcIndxTmp);
           WHERE cFile_Nam = laData[1]

    SELECT (lcIndxTmp)
    laData[5] = RECCOUNT()
    lsTags    = 1
    lcNewTag  = &lcIndxTmp..cFile_tag
    cbUniq    = &lcIndxTmp..lUnique
    rbIndxDir = IIF(&lcIndxTmp..lAscend , 1 , 2)
    lcIndx_ex = &lcIndxTmp..cIndx_exp
    lcDesc    = &lcIndxTmp..mIndx_des
    IF _TALLY = 0 
      SHOW GET lcNewTag   DISABLE
      SHOW GET lcDesc     DISABLE
      SHOW GET pbZoomTag  DISABLE
      SHOW GET puFld_typ  DISABLE
      SHOW GET pbRemTags  DISABLE
      SHOW GET lnexpr     DISABLE
      SHOW GET rbIndxDir  DISABLE
      SHOW GET cbuniq     DISABLE
    ENDIF  
    
    *** List of applictions using this file
    lcTmpStr = ALLTRIM(sydFiles.mFile_App)
    SELECT CAPP_NAME FROM (gcSysHome+"SYDAPPL") ;
           WHERE CAPP_ID $ lcTmpStr ;
           INTO ARRAY laAppl
    
    lcSavExact = SET("EXACT")
    SET EXACT ON
    puFld_Typ = IIF(ASCAN(laTags,laData[8])>0,ASCAN(laTags,laData[8])," ")
    SET EXACT &lcSavExact
    
    IF laScrMode[3]
      llFound    = .F.
      lcExactSav = SET("EXACT")
      SET EXACT ON
      FOR lnCount = 1 TO ALEN(laTAudit,1)
        IF ASCAN(laLogPhFld,laTAudit[lnCount,1]) = 0
          llFound = .T.
          EXIT
        ENDIF
      ENDFOR
      IF llFound
        *** There is one or more audit information field(s). ***
        *** are not exist in this file strucure.  Do you want ***
        *** want to add them...
        lnOption = gfModalGen("QRM00131B00006","DIALOG")
        IF lnOption = 1
          FOR lnCount = 1 TO ALEN(laTAudit,1)
            IF ASCAN(laLogPhFld,laTAudit[lnCount,1]) = 0
              IF !EMPTY(laLogPhFld[1,1])
                DIMENSION laLogPhFld [ALEN(laLogPhFld,1)+1,1]
              ENDIF
              laLogPhFld [ALEN(laLogPhFld,1),1] = laTAudit[lnCount,1]
            ENDIF
          ENDFOR
          =gfFillpop("puField","laLogPhFld")
          lsField = ALEN(laLogPhFld,1)
        ENDIF
      ENDIF
      SET EXACT &lcExactSav
    ENDIF
  CASE laScrMode[4]
    laData[7] = SUBSTR('SAU',rbDfnBy,1)
    
    SHOW GET lcNewTag   DISABLE
    SHOW GET lcDesc     DISABLE
    SHOW GET pbZoomTag  DISABLE
    SHOW GET puFld_typ  DISABLE
    SHOW GET pbRemTags  DISABLE
    SHOW GET lnexpr     DISABLE
    SHOW GET rbIndxDir  DISABLE
    SHOW GET cbuniq     DISABLE
ENDCASE

IF laScrMode[3] .OR. laScrMode[4]
  DO lpRdspSays
ENDIF

*IF laScrMode[1] .OR. laScrMode[4]
  ***
*  REPLACE &lcDefData..mDefData WITH ""
*ELSE
*  REPLACE &lcDefData..mDefData WITH sydFiles.mFldfdata
*ENDIF

SELECT SYDFILES

*!***********************************************************
*!
*!      Function lfvMntFld
*!
*!************************************************************
*
FUNCTION lfvMntFld

DO CASE
  CASE pbField = 1     && Insert a field
    DECLARE laNewFiles[1,1]
    *** List of fields assigned to this file
    SELECT RTRIM(sydField.cFld_name) +;
           SPACE(11-LEN(RTRIM(sydField.cFld_name))) +;
           IIF(EMPTY(sydField.cfld_head),;
           sydField.cFld_Name+SPACE(34-LEN(sydField.cFld_Name)),;
           sydField.cfld_head);
      FROM (gcSysHome+"sydField");
      WHERE ASCAN(laLogPhFld,cFld_Name) = 0;
      ORDER BY cFld_Name;
      INTO ARRAY laNewFiles

    IF !EMPTY(laNewFiles [1,1])
      STORE 0 TO lsFl_List,pbSelect
      *** Dialog to display list of unassigned fields
      *E300683,4 Call programs from PRGS directory
      *DO SMFlLst.PRG 
      DO (gcAppHome + gcWinAppl + '\SMFlLst.PRG ') 
      *E300683,4 end

      *** If a field was selected
      DO CASE
        CASE lsFl_List > 0    && Select
          *** Add the field to the array hold the strucure. ***
          IF !EMPTY(laLogPhFld[1,1])
            DIMENSION laLogPhFld [ALEN(laLogPhFld,1)+1,1]
          ENDIF
          laLogPhFld [ALEN(laLogPhFld,1),1] = laNewFiles[lsFl_List,1]+PADR('~',10)+'F'
          =gfFillpop("puField","laLogPhFld")
          lsField = ALEN(laLogPhFld,1)
          
          SELECT sydField
        *** If the user choose to add a new field
        CASE lsFl_List < 0     && New
          lcFld_Nam = SPACE(10)
          *** This branching is for adding new field. ***
          SELECT (lcAddNewFd)
          REPLACE cFld_name WITH SPACE(10) ;
                  cFld_head WITH SPACE(25) ;
                  nScrMode  WITH 4 ;
                  cFile_nam WITH laData[1] ;
                  lAdd_New  WITH .F.
          =gfstatic()
          *E300683,4 Pass parameters to gpDoProg
          *DO gpDoProg WITH 'AWRSMFDDIC WITH "&lcAddNewFd"'
          DO gpDoProg WITH 'AWRSMFDDIC',.F.,'SM',"'"+lcAddNewFd+"'"
          *E300683,4 end
          IF &lcAddNewFd..lAdd_New
            IF ASCAN(laLogPhFld,&lcAddNewFd..cFld_name) = 0
              IF !EMPTY(laLogPhFld[1,1])
                DIMENSION laLogPhFld [ALEN(laLogPhFld,1)+1,1]
              ENDIF
              laLogPhFld[ALEN(laLogPhFld,1),1]=RTRIM(&lcAddNewFd..cFld_name)+;
                       SPACE(11-LEN(RTRIM(&lcAddNewFd..cFld_name))) +;
                       IIF(EMPTY(&lcAddNewFd..cfld_head),;
                       &lcAddNewFd..cFld_Name+;
                       SPACE(34-LEN(&lcAddNewFd..cFld_Name)),;
                       &lcAddNewFd..cfld_head)+PADR('~',10)+'F'
              =gfFillpop("puField","laLogPhFld")
              lsField = ALEN(laLogPhFld,1)
            ENDIF
          ENDIF
          SELECT sydFiles
      ENDCASE
    ENDIF 

  *** Remove field
  CASE  pbField = 2    && Remove field

    IF lsField <> 0 AND !EMPTY(laLogPhFld[1,1])
      IF ASCAN(laTAudit,laLogPhFld[lsField,1]) <> 0
        *** You cannot remove any audit information ***
        *** field from the structure of any database. ***
        *** <  Ok  > ***
        =gfModalGen("TRM00120B00000","ALERT")
      ELSE
        lnOption = gfModalGen('QRM00007B00007','ALERT'," ")  && Confirm
        DO CASE
          CASE lnOption = 1             && Remove
            SELECT (lcIndxTmp)
            lnTmpRec = RECNO()
            LOCATE FOR laPhFld[lsField,1] $ cIndx_exp
            lcTag_nam = cFile_Tag 
            IF lnTmpRec <=RECCOUNT()
              GO lnTmpRec
            ENDIF
            IF FOUND()
              *** This field is involved in tag {Tag name} ***
              = gfModalGen('QRM00019B00000','ALERT',PROPER(lcTag_nam))
              SELECT SYDFILES
              RETURN
            ELSE
              *** Move the field from the browse fields. ***
              lnExist = AT(RTRIM(SUBSTR(laLogPhFld[getbar("puField",lsField),1];
                        ,1,10)),laData[6])
              IF lnExist > 0
                laData[6] = STRTRAN(laData[6],RTRIM(SUBSTR(laLogPhFld[getbar("puField",lsField),1],1,10)),"")
                DO CASE
                  CASE LEFT(laData[6],1) = "|"
                    laData[6] = SUBSTR(laData[6],2)
                  CASE RIGHT(laData[6],1) = "|"
                    laData[6] = SUBSTR(laData[6],1,LEN(laData[6])-1)
                  CASE AT("||",laData[6]) > 0
                    laData[6] = STRTRAN(laData[6],"||","|")
                ENDCASE
                SHOW GET laData[6]
              ENDIF
              
              *** Move the field from the array hold the struc. ***
              IF ALEN(laLogPhFld,1) <> 1
                =ADEL(laLogPhFld,getbar("puField",lsField)) 
                DIMENSION laLogPhFld [ALEN(laLogPhFld,1)-1,1]
              ELSE
                laLogPhFld = ' '  
              ENDIF
              
              lsField = 1
              IF !EMPTY(laLogPhFld[1,1])
                =gfFillpop("puField","laLogPhFld")
              ENDIF
            ENDIF

          CASE lnOption = 2               && Cancel
            RETURN
        ENDCASE
      ENDIF
    ELSE         && If no element was marked in the list
      *** You must mark a list item first. ***
      *** <  Ok  > ***
      = gfModalGen('TRM00008B00000','Dialog'," ")
    ENDIF
ENDCASE

DO lpRdspSays

SHOW GET lsField

SELECT sydFiles

*!***********************************************************
*!
*!      Function lfvData_1
*!
*!***********************************************************
*
FUNCTION lfvData_1

IF llBrowse .OR. (!EMPTY(laData[1]) .AND. LASTKEY() = 13)
  IF ALLTRIM(laData[1]) = "?" .OR. llBrowse
    =gfSeekRec()
  ELSE
    llChar = .T.
    llChar = lfAlpha(ALLTRIM(laData[1]))
    IF !llChar
      *** File name should not have any numbers. ***
      *** <  Ok  > ***
      =gfModalGen("TRM00221B00000","DIALOG")
      laData[1] = SPACE(8)
      _CUROBJ   = OBJNUM(laData[1])
      RETURN
    ELSE
      =gfSeeKRec()
    ENDIF
  ENDIF
  IF laScrMode[4]
    *** Hold this tell we decide something for the default data. ***
    *** Get the strucure for the default data. ***
*    DECLARE laStruc[ALEN(laTAudit,1),1]
*    FOR lnCount = 1 TO ALEN(laStruc,1)
*      laStruc[lnCount,1] = ALLTRIM(SUBSTR(laTAudit[lnCount,1],1,11))
*    ENDFOR
    
    *** Put the audit information fields as default in the strucure. ***
    DECLARE laLogPhFld[ALEN(laTAudit,1),1]
    =ACOPY(laTAudit,laLogPhFld)
    =gfFillpop("puField","laLogPhFld")
    DO lpRdspSays
  ENDIF
ENDIF
llBrowse = .F.

*!**************************************************************************
*!
*!      Function lfvData_7
*!
*!**************************************************************************
*
FUNCTION lfvData_7

laData[7] = SUBSTR('SAU',rbDfnBy,1)

*!***********************************************************
*!
*!      Function lfwData_8
*!
*!***********************************************************
*
FUNCTION lfwData_8

lcOldTag = laData[8]

*!***********************************************************
*!
*!      Function lfvData_8
*!
*!***********************************************************
*
FUNCTION lfvData_8

DO CASE
  CASE _DOS
    laData[8] = gfActPop(8,37,13,51,"laTags",1,1,'laData[8]')
    =lfRefresh()
  CASE _WINDOWS
    laData[8] = laTags(puFld_typ,1)
ENDCASE

llClnStatc = IIF(laData[8] = lcOldTag , .F. , .T.)

*!***********************************************************
*!
*!      Function lfvData_11
*!
*!***********************************************************
*
FUNCTION lfvData_11

IF laData[11]
*  DO SMDFDAT.PRG
*  SELECT SYDFILES
ENDIF

*!**************************************************************************
*!
*!      Function lfvField
*!
*!**************************************************************************
*
FUNCTION lfvField

IF lsField<> 0
  IF laScrMode[2]
    lcStr_Tmp   = ""
    FOR lnCount = 1 TO ALEN(laLogPhFld,1)
      lcStr_Tmp = lcStr_Tmp + IIF(EMPTY(lcStr_Tmp),"","|") + ;
             RTRIM(SUBSTR(laLogPhFld[lnCount,1],1,10))
    ENDFOR
  ELSE
    lcStr_Tmp   = ""
  ENDIF
  SELECT (lcAddNewFd)
  REPLACE cFld_name WITH SUBSTR(laLogPhFld(getbar("puField",lsField),1),1,10) ;
          cFld_head WITH SUBSTR(laLogPhFld(getbar("puField",lsField),1),12) ;
          nScrMode  WITH IIF(laScrMode[2],2,3) ;
          cFile_nam WITH laData[1] ;
          lAdd_New  WITH .F. ;
          mFil_Strc WITH IIF(!EMPTY(lcStr_Tmp) .AND.;
                         laScrMode[2] , "|"+lcStr_Tmp+"|" ,"")

  *** B600263 Line was added by YI-Yasser on 4-6-95
  =gfstatic()
  *E300683,4 Pass parameters to gpDoProg
  *DO gpDoProg WITH 'AWRSMFDDIC WITH "&lcAddNewFd"'
  DO gpDoProg WITH 'AWRSMFDDIC',.F.,'GL',"'"+lcAddNewFd+"'"
  *E300683,4 end
  SELECT sydFiles
ENDIF

*!**************************************************************************
*!
*!      Procedure lpRdspSays
*!
*!**************************************************************************
*
PROCEDURE lpRdspSays

EXTERNAL ARRAY laData

IF !EMPTY(laLogPhFld[1,1])
  lcExact   = SET("EXACT")
  SET EXACT ON
  
  laData[4] = ALEN(laLogPhFld,1)
  SELECT SydField
  SUM nFld_Wdth FOR ASCAN(laLogPhFld,cFld_Name) <> 0 TO laData[2]
  
  SET EXACT &lcExact
  
  SELECT sydFiles
  laData[2] = INT(laData[2]+1)
ELSE
  STORE 0 TO laData[2],laData[4]
ENDIF

SHOW GET laData[2]
SHOW GET laData[4]

*!**************************************************************************
*!
*!      Procedure lpSavScr
*!
*!**************************************************************************
*
PROCEDURE lpSavScr

EXTERNAL ARRAY laData,laScrMode

*** If there is no field assigned to file. ***
IF EMPTY(laLogPhFld[1,1])  
  = gfModalGen('TRM00011B00000','Dialog'," ")
  llCSave = .F.    && Flag to say that no save happen.
  RETURN
ENDIF

*** No Empty file title. ***
IF EMPTY(laData[9])
  *** You cannot save this file with empty title. ***
  *** <   Ok   > ***
  = gfModalGen("TRM00125B00000","DIALOG","file")
  llCSave = .F.    && Flag to say that no save happen.
  _CUROBJ = OBJNUM(laData[9])
  RETURN
ENDIF

SELECT (lcIndxTmp)
LOCATE FOR EMPTY(CINDX_EXP)

IF FOUND()
  *** You cannot save this file with empty index expression. ***
  *** <   Ok   > ***
  = gfModalGen("TRM00126B00000","DIALOG")
  llCSave = .F.    && Flag to say that no save happen.  
  lcNewTag  = &lcIndxTmp..cFile_tag
  cbUniq    = &lcIndxTmp..lUnique
  rbIndxDir = IIF(&lcIndxTmp..lAscend , 1 , 2)
  lcIndx_ex = &lcIndxTmp..cIndx_exp
  lcDesc    = &lcIndxTmp..mIndx_des
  lsTags    = &lcIndxTmp..cFile_tag
  SHOW GETS WINDOW cwdSMFlTag ENABLE ONLY

  IF !WVISIBLE('CWDSMFLTAG')
    =gfActWind('cwdSMFlTag',lctFilTag)  
  ENDIF

  _CUROBJ=OBJNUM(lnexpr)
  KEYBOARD "{ENTER}" 
  RETURN
ENDIF

IF llClnStatc
  DECLARE laWin2Cln[1,2]
  SELECT sydObjct
  SELECT sydObjct.cBasewind,sydObjct.cPrglname;
         WHERE AT(ALLTRIM(laData[1]),sydObjct.mPrgFiles) > 0;
         FROM (gcSysHome+"sydObjct");
         INTO ARRAY laWin2Cln
  IF !EMPTY(laWin2Cln[1,1])
    SELECT SYUSTATC
    FOR lnCount = 1 TO ALEN(laWin2Cln,1)
      IF SEEK("INI"+laWin2Cln[lnCount,1])
        IF gfRlock("SYUSTATC",.T.,10)
          DELETE
        ELSE
          *** There is someone using program ð now. ***
          *** You  have to rebuild statics for this ***
          *** program. ***
          *** <   Ok   > ***
          = gfModalGen("TRM00220B00000","ALERT",ALLTRIM(laWin2Cln[lnCount,2]))
          llCSave = .F.    && Flag to say that no save happen.
          RETURN
        ENDIF  
      ENDIF
    ENDFOR
  ENDIF
ENDIF

SELECT SYDFILES
*** Saving file into the file data base. ***
IF laScrMode[4]
  APPEND BLANK
  *B607972,1 TMI [Start] update only in add mode
  GATHER FROM laData FIELDS &lcScFields MEMO
  *B607972,1 TMI [End  ] 
ENDIF

*B607972,1 TMI [Start] update only the field count
*GATHER FROM laData FIELDS &lcScFields MEMO
REPLACE NFLD_NO WITH laData[4]
*B607972,1 TMI [End  ] 
=gfAdd_Info()

** Append memo. ***
*REPLACE sydFiles.mFldfdata WITH &lcDefData..mDefdata

*** Saving the file fields. ***
SELECT sydFlFld

*B607972,1 TMI [Start] remove the relation
***B040236,1  TMI [Start] set relation to sydfield 
**SET RELATION TO CFLD_NAME INTO SYDFIELD
***B040236,1  TMI [End  ] 
*B607972,1 TMI [End  ] 

IF !EMPTY(laLogPhFld[1,1])
  =gfPopArang("puField",@laLogPhFld)
ENDIF

lnCount1 = 1
SET ORDER TO TAG cFile_Nam
IF SEEK (laData[1])
  SCAN REST WHILE cFile_Nam = laData[1]
    IF lnCount1 <= ALEN(laLogPhFld,1)
      *B607972,1 TMI [Start] locate the pointer to the correct line in sydfield
      =SEEK(SUBSTR(laLogPhFld[lnCount1,1],1,10),'SYDFIELD')
      *B607972,1 TMI [End  ] 
      *B040236,1  TMI [Start] Update the cUpgrdlvl with the correct value from sydfield.cupgrdlvl field
      ** REPLACE cFld_Name WITH SUBSTR(laLogPhFld[lnCount1,1],1,10),;
      **         nFld_Pos  WITH lnCount1,;
      **         cUpGrdLvl WITH laData[7],;
      **         lenumerate WITH RIGHT(ALLT(laLogPhFld[lnCount1,1]),1)='T',;
      **         ndef_seq   WITH VAL(ALLT(LEFT(RIGHT(ALLT(laLogPhFld[lnCount1,1]),11),10)))
      REPLACE cFld_Name WITH SUBSTR(laLogPhFld[lnCount1,1],1,10),;
              nFld_Pos  WITH lnCount1,;
              cUpGrdLvl WITH SYDFIELD.CUPGRDLVL,;
              lenumerate WITH RIGHT(ALLT(laLogPhFld[lnCount1,1]),1)='T',;
              ndef_seq   WITH VAL(ALLT(LEFT(RIGHT(ALLT(laLogPhFld[lnCount1,1]),11),10)))
      *B040236,1  TMI [End  ] 
    ELSE
      SCATTER MEMVAR BLANK
      GATHER  MEMVAR
      DELETE
    ENDIF  
    lnCount1 = lnCount1 + 1
  ENDSCAN  
ENDIF

IF lnCount1 <= ALEN(laLogPhFld,1)
  lcSavDelt = SET("DELETE")
  SET DELETE OFF
  FOR lnCount = lnCount1 TO ALEN(laLogPhFld,1)
    *B607972,1 TMI [Start] locate the pointer to the correct line in sydfield
    =SEEK(SUBSTR(laLogPhFld[lnCount,1],1,10),'SYDFIELD')
    *B607972,1 TMI [End  ] 
    IF SEEK(' ') .AND. DELETED()
      RECALL
      *B040236,1  TMI [Start]
      ** REPLACE cFile_nam WITH laData[1] ;
      **         cFld_name WITH SUBSTR(laLogPhFld[lnCount,1],1,10) ;
      **         nFld_pos  WITH lnCount ;
      **         cUpGrdLvl WITH laData[7],;
      **         lenumerate WITH RIGHT(ALLT(laLogPhFld[lnCount1,1]),1)='T',;
      **         ndef_seq   WITH VAL(ALLT(LEFT(RIGHT(ALLT(laLogPhFld[lnCount1,1]),11),10)))
      REPLACE cFile_nam WITH laData[1] ;
              cFld_name WITH SUBSTR(laLogPhFld[lnCount,1],1,10) ;
              nFld_pos  WITH lnCount ;
              cUpGrdLvl WITH SYDFIELD.CUPGRDLVL,;
              lenumerate WITH RIGHT(ALLT(laLogPhFld[lnCount1,1]),1)='T',;
              ndef_seq   WITH VAL(ALLT(LEFT(RIGHT(ALLT(laLogPhFld[lnCount1,1]),11),10)))
      *B040236,1  TMI [End  ]         
    ELSE
      *B040236,1  TMI [Start] 
      ** INSERT INTO (gcSysHome+"sydFlFld") ;
      **        (cFile_nam,cFld_name,nFld_pos,cUpGrdLvl,lenumerate,ndef_seq);
      **        VALUES(laData[1],SUBSTR(laLogPhFld[lnCount,1],1,10),;
      **               lnCount,laData[7],RIGHT(ALLT(laLogPhFld[lnCount1,1]),1)='T',;
      **               VAL(ALLT(LEFT(RIGHT(ALLT(laLogPhFld[lnCount1,1]),11),10))))
      INSERT INTO (gcSysHome+"sydFlFld") ;
             (cFile_nam,cFld_name,nFld_pos,cUpGrdLvl,lenumerate,ndef_seq);
             VALUES(laData[1],SUBSTR(laLogPhFld[lnCount,1],1,10),;
                    lnCount,SYDFIELD.CUPGRDLVL,RIGHT(ALLT(laLogPhFld[lnCount1,1]),1)='T',;
                    VAL(ALLT(LEFT(RIGHT(ALLT(laLogPhFld[lnCount1,1]),11),10))))
      *B040236,1  TMI [End  ]

    ENDIF
  ENDFOR
  SET DELETE &lcSavDelt
ENDIF

*** Saving the file indexes. ***
SELECT (lcIndxTmp)

REPLACE ALL cIndx_Nam WITH ALLTRIM(laData[1])+".CDX" ,;
            cUpGrdLvl WITH laData[7]

=gfTmp2Mast("SYDINDEX",lcIndxTmp,'Saving indexes of file ' +;
                                 ALLTRIM(laData[1])+' ...')  

IF llFixTag
  SELECT SYDOBJCT
  FOR lnCount2 = 1 TO ALEN(laFixTag,1)
    IF !EMPTY(laFixTag[lnCount2,3])
      LOCATE FOR ALLTRIM(laData[1]) $ sydObjct.mPrgfiles .AND. ;
                 ALLTRIM(laFixTag[lnCount2,1]) $ sydObjct.mPrgfiles
      DO WHILE FOUND()
        REPLACE sydObjct.mPrgfiles WITH ;
                STRTRAN(sydObjct.mPrgfiles,;
                ALLTRIM(laFixTag[lnCount2,1]),;
                IIF(!EMPTY(laFixTag[lnCount2,2]),;
                ALLTRIM(laFixTag[lnCount2,2]),SPACE(10)))
        CONTINUE
      ENDDO
    ENDIF
  ENDFOR
  llFixTag = .F.
ENDIF

SELECT sydFiles

*!**************************************************************************
*!
*!      Procedure lpDelScr
*!
*!**************************************************************************
*
PROCEDURE lpDelScr

EXTERNAL ARRAY laData,laScrMode

*** See if there is any applications using this file. ***
IF !EMPTY(laAppl[1])
  *** You cannot delete this file. It's have been used in ***
  *** one or more than module.
  =gfModalGen("TRM00121B00000","ALERT")
  RETURN
ENDIF
  
*** See if there is any program using this file. ***
DECLARE laPrograms[1]
laPrograms = " "
SELECT cPrglname FROM (gcSysHome+"SYDOBJCT") ;
       WHERE ALLTRIM(laData[1]) $ sydobjct.mPrgfiles ;
       INTO ARRAY laPrograms

IF !EMPTY(laPrograms[1])
  *** You cannot delete this file. It's have been used by ***
  *** one or more the program.
  =gfModalGen("TRM00122B00000","ALERT")
  RETURN
ENDIF

*** Delete the indexes. ***
SELECT (lcIndxTmp)
REPLACE ALL cStatus WITH 'D'
DELETE ALL
=gfTmp2Mast("SYDINDEX",lcIndxTmp,'Deleting indexes of file '+laData[1]+' ...')

*** Deleteing the file fields data
IF !EMPTY(laLogPhFld[1,1])
  SELECT sydFlFld
  IF SEEK(laData[1])
    SCAN FOR cFile_Nam = laData[1]
      DELETE
    ENDSCAN 
  ENDIF
ENDIF

SELECT sydFiles
SCATTER MEMVAR MEMO BLANK
GATHER MEMVAR MEMO  
DELETE

laScrMode   =.F.
laScrMode[1]=.T.

*!***********************************************************
*!
*!      Function lfvBrowFld
*!
*!***********************************************************
*
FUNCTION lfvBrowFld

lcOldBrow = laData[6]

DECLARE laBrowFld[1]

IF !EMPTY(laData[6])
  =gfSubStr(laData[6],@laBrowFld,"|")
  SELECT sydField
  FOR lnCount = 1 TO ALEN(laBrowFld,1)
    IF SEEK(laBrowFld[lnCount])
      *B604857,4 (Begin) Make both arrays (laBrowFld,laLogPhFld) exactly the same for the mover to work
      *B604857,4         well and Move button work.
      *laBrowFld[lnCount] = ALLTRIM(sydField.cFld_name) + ;
                           SPACE(11-LEN(RTRIM(sydField.cFld_name))) +;
                           IIF(EMPTY(sydField.cfld_head),;
                           sydField.cFld_Name,sydField.cfld_head)
      laBrowFld[lnCount] = ALLTRIM(sydField.cFld_name) + ;
                           SPACE(11-LEN(RTRIM(sydField.cFld_name))) +;
                           IIF(EMPTY(sydField.cfld_head),;
                           sydField.cFld_Name,sydField.cfld_head)+ALLT('~'+PADR(sydflfld.ndef_seq,10)+;
                           IIF(sydflfld.lenumerate,'T','F'))
                    
      *B604857,4 (End)
    ENDIF
  ENDFOR
  SELECT sydFiles
ENDIF  
IF !EMPTY(laLogPhFld[1,1])
  =gfPopArang("puField",@laLogPhFld)
ENDIF  

=gfMover(@laLogPhFld,@laBrowFld,"Select browse fields")

IF !EMPTY(laBrowFld[1])
  lcFieldstr   = ''
  laData[6]    = ''
  FOR lnCount  = 1 TO ALEN(laBrowFld,1)
    lcFieldstr = lcFieldstr + IIF(EMPTY(lcFieldstr),'','|')+;
                 ALLTRIM(SUBSTR(laBrowFld[lnCount],1,10))
  ENDFOR
  
  laData[6] = lcFieldstr
ELSE
  laData[6] = ' '
ENDIF    

SHOW GET laData[6]

=gfUpdate()

llClnStatc = .T.

*!**************************************************************************
*!
*!      Function lfvNewTag
*!
*!**************************************************************************
*
FUNCTION lfvNewTag

IF !EMPTY(lcNewTag) .AND. LASTKEY() = 13
  SELECT (lcIndxTmp)
  lnTmpRec = RECNO()
  LOCATE FOR cFile_tag = lcNewTag

  IF FOUND()
    *** Tag is allredy exist, File cannot have ***
    *** two tages with the same name.
    *** <  Ok  > ***
    =gfModalGen("TRM00127B00000","DIALOG")
    lcNewTag  = &lcIndxTmp..cFile_tag
    cbUniq    = &lcIndxTmp..lUnique
    rbIndxDir = IIF(&lcIndxTmp..lAscend , 1 , 2)
    lcIndx_ex = &lcIndxTmp..cIndx_exp
    lcDesc    = &lcIndxTmp..mIndx_des
    lsTags    = &lcIndxTmp..cFile_tag
    SHOW GETS WINDOW cwdSMFlTag ENABLE ONLY
  ELSE   
    IF lnTmpRec <= RECCOUNT()
      GO lnTmpRec
    ENDIF  
  
    IF llNewTag
      llNewTag = .F.

      DIMENSION laTags[ALEN(laTags,1)+1,1]
      laTags[ALEN(laTags,1),1] = lcNewTag
      
      *B600655,1 Reham On 08/23/95   *** Begin ***
      *B600655,1 Add the changing of tags names in the fixing array
      *B600655,1 to prevent the error "Subscript out of bounds"
      IF !EMPTY(laFixTag[1,1])
        DIMENSION laFixTag[ALEN(laFixTag,1)+1,3]
      ENDIF
      laFixTag[ALEN(laFixTag,1),1] = lcNewTag
      laFixTag[ALEN(laFixTag,1),2] = lcNewTag
      *B600655,1 Reham On 08/23/95   *** End   ***
      
      INSERT INTO (gcWorkDir+lcIndxTmp) ;
                  (cIndx_nam,cFile_nam,cFile_tag,lAscend,lUnique,cStatus);
           VALUES (ALLTRIM(laData[1])+".CDX",laData[1],lcNewTag,.T.,.F.,"A")
      laData[5]  = laData[5] + 1
      lsTags     = laData[5]
      SHOW GET laData[5]
    ELSE
      IF cFile_tag = laData[8]
        laData[8]  = lcNewTag
        =lfRefresh()
      ENDIF
      lcSavExact = SET("EXACT")
      SET EXACT ON
      IF ASCAN(laTags,cFile_tag) > 0
        laTags[ASCAN(laTags,cFile_tag),1] = lcNewTag
      ENDIF
      
      IF ASUBSCRIPT(laFixTag,ASCAN(laFixTag,cFile_tag),1) > 0
        llFixTag = .T.
        laFixTag[ASUBSCRIPT(laFixTag,ASCAN(laFixTag,cFile_tag),1),2] = lcNewTag
        laFixTag[ASUBSCRIPT(laFixTag,ASCAN(laFixTag,cFile_tag),1),3] = "F"
      ENDIF
      
      SET EXACT &lcSavExact
      lcStatus = SUBSTR("MAM" , AT(cStatus,"MAS") , 1)
      REPLACE cFile_tag  WITH lcNewTag ,;
              cStatus    WITH lcStatus
    ENDIF
    SHOW GETS WINDOW cwdSMFlTag ENABLE ONLY
  ENDIF
ENDIF  
SELECT sydFiles

*!**************************************************************************
*!
*!      Function lfwTags
*!
*!**************************************************************************
*
FUNCTION lfwTags

IF llNewTag
  _CUROBJ = OBJNUM(lcNewTag) 
  RETURN .F.
ENDIF

IF  laData[5] = 0
  _CUROBJ = OBJNUM(pbNewTags) 
  RETURN .F.
ENDIF  

SELECT (lcIndxTmp)
lcNewTag  = &lcIndxTmp..cFile_tag
cbUniq    = &lcIndxTmp..lUnique
rbIndxDir = IIF(&lcIndxTmp..lAscend , 1 , 2)
lcIndx_ex = &lcIndxTmp..cIndx_exp
lcDesc    = &lcIndxTmp..mIndx_des

SHOW GET lcNewTag
SHOW GET cbUniq
SHOW GET rbIndxDir
SHOW GET lcIndx_ex
SHOW GET lcDesc

SELECT sydFiles

*!**************************************************************************
*!
*!      Function lfvIndxDir
*!
*!**************************************************************************
*
FUNCTION lfvIndxDir

SELECT (lcIndxTmp)

lcStatus = SUBSTR("MAM" , AT(cStatus,"MAS") , 1)

REPLACE &lcIndxTmp..lAscend WITH IIF(rbIndxDir = 1 , .T. , .F.) ;
        &lcIndxTmp..cStatus WITH lcStatus

SELECT sydFiles

*!**************************************************************************
*!
*!      Function lfvUniq
*!
*!**************************************************************************
*
FUNCTION lfvUniq

SELECT (lcIndxTmp)

lcStatus = SUBSTR("MAM" , AT(cStatus,"MAS") , 1)

REPLACE &lcIndxTmp..lUnique WITH cbUniq ;
        &lcIndxTmp..cStatus WITH lcStatus

SELECT sydFiles

*!**************************************************************************
*!
*!      Function lfvDesc
*!
*!**************************************************************************
*
FUNCTION lfvDesc

SELECT (lcIndxTmp)

lcStatus = SUBSTR("MAM" , AT(cStatus,"MAS") , 1)

REPLACE &lcIndxTmp..mIndx_des WITH lcDesc ;
        &lcIndxTmp..cStatus   WITH lcStatus

SELECT sydFiles

*!**************************************************************************
*!
*!      Function lfvExpr
*!
*!**************************************************************************
*
FUNCTION lfvExpr

*** Get the phisical & the logical names of the fields. ***
IF !EMPTY(laLogPhFld[1,1])
  =gfPopArang("puField",@laLogPhFld)
ENDIF  
DECLARE laPhFld [ALEN(laLogPhFld,1),1]
DECLARE laLogFld[ALEN(laLogPhFld,1),1]
FOR lnCount = 1 TO ALEN(laLogPhFld,1)
  laPhFld [lnCount,1] = ALLTRIM(SUBSTR(laLogPhFld[lnCount,1],1,11))
  laLogFld[lnCount,1] = ALLTRIM(SUBSTR(laLogPhFld[lnCount,1],13))
ENDFOR

SELECT (lcIndxTmp)

lcStatus  = SUBSTR("MAM" , AT(cStatus,"MAS") , 1)

lcOldVal  = lcIndx_ex

lcIndx_ex = GetExpr (lcIndx_ex,laData[1],"Index Expression","laLogFld",@laPhFld)

lcIndx_ex = IIF(EMPTY(lcIndx_ex),lcOldVal,lcIndx_ex)

REPLACE &lcIndxTmp..cIndx_exp WITH lcIndx_ex ;
        &lcIndxTmp..cStatus   WITH lcStatus

SHOW GET lcIndx_ex

SELECT sydFiles

*!***********************************************************
*!
*!      Function lfvAddTags
*!
*!***********************************************************
*
FUNCTION lfvAddTags

SELECT (lcIndxTmp)
llNewTag  = .T.
lcNewTag  = SPACE(10)
*lcNewTag  = " "
cbUniq    = .F.
rbIndxDir = 1
lcIndx_ex = " "
lcDesc    = " "
_CUROBJ = OBJNUM(lcNewTag)

SHOW GET lcNewTag   ENABLE
SHOW GET lcDesc     DISABLE
SHOW GET pbZoomTag  DISABLE
SHOW GET puFld_typ  DISABLE
SHOW GET pbRemTags  DISABLE
SHOW GET lnexpr     DISABLE
SHOW GET rbIndxDir  DISABLE
SHOW GET cbuniq     DISABLE

SELECT sydFiles

*!***********************************************************
*!
*!      Function lfvRemTags
*!
*!***********************************************************
*
FUNCTION lfvRemTags

IF lsTags <> 0
  IF !EMPTY(laData[8]) .AND. ;
     ALLTRIM(UPPER(&lcIndxTmp..cFile_tag)) = ALLTRIM(UPPER(laData[8]))
    *** You cannot delete the master tag. ***
    *** <  Ok  > ***
    =gfModalGen("TRM00119B00000","ALERT")
    RETURN
  ELSE
    lnOption = gfModalGen('QRM00007B00007','ALERT'," ")  && Confirm
    DO CASE
      CASE lnOption = 1             && Remove

        SELECT (lcIndxTmp)
        
        lcSavExact = SET("EXACT")
        SET EXACT ON
        IF ASCAN(laTags,cFile_tag) > 0
          =ADEL(laTags,ASCAN(laTags,cFile_tag))
          DIMENSION laTags[ALEN(laTags,1)-1,1]
        ENDIF
        
        IF ASUBSCRIPT(laFixTag,ASCAN(laFixTag,cFile_tag),1) > 0
          llFixTag = .T.
          laFixTag[ASUBSCRIPT(laFixTag,ASCAN(laFixTag,cFile_tag),1),2] = " "
          laFixTag[ASUBSCRIPT(laFixTag,ASCAN(laFixTag,cFile_tag),1),3] = "F"
        ENDIF
        
        SET EXACT &lcSavExact
        
        REPLACE cStatus WITH SUBSTR("DSD",AT(cStatus,"MAS"),1)
        DELETE
        laData[5] = laData[5] - 1

        SKIP
        IF EOF()
          GO TOP
        ENDIF

        lsTags    = RECNO()
        lcNewTag  = &lcIndxTmp..cFile_tag
        cbUniq    = &lcIndxTmp..lUnique
        rbIndxDir = IIF(&lcIndxTmp..lAscend , 1 , 2)
        lcIndx_ex = &lcIndxTmp..cIndx_exp
        lcDesc    = &lcIndxTmp..mIndx_des

        SHOW GET laData[5]
        SHOW GET lsTags
        SHOW GET lcNewTag
        SHOW GET cbUniq
        SHOW GET rbIndxDir
        SHOW GET lcIndx_ex
        SHOW GET lcDesc

        SELECT sydFiles

      CASE lnOption = 2               && Cancel
        RETURN
    ENDCASE
  ENDIF
ELSE         && If no element was marked in the list
  = gfModalGen('TRM00008B00000','Dialog'," ")
ENDIF


FUNCTION lfWFlFld
IF lsField>0
  lnSeqStart = LEFT(RIGHT(ALLT(laLogPhFld[GETBAR('puField',lsField),1]),11),10)
  puseq = IIF(RIGHT(ALLT(laLogPhFld[GETBAR('puField',lsField),1]),1) = 'T',1,2)
ELSE
  lnSeqStart = ''
  puseq  = 2
ENDIF  
SHOW GET lnSeqStart
SHOW GET puSeq


FUNCTION lfvSeqNum
IF lsField>0
  =gfPopArang("puField",@laLogPhFld)
*  lnCpos = RAT('~',laLogPhFld[GETBAR('puField',lsField),1])
*  laLogPhFld[GETBAR('puField',lsField),1] = SUBSTR(laLogPhFld[GETBAR('puField',lsField),1],1,lnCpos)+;
*  PADR(ALLT(lnSeqStart),10)+SUBSTR('TF',puseq,1)
  lnCpos = RAT('~',laLogPhFld[lsField,1])
  laLogPhFld[lsField,1] = SUBSTR(laLogPhFld[lsField,1],1,lnCpos)+;
  PADR(ALLT(lnSeqStart),10)+SUBSTR('TF',puseq,1)
  =gfFillpop("puField","laLogPhFld")
ENDIF

FUNCTION lfvSeqStat
IF puSeq = 1
   DIME laConfield[1]
   STORE '' TO laConfield
   SELECT SYDFLFLD.CFLD_NAME ;
   FROM (gcSysHome+'SYDFLFLD');
   WHERE CFILE_NAM <> laData[1];
   AND CFLD_NAME = SUBSTR(laLogPhFld[GETBAR('puField',lsField),1],1,10);
   AND lenumerate INTO ARRAY laConfield
   IF !EMPTY(laConfield[1])
     =gfModalGen("TRM00256B00000","ALERT")
     puSeq = 2
     SHOW GET PUSEQ
   ENDIF
ENDIF
IF lsField>0
  =gfPopArang("puField",@laLogPhFld)
*  lnCpos = RAT('~',laLogPhFld[GETBAR('puField',lsField),1])
*  laLogPhFld[GETBAR('puField',lsField),1] = SUBSTR(laLogPhFld[GETBAR('puField',lsField),1],1,lnCpos)+;
*  PADR(ALLT(lnSeqStart),10)+SUBSTR('TF',puseq,1)
  lnCpos = RAT('~',laLogPhFld[lsField,1])
  laLogPhFld[lsField,1] = SUBSTR(laLogPhFld[lsField,1],1,lnCpos)+;
  PADR(ALLT(lnSeqStart),10)+SUBSTR('TF',puseq,1)
  =gfFillpop("puField","laLogPhFld")
ENDIF
