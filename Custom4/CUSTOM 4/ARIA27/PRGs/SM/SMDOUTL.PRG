*!********************************************************************
*: Program file  : SMDOUTL.PRG 
*: Program desc. : Schedule File Utilities.
*: Program desc. : REORGANIZING, REINDEXING AND REBALANCE DATABASES.
*:         System: ARIA APPAREL SERIES
*:         Module: SM
*:      Developer: Adel Mohammed El Gazzar (ADEL)
*: Refer to      : (E301516)
*:     Date      : 23/01/2001
*!********************************************************************
PARAMETER lcCompId,lcReindex,lcReOrganz,lcReBalanc,lcLogTxt

SELECT SYUSTATC
LOCATE FOR cObj_name ='OLDVARS' .AND. syuStatc.cStation <> gcStation
*** If at least one user have a record in the static files
*** keep locating for all users
DO WHILE FOUND()
  lcUser    = syuStatc.cUser_ID        
  lcStion   = syuStatc.cStation
  IF lcUser = gcUser_Id .AND. lcStion = gcStation
    CONTINUE
  ELSE
    lcOldRep = SET('REPROCESS')
    SET REPROCESS TO 1
    *B604531,4 AME [Start] Check static file just in case of reindex or/and reorganize
    IF !EMPTY(lcReindex) .OR. !EMPTY(lcReOrganz)
    *B604531,4 AME [End]
      IF RLOCK('SYUSTATC')
        UNLOCK IN ALIAS('SYUSTATC')
        SET REPROCESS TO lcOldRep
        CONTINUE
      ELSE
        SET REPROCESS TO lcOldRep
        =gfModalGen ('INM00025B00000','Alert')  
        glQuitting = .T.
        RETURN .T.
      ENDIF
    *B604531,4 AME [Start]  
    ELSE
      SET REPROCESS TO lcOldRep
      CONTINUE
    ENDIF
    *B604531,4 AME [End]
  ENDIF
ENDDO
*-- If all the parameters were passed empty then terminate the processing
IF EMPTY(lcReindex) AND EMPTY(lcReOrganz) AND EMPTY(lcReBalanc) 
  RETURN(.F.)
ENDIF
DECLARE laApp_fl[1,1],laSelected [1,2],laComMdls[1,1]
STORE '' TO  laApp_fl,laSelected,laComMdls,lcComMdls
=gfOpenFile(gcSysHome+'SYDFILES')
=gfOpenFile(gcSysHome+'SYCCOMP','CCOMP_ID')
=SEEK(lcCompId)
lcFilePath = ALLTRIM(ccom_ddir)
lcComMdls  = ALLT(mcomp_mdl)
*--Fill laComMdls with company modules.
=gfSubStr(lcComMdls,@laComMdls,"|")
IF !EMPTY(lcLogTxt)
  IF TYPE('llOvWrt') <> 'U' AND llOvWrt
    lcFilHandl = FCREATE(lcLogTxt,0)
    llOvWrt = .F.
  ENDIF
  IF !FILE(lcLogTxt)
    lcFilHandl = FCREATE(lcLogTxt,0)
  ELSE  
    lcFilHandl = FOPEN(lcLogTxt,2)
    =FSEEK(lcFilHandl,0,2)
  ENDIF
ENDIF
llOpenRep = .T.
STORE .T. TO llGoOn
IF !EMPTY(lcReindex) OR !EMPTY(lcReOrganz)
  *--Will we select Standard or Custom files?
  lcCond = IIF(lcReindex='B' OR lcReOrganz='B';
                OR (!EMPTY(lcReindex) AND !EMPTY(lcReOrganz);
                AND lcReindex<>lcReOrganz),'.T.',;
                IIF(!EMPTY(lcReindex),IIF(lcReindex='S','cupgrdlvl <> "U"','cupgrdlvl = "U"'),;
                IIF(!EMPTY(lcReOrganz),IIF(lcReOrganz='S','cupgrdlvl <> "U"','cupgrdlvl = "U"'),.T.)))
  SELECT " "+cFile_nam+" - "+cfile_ttl,lsys_data,cupgrdlvl  ;
      FROM (gcSyshome+"sydfiles") ;
      INTO ARRAY laApp_fl ;
      WHERE LEFT(cFile_nam,2) <> "SY" AND &lcCond .AND. lfCompMdl(ALLTRIM(mfile_app));
      ORDER BY 1
  IF _TALLY = 0
    lcMess = IIF(!EMPTY(lcReindex),'reindex','')+IIF(!EMPTY(lcReOrganz),;
             IIF(!EMPTY(lcReindex),' or recognize','recognize'),'')
    =FPUTS(lcFilHandl,'ReIndexing:')
    =FPUTS(lcFilHandl,'START PROCESSING     '+DTOC(DATE())+SPACE(5)+TIME())
    =FPUTS(lcFilHandl,'No files to reindex for company : '+lcCompId)
    =FPUTS(lcFilHandl,'END PROCESSING       '+DTOC(DATE())+SPACE(5)+TIME())
    =FPUTS(lcFilHandl,'ReOrganizing:')
    =FPUTS(lcFilHandl,'START PROCESSING     '+DTOC(DATE())+SPACE(5)+TIME())
    =FPUTS(lcFilHandl,'No files to Reorganize for company : '+lcCompId)
    =FPUTS(lcFilHandl,'END PROCESSING       '+DTOC(DATE())+SPACE(5)+TIME())
    llGoOn = .F.
  ENDIF    
ENDIF
*--Reindex OR Recognize
IF !EMPTY(lcReindex) AND llGoOn
  =FPUTS(lcFilHandl,'ReIndexing:')
  =FPUTS(lcFilHandl,'START PROCESSING     '+DTOC(DATE())+SPACE(5)+TIME())

  =lfDoAction('INDEX')

  =FPUTS(lcFilHandl,'END PROCESSING       '+DTOC(DATE())+SPACE(5)+TIME())
ENDIF
IF !EMPTY(lcReOrganz) AND llGoOn
  =FPUTS(lcFilHandl,'ReOrganizing:')
  =FPUTS(lcFilHandl,'START PROCESSING     '+DTOC(DATE())+SPACE(5)+TIME())

  =lfDoAction('REORGANIZ')
  =FPUTS(lcFilHandl,'END PROCESSING       '+DTOC(DATE())+SPACE(5)+TIME())  
ENDIF
IF !EMPTY(lcReBalanc)
  *--Fill the company array
  DECLARE laRpCmpCod[1,3]
  STORE '' TO lcRpCmpExp
  *-- Collect all companies
  SELECT ccomp_id+" - "+cCom_Name,cCom_dDir,mModlSet ;
    FROM SYCCOMP                            ;
    WHERE ccomp_id = lcCompId               ;
    INTO ARRAY laRpCmpCod                   ;
    ORDER BY 1
    DIMENSION laFxFlt[10,6]
  STORE ' ' TO laFxFlt
  DO (gcAppHome+"SM\SMREBAL.FXP") WITH '',lcCompId,.F.,.F.,.T.
ENDIF
=FCLOSE(lcFilHandl)
*!*******************************************************************
*!
*!      Function: lfDoAction
*!
*!*******************************************************************
*
FUNCTION lfDoAction
PARAMETERS lcAction

DECLARE lax[1]
lax=" "
laApp_fl = ""
*--Get the needed files for easch process
IF lcAction = "INDEX" 
  IF !EMPTY(lcReindex)
    *--Will we select Standard or Custom files?
    lcCond = IIF(lcReindex='B','.T.',;
             IIF(lcReindex='S','cupgrdlvl <> "U"','cupgrdlvl = "U"'))
    SELECT " "+cFile_nam+" - "+cfile_ttl,lsys_data,cupgrdlvl  ;
        FROM (gcSyshome+"sydfiles") ;
        INTO ARRAY laApp_fl ;
        WHERE LEFT(cFile_nam,2) <> "SY" AND &lcCond .AND. lfCompMdl(ALLTRIM(mfile_app));
        ORDER BY 1
  ENDIF
ELSE
  IF !EMPTY(lcReOrganz)
    *--Will we select Standard or Custom files?
    lcCond = IIF(lcReOrganz='B','.T.',;
             IIF(lcReOrganz='S','cupgrdlvl <> "U"','cupgrdlvl = "U"'))
    SELECT " "+cFile_nam+" - "+cfile_ttl,lsys_data,cupgrdlvl  ;
        FROM (gcSyshome+"sydfiles") ;
        INTO ARRAY laApp_fl ;
        WHERE LEFT(cFile_nam,2) <> "SY" AND &lcCond .AND. lfCompMdl(ALLTRIM(mfile_app));
        ORDER BY 1
  ENDIF
ENDIF
=lfvAselect()                 && Collect Selected files
IF !EMPTY(laSelected [1,1])   && Exit if None 
  DECLARE laFileStrc[1,1],laFileAStr[1,1]
*  llOpenRep  = .F.               && Open Report flage
  lcFilHandl = ''
  lcTempNam  = ''
  lcFixMess  = ''
  lnRepLine  = 0
  llYes2All  = .F.
  llUpdated = .f.
  DO CASE
    CASE lcAction = "INDEX"
      lcTherMesg  = "Building index for file..."
    CASE lcAction = "REORGANIZ"
      lcTherMesg  = "Reorganizing file ..."
  ENDCASE    
  lnSelected = ALEN(laSelected,1)
  *** Loop all selected files
  FOR lnCount = 1 TO lnSelected
    lcFileNam = UPPER(ALLTRIM(laSelected[lnCount,1]))
    lcTargDir = UPPER(IIF(UPPER(LEFT(lcFileNam ,2)) = 'SY', ALLTRIM(gcSysHome) , ALLTRIM(lcFilePath)))
    lcSubsVar = ALLTRIM(laSelected[lnCount,2])
    =gfThermo(lnSelected ,lnCount-.01,lcTherMesg,lcSubsVar)
    DO CASE
      CASE lcAction = "INDEX"
        =lfIndex(lcFileNam)
      CASE lcAction = "REORGANIZ"
        =lfReOrg(lcFileNam)
    ENDCASE
    =gfThermo(lnSelected ,lnCount,lcTherMesg,lcSubsVar)
  ENDFOR
  =FPUTS(lcFilHandl,'END PROCESSING       '+DTOC(DATE())+SPACE(5)+TIME())
  *** If there is a report created dispaly it to the user
ELSE
  IF lcAction = "INDEX"
    *     =gfModalGen ('INM00381B00000','Dialog','reindex for company : '+lcCompId)
    =FPUTS(lcFilHandl,'ReIndexing:')
    =FPUTS(lcFilHandl,'START PROCESSING     '+DTOC(DATE())+SPACE(5)+TIME())
    =FPUTS(lcFilHandl,'No files to reindex for company : '+lcCompId)
    =FPUTS(lcFilHandl,'END PROCESSING       '+DTOC(DATE())+SPACE(5)+TIME())
  ELSE
    * =gfModalGen ('INM00381B00000','Dialog','recognize for company : '+lcCompId)
    =FPUTS(lcFilHandl,'ReOrganizing:')
    =FPUTS(lcFilHandl,'START PROCESSING     '+DTOC(DATE())+SPACE(5)+TIME())
    =FPUTS(lcFilHandl,'No files to Reorganize for company : '+lcCompId)
    =FPUTS(lcFilHandl,'END PROCESSING       '+DTOC(DATE())+SPACE(5)+TIME())
  ENDIF
ENDIF

*!*******************************************************************
*!
*!      Function: lfvAselect
*!
*!*******************************************************************
*
* Fill array with marked options from multyselect popup
*
FUNCTION lfvAselect
DECLARE laSelected [1,3]
laSelected = '' 
lnAryLen   = 0
IF !EMPTY(laApp_fl)
  FOR lnCount = 1 TO ALEN(laApp_fl,1)
      lnAryLen = lnAryLen + 1 
      DECLARE laSelected [lnAryLen,3]
      laSelected [lnAryLen,1] = SUBSTR(laApp_fl[lnCount,1],2,9)
      laSelected [lnAryLen,2] = SUBSTR(laApp_fl[lnCount,1],13 )
      laSelected [lnAryLen,3] = laApp_fl[lnCount,3]
  ENDFOR
ENDIF  

*!*******************************************************************
*!
*!      Function: lfIndex
*!
*!*******************************************************************
* This function will creat or fix the index for one file
*
FUNCTION lfIndex
PARAMETERS lcFileNam
DECLARE laFileCDX[1,1]

lcSavAlias = SELECT(0)
SELECT ALLTRIM(sydindex.cindx_exp),ALLTRIM(sydindex.cfile_tag),sydindex.lascend,;
       sydindex.lunique;
       FROM (gcSyshome+"sydindex");
       WHERE UPPER(sydindex.cfile_nam) = PADR(lcFileNam,8);
       INTO ARRAY laFileCDX
IF !USED(lcFileNam)
  SELECT 0
ELSE
  SELECT (lcFileNam)
ENDIF  
IF USED(lcFileNam)
  USE IN (lcFileNam)
ENDIF
IF FILE(ALLTRIM(lcTargDir)+ALLTRIM(lcFileNam)+'.CDX')
  IF FILE(ALLTRIM(lcTargDir)+ALLTRIM(lcFileNam)+'.CDX') 
     ERASE (ALLTRIM(lcTargDir)+ALLTRIM(lcFileNam)+'.CDB')
  ENDIF
  RENAME ALLTRIM(lcTargDir)+ALLTRIM(lcFileNam)+'.CDX' TO ALLTRIM(lcTargDir)+ALLTRIM(lcFileNam)+'.CDB'
ENDIF
PRIVATE lcOnError,llError
lcOnError = ON('ERROR')
llError = .F.
ON ERROR llError = .T.
IF FILE(ALLTRIM(lcTargDir)+ALLTRIM(lcFileNam)+'.DBF')
  USE (ALLTRIM(lcTargDir)+ALLTRIM(lcFileNam)) EXCL
  IF llError
    USE (ALLTRIM(lcTargDir)+ALLTRIM(lcFileNam)) EXCL
  ENDIF
  IF !EMPTY(laFileCDX[1])
      FOR lnTagNo = 1 TO ALEN(laFileCDX,1)
        lcAscend  =IIF(laFileCDX[lnTagNo,3],'ASCENDING','DESCENDING')
        lcUnique  =IIF(laFileCDX[lnTagNo,4],'UNIQUE','')
        INDEX ON &laFileCDX[lnTagNo,1] TAG &laFileCDX[lnTagNo,2]; 
              ADDITIVE &lcAscend &lcUnique
      ENDFOR            
  ENDIF
  USE
ELSE
  =lfReport(2)
ENDIF  
ON ERROR &lcOnError
IF FILE(ALLTRIM(lcTargDir)+ALLTRIM(lcFileNam)+'.CDX') 
   ERASE (ALLTRIM(lcTargDir)+ALLTRIM(lcFileNam)+'.CDB')
ELSE
  IF FILE(ALLTRIM(lcTargDir)+ALLTRIM(lcFileNam)+'.CDB')
    RENAME (ALLTRIM(lcTargDir)+ALLTRIM(lcFileNam)+'.CDB') TO (ALLTRIM(lcTargDir)+ALLTRIM(lcFileNam)+'.CDX)
  ENDIF
ENDIF   
SELECT (lcSavAlias)

*!*******************************************************************
*!
*!      Function: lfReOrg
*!
*!*******************************************************************
*!
FUNCTION lfReOrg

PARAMETERS lcFileNam

IF !USED('SYDFILES')
  SELECT 0
  USE (gcSysHome+'SYDFILES') EXCL
ELSE
  SELECT SYDFILES 
ENDIF
SET ORDER TO TAG CFILE_NAM
lcMastTag = LOOKUP(cfile_tag,lcFileNam,CFILE_NAM,'CFILE_NAM')
IF !USED(lcFileNam)
  SELECT 0
ELSE
  SELECT (lcFileNam)
ENDIF  
*-- use gfopenfile to be able to see our errorhandler so if any
*-- error in file openning error handler will fix it
USE (lcTargDir+lcFileNam) EXCL
IF !EMPTY(lcMastTag)
  SET ORDER TO TAG &lcMastTag
ENDIF
IF TYPE('lcExt') # "C"
  lcTempName = gfTempName()
  COPY TO (lcTargDir+lcTempName+".DBF") CDX FOR .T.
  USE 
  ERASE (lcTargDir+lcFileNam+".DBF") 
  ERASE (lcTargDir+lcFileNam+".CDX")
  ERASE (lcTargDir+lcFileNam+".FPT")
  IF FILE (lcTargDir+lcTempName+".DBF")
    RENAME (lcTargDir+lcTempName+".DBF") TO (lcTargDir+lcFileNam+".DBF")
  ENDIF
  IF FILE (lcTargDir+lcTempName+".CDX")
    RENAME (lcTargDir+lcTempName+".CDX") TO (lcTargDir+lcFileNam+".CDX")
  ENDIF
  IF FILE (lcTargDir+lcTempName+".FPT")
    RENAME (lcTargDir+lcTempName+".FPT") TO (lcTargDir+lcFileNam+".FPT")
  ENDIF
ENDIF

*!*******************************************************************
*!
*!      Function: lfReport
*!
*!*******************************************************************
*
FUNCTION lfReport
PARAMETERS lnAction

IF lcTempNam <> lcFileNam
  lcTempNam =  lcFileNam
  IF lnRepLine > 0
    =FPUTS(lcFilHandl,REPLICATE('Ä',68))
  ENDIF
  lnRepLine   = lnRepLine + 1
  =FPUTS(lcFilHandl,STR(lnRepLine,2)+". File &lcTargDir&lcFileNam :")
ENDIF
=FPUTS(lcFilHandl,"   * File's information is not stored in the dictionary.")

*!*******************************************************************
*!
*!      Function: lfCompMdl
*!
*!*******************************************************************
* This function is beeing called from the select SQL to determin if 
* one file from the file dectionary belong to a module installed to 
* a spacific company to be colleted or not
FUNCTION lfCompMdl
PARAMETERS lcFileMdl

llRetFlag = .F.
FOR lnCount = 1 TO ALEN(laComMdls,1)
  IF (laComMdls[lnCount] $ lcFileMdl) OR ('SY' $ lcFileMdl)
    llRetFlag = .T.
    EXIT
  ENDIF
ENDFOR
RETURN llRetFlag
