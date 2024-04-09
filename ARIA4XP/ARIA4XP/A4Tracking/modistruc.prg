*!*************************************************************
*! Name      : lpUpdateTableStructure
*: Developer : Ahmed Khalifa (AKM)
*! Date      : 08/04/2005
*! Purpose   : Upadtwe Table structure
*!           :
*:***************************************************************************
*: Calls :
*:     Procedures : lpCreateConnStr
*:     Functions  :
*:***************************************************************************
*: Passed Parameters  : lcA27SysFiles
*:                      lbIndexOnly
*:                      SYDFILES
*:                      SYDFLFLD
*:                      SYDFIELD
*:                      SYDINDEX
*:                      UpdTables
*:***************************************************************************
*: Example :Do lpUpdateTableStructure with lcA27SysFiles, lcSqlDictionary, lcTable, lcFile_Ttl , lbIndexOnly
*:***************************************************************************
*: Note that : If lcAccount is ommited it means that calling from menu.
*:           : else if it's character it means that calling from another program.
*:***************************************************************************
*: Modifications :
*:***************************************************************************
*! E303030,1 MAB 12/28/2011 Extend File/Field/Index to NN length instead of 8,10,10

*E303030,1 BEGIN
#INCLUDE r:\aria4xp\classes\ariaapplication.h
*E303030,1 END

PROCEDURE lpUpdateTableStructure
  LPARAMETERS lcA27SysFiles , lbIndexOnly ,SYDFILES, SYDFLFLD, SYDFIELD, SYDINDEX , UpdTables

  USE (lcA27SysFiles)  +'\syccomp.dbf'  IN 0 SHARED ALIAS syccomp
  SELECT syccomp
  LOCATE
  SET CLASSLIB TO "Classes\main.vcx"
  objRemoteDataAccess=CREATEOBJECT("remotedataaccess")
  nDataSessionID=SET("Datasession")
  SCAN FOR lrunfroma4= .T.
      lcCompID=ccomp_id
      lcConnStr= lpCreateConnStr(ccondriver,cconserver,ccondbname,cconuserid,cconpaswrd)
      lnHandle=SQLSTRINGCONNECT(lcConnStr)
    SELECT (UpdTables)
    LOCATE
    SCAN
      lcTable= cfile_nam 
      lcFile_Ttl= cfile_ttl
      WAIT WINDOW "Update "+lcTable+"-"+ALLTRIM(lcFile_Ttl)+" In company "+lcCompID+", Please Wait." TIMEOUT 1
      IF !lbIndexOnly=.T.
        *E303030,1 BEGIN
        *SET KEY TO PADR(ALLTRIM(lcTable),8) IN (SYDFLFLD)
        SET KEY TO PADR(ALLTRIM(lcTable),FILE_W) IN (SYDFLFLD)
        *E303030,1 END

        objRemoteDataAccess.mcreatetable(lcCompID,lcTable,lcFile_Ttl,SYDFLFLD,SYDFIELD,SYDINDEX,nDataSessionID,lnHandle,.F.,lcConnStr)
      ENDIF
      *E303030,1 BEGIN
      *SET KEY TO PADR(ALLTRIM(lcTable),8) IN (SYDINDEX)
      SET KEY TO PADR(ALLTRIM(lcTable),FILE_W) IN (SYDINDEX)
      *E303030,1 END
      objRemoteDataAccess.mcreateIndex(lcCompID,lcTable,lcFile_Ttl,SYDINDEX,nDataSessionID,lnHandle,lcConnStr)
    ENDSCAN
    SELECT syccomp
  ENDSCAN
  RELEASE objRemoteDataAccess
  RELEASE CLASSLIB  "Classes\main.vcx"
  USE IN syccomp
ENDPROC

*!*************************************************************
*! Name      : lpCreateConnStr
*! Developer : Ahmed Khalifa (AKM)
*! Date      : 08/04/2005
*! Purpose   : Create connection String
*!           :
*!*************************************************************
*! Calls     :
*!             Procedures : None
*!             Functions  : None
*!*************************************************************
*! Passed Parameters  : CCONDRIVER, CCONSERVER, CCONDBNAME, CCONUSERID, CCONPASWRD
*!*************************************************************
*! Returns            : lcConnStr
*!*************************************************************
*! Example   : lpCreateConnStr(CCONDRIVER ,CCONSERVER, CCONDBNAME, CCONUSERID, CCONPASWRD)
*!*************************************************************
PROCEDURE lpCreateConnStr
  LPARAMETERS ccondriver ,cconserver, ccondbname, cconuserid, cconpaswrd
  LOCAL lcConnStr
  lcConnStr = ""
  DO CASE
    CASE ccondriver = 'SQL'
      lcConnStr = "Driver={SQL Server};server="+ALLTRIM(cconserver)+";DATABASE="+ALLTRIM(ccondbname)+;
        ";uid="+ALLTRIM(cconuserid)+";pwd="+ALLTRIM(cconpaswrd)
    CASE ccondriver = 'FOX'
      lcConnStr = "Driver={Microsoft Visual FoxPro Driver};UID=;PWD=;SourceDB="+ALLTRIM(ccondbname)+;
        ";SourceType=DBF;Exclusive=No;BackgroundFetch=Yes;Collate=Machine;Null=Yes;Deleted=Yes;"
  ENDCASE
  RETURN lcConnStr


  *!*************************************************************
  *! Name      : gfSubStr
  *! Developer : Yasser Saad Ibrahime
  *! Date      : 1993-1995
  *! Purpose   : To extract element from string or to convert string to array
  *!*************************************************************
  *! Calls     :
  *!      Called by: ARIA3.PRG
  *!*************************************************************
  *! Passed Parameters  : String to be used
  *!                      poiter to array or element position
  *!                      sparators used in the string
  *!*************************************************************
  *! Returns            : ............
  *!*************************************************************
  *! Example   :
  *!*************************************************************
  * This function will return eather a string part # OR an array of all
  * the string parts according to the type of the second parameter. The
  * firest parameter will be the string or string variable. If the
  * second parameter have a numeric type, the function will return the
  * but if it is an array the function will return the array with each
  *  element having a part from the string.
  *
  *:->
FUNCTION gfSubStr
  PARAMETERS lcString,lnAryOrPos,lcSepta

  lcSubstr  =' '
  lnAryDim  = 1
  lnAryRows = 1
  lnAryCols = 1
  lcSepta   = IIF(TYPE('lcSepta')='C',lcSepta,',')

  IF LEN(ALLTRIM(lcSepta))>1
    lcColSep  = SUBSTR(lcSepta,2,1)
    lcSepta   = LEFT(lcSepta,1)
    lnAryDim  = IIF(OCCURS(lcSepta,lcString)>0,;
      OCCURS(lcSepta,lcString)+;
      IIF(RIGHT(lcString,1)<>lcSepta,1,0),;
      lnAryDim)
    lnAryCols = IIF(OCCURS(lcColSep,lcString)>0,;
      OCCURS(lcColSep,lcString)+;
      IIF(RIGHT(lcString,1)<>lcColSep,1,0),;
      lnAryDim)
    lnAryRows = (lnAryDim+(lnAryCols-1)) / lnAryCols
    lnAryDim  = lnAryDim +(lnAryCols-1)
    lcString  = STRTRAN(lcString,lcColSep,lcSepta)
  ELSE
    lnAryDim = IIF(OCCURS(lcSepta,lcString)>0,;
      OCCURS(lcSepta,lcString)+;
      IIF(RIGHT(lcString,1)<>lcSepta,1,0),;
      lnAryDim)
  ENDIF

  *** Chek if second parameter array or numeric
  DO CASE
      *** If no parameter found assume firest part of string
    CASE TYPE ('lnAryOrPos')='U'
      lnAryOrPos = 1

      *** If array strich it to hold all string parts
    CASE TYPE ('lnAryOrPos') $ 'C,L'
      IF lnAryCols > 1
        DIMENSION lnAryOrPos[lnAryRows,lnAryCols]
      ELSE
        IF ALEN(lnAryOrPos,2) > 0
          DIMENSION lnAryOrPos[lnAryDim,ALEN(lnAryOrPos,2)]
        ELSE
          DIMENSION lnAryOrPos[lnAryDim]
        ENDIF

      ENDIF
      lnAryOrPos  = ' '

  ENDCASE

  FOR lnArElem  = 1 TO lnAryDim
    IF TYPE ('lnAryOrPos')='N'
      lnArElem = lnAryOrPos
    ENDIF

    DO CASE
        *** In case of firest string part
      CASE lnArElem = 1
        lcSubstr = SUBSTR(lcString,1,;
          IIF(lcSepta $ lcString,AT(lcSepta,lcString)-1,LEN(lcString)))

        *** In case of last string part
      CASE lnArElem = lnAryDim
        lcSubstr = SUBSTR(lcString,AT(lcSepta,lcString,lnArElem-1)+1)
        lcSubstr = IIF(RIGHT(lcSubstr,1)=lcSepta,;
          SUBSTR(lcSubstr,1,LEN(lcSubstr)-1),lcSubstr)
        *** In case of any string part from the meddel
      CASE lnArElem > 1
        lcSubstr = SUBSTR(lcString,AT(lcSepta,lcString,lnArElem-1)+1,;
          AT(lcSepta,lcString,lnArElem)-;
          AT(lcSepta,lcString,lnArElem-1)-1)
    ENDCAS

    IF TYPE ('lnAryOrPos')='N'
      RETURN lcSubstr
    ENDIF

    IF lnAryCols > 1
      lnAryOrPos[((lnArElem-1)%lnAryRows)+1,INT((lnArElem-1)/lnAryRows)+1] = lcSubstr
    ELSE
      lnAryOrPos[lnArElem] = lcSubstr
    ENDIF
  ENDFOR