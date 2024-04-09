*********************************************************************
* Procedure to maintane the SYDFILES, SYDFIELD, & SYDFLFLD from the 
* already exist databases files
*********************************************************************
PARAMETERS lcFlNm

SET TALK OFF
llFlFld  = .F.  && To record if SydFlFld file should be closed or not
llField  = .F.  && To record if SydField file should be closed or not
llFiles  = .F.  && To record if SydFiles file should be closed or not
llIndex  = .F.  && To record if SydFiles file should be closed or not
llDbfExt = .F.  && To record if the selected file should be closed or not

lcSysHome   = GETENV('SYSHOME')  && Path of system data files
lcSyshome   = lcSyshome+IIF(!EMPTY(lcSyshome).AND.RIGHT(lcSyshome,1)<>'\','\','')

*** Declare an array with any dimensions, the array will be redimensioned
*** later with the SQL SELECT command to store the analyzed file structure
DECLARE laDbfStruc[1,4]

*** Open the SYDFIELD
IF !USED('SYDFIELD')
   SELECT 0
   USE &gcSysHome.sydField  ORDER 1
   llfield  = .T.
ELSE
   SELECT SYDFIELD
   SET ORDER TO 1
ENDIF

*** Open the SYDFLFLD
IF !USED('SydFlFld')
  SELECT 0
  USE &gcSysHome.sydFlFld ORDER 1
  llFlFlD = .T.
ELSE
  SELECT  SYDFLFLD
  SET ORDER TO 1 
ENDIF

*** Open the SYDFILES
IF !USED('SydFiles')
  SELECT 0
  USE &gcSysHome.sydfiles ORDER 1
  llFiles = .T.
ELSE
  SELECT SYDFILES
  SET ORDER TO 1  
ENDIF

*** Open the SydIndex
IF !USED('SydIndex')
    SELECT 0
    USE &gcSysHome.SydIndex  ORDER 1
    llIndex = .T.
ELSE
   SELECT  SydIndex
   SET ORDER TO 1   
ENDIF

*** Ask the user to choose the data base if it wasn't given
IF TYPE ('lcFlNm') ='U' OR EMPTY(lcFlNm )
  lcFlNm = GETFILE("DBF", "Select database file...")
ENDIF  

IF EMPTY(lcFlNm)

  WAIT lcTNofiles WINDOW NOWAIT

  IF llFlFld 
    SELECT SydFlFld
    USE
  ENDIF
  IF llFiles
    SELECT SydFiles
    USE
  ENDIF
  IF llfield
    SELECT SydField
    USE
  ENDIF
  IF llIndex
    SELECT SydIndex
    USE
  ENDIF
  RETURN
ENDIF

**** Open the selected data base
lcFl_Nam = lcFlNm
lcFlNm   = SUBSTR(lcFlNm,RAT("\",lcFlNm)+1)
lcFlNm   = LEFT(lcFlNm,len(lcFlNm)-4)

IF !USED(lcFlNm)
  SELECT 0
  USE &lcFl_Nam
ELSE
  SELECT &lcFlNm 
  llDbfExt = .T.
ENDIF

*** Create a temp. data base to hold the structure of the selected file
lcFl_Nam   = SUBSTR(lcFl_Nam,RAT("\",lcFl_Nam)+1) && File name.ext without path

lcTempFile = SUBSTR(SYS(2015),1,8)
lnRec_Len  = RECSIZE()
COPY STRUCTURE EXTENDED TO &lcTempFile
SELECT * FROM &lcTempFile INTO ARRAY laDbfStruc
SELECT &lcTempFile
USE
lcUpGrdLvl = IIF(LEFT(lcFl_Nam,2)='SY','S','A')
SELECT SydFlFld
DELETE FOR cFile_Nam = lcFl_Nam
FOR lnCount=1 TO ALEN(laDbfStruc,1)
  IF !SEEK(laDbfStruc(lnCount,1),'SydField')
     *** Update the SYDFIELD 
     INSERT INTO SydField; 
           (cFld_Name,cData_Typ,nFld_Wdth,nFld_Dec,cUpGrdLvl);
           VALUES (laDbfStruc(lnCount,1),laDbfStruc(lnCount,2),;
                  laDbfStruc(lnCount,3),laDbfStruc(lnCount,4),;
                  lcUpGrdLvl)
  ENDIF
  *** Update the SYDFLFLD
  INSERT INTO SydFlFld (cFile_Nam,cFld_Name,nFld_Pos,cUpGrdLvl);
         VALUES (lcFlNm,laDbfStruc(lnCount,1),lnCount,lcUpGrdLvl)
ENDFOR
SELECT SYDINDEX
DELETE FOR cFile_Nam = lcFlNm
SELECT &lcFlNm 
lcCur_Tag = SYS(22)
lnCount = 1
*** Update the SYDINDEX
lcIndx_Nam = IIF(ATC("\",CDX(1))=0,;
             SUBSTR(CDX(1),RAT(":",CDX(1))+1),;
             SUBSTR(CDX(1),RAT("\",CDX(1))+1))
IF !EMPTY(CDX(1))              
  DO WHILE !EMPTY(TAG(lcFlNm,lnCount))
    SET ORDER TO lnCount
    lcFile_Tag =  TAG(lcFlNm,lnCount)
    lcIndx_Exp =  SYS(14,lnCount)
    llAscend   =  IIF("DESC" $ SET('ORDER'),.F.,.T.)
    INSERT INTO SydIndex;
           (cIndx_Nam,cFile_Nam,cFile_Tag,cIndx_Exp,cUpGrdLvl,lAscend);
           VALUES (lcIndx_Nam,lcFlNm,lcFile_Tag,lcIndx_Exp,;
                   lcUpGrdLvl,llAscend)
    lnCount=lnCount+1
  ENDDO
  IF !EMPTY(lcCur_Tag )
    SET ORDER TO TAG &lcCur_Tag
  ENDIF
ENDIF

*** Update the SYDFILES
SELECT SydFiles
IF !SEEK(lcFl_Nam)
  INSERT INTO SydFiles;
         (cFile_Nam,nRec_Len,nFld_No,nIndx_No,cUpGrdLvl);
         VALUES (lcFlNm,lnRec_Len,ALEN(laDbfStruc,1),lnCount-1,;
                 lcUpGrdLvl)
ELSE
  REPLACE nRec_Len WITH lnRec_Len, nFld_No WITH ALEN(laDbfStruc,1),;
          nIndx_No WITH lnCount-1
ENDIF
IF llFlFld 
  SELECT SydFlFld
  USE
ENDIF
IF llFiles
  SELECT SydFiles
  USE
ENDIF
IF llfield
  SELECT SydField
  USE
ENDIF
IF llIndex
  SELECT SydIndex
  USE
ENDIF
ERASE lcTempFile+".dbf"
IF !llDbfExt
  SELECT &lcFlNm 
  USE
ENDIF  
