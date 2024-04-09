	****************************************************************************
*: Program file  : ICASI02.PRG
*: Program desc. : Convert Additional Style Information.
*: System        : Aria Apparel System (A27).
*: Module        : Inventory Contol  (IC)
*: Developer     : ABDOU ELGENDI - (ABD) Due to C#101987,1
*: Date          : 09/25/2000
*:**************************************************************************
*: Calls 
*:               : FUNCTIONS  : lfOpenFils , lfConvert , lfUpdatErr , 
*:               :              lfClosFils , 
*:               -----------------------------------------------------------
*:               : PROCEDURE  : 
*:**************************************************************************
*: Passed Parameters  : None
*:**************************************************************************
*:C101987,1           : New Convert Program.
*:**************************************************************************
*:Modifications :
*:B603947,1 ABD 10/08/2000 Fix bug that The program has rejected some of the
*:B603947,1 ABD            he didn't found them however those styles we have 
*:B603947,1 ABD            them in style file.
*:B603947,1 ABD            Anther bug I found that the program has rejected some 
*:B603947,1 ABD            styles saying that the code does not exist in our code file. 
*:B603956,1 ABD 10/11/2000 1)The program updates the file with the date as is without setting
*:B603956,1 ABD              the "century" on and all dates in the file are updated with
*:B603956,1 ABD              1900 instead of 2000.
*:B603956,1 ABD            2)Some style are missing the "Kids Size Group" code.  
*:B603964,1 WAB 10/17/2000 add filed Contents to convert program
*:**************************************************************************
*:
*-- Declaration variables.
STORE .F. To llCanConvt
STORE ''  To lcText , lcFulDir
*-- End Declaration variables.

*- Create file hold the rejected reson
lcErrors    = gfTempName()
CREATE TABLE (gcWorkDir+lcErrors) (COMMENTS C(100))

*- Create the file
*- Initializing name of fildes in the filed.
DIMENSION laStr[11,4]  && Array of the Structure of the *.CSV File

*- Field No. 1 [Field Name]
laStr[1,1] = 'STYLE'
laStr[2,1] = 'COLOR1'
laStr[3,1] = 'COLOR2'
laStr[4,1] = 'CLRNAME'
laStr[5,1] = 'LONGDESC'
laStr[6,1] = 'CONTENTS'
laStr[7,1] = 'STARTDAT'
laStr[8,1] = 'SOLDOUTE'
laStr[9,1] = 'GROUP'
laStr[10,1]= 'MRSP'
laStr[11,1]= 'SIZEGROP'

*- Initializing character filed
*- Field No. 2 [Field Type]
FOR lnCnt = 1 TO 11
  laStr[lnCnt,2]='C'
ENDFOR
*- Field No. 3 [Field Width]
laStr[1,3] = 08
laStr[2,3] = 05
laStr[3,3] = 03
laStr[4,3] = 20
laStr[5,3] = 40
*B603947,1 ABD Modify the width of the filed to hold all data. [begin]
*laStr[6,3] = 40
laStr[6,3] = 80
*B603947,1 ABD [End]
laStr[7,3] = 10
laStr[8,3] = 13
laStr[9,3] = 30
laStr[10,3]= 10
laStr[11,3]= 30


*- Field No. 4 [Descimal Field]
FOR lnCnt = 1 TO 11
  laStr[lnCnt,4]=0
ENDFOR

DIMENSION laSFields[11] && Array of fields of selected file
DIMENSION laFields[11]  && Array of correct fields of  file
STORE '' To laFields ,laSFields

laFields[1] = 'STYLE'
laFields[2] = 'COLOR'
laFields[3] = ''
laFields[4] = 'CLR NAME'
laFields[5] = 'LONG DESCRIPTION'
laFields[6] = 'CONTENTS'
laFields[7] = 'START DATE'
laFields[8] = 'SOLD OUT DATE'
laFields[9] = 'GROUP'
laFields[10]= 'MRSP'
laFields[11]= 'SIZE GROUP'

*- Messeage text :- Are you sure you want to convert additional style information ?
*- Messeage No.  :-  40169.
*- Button text   :- Proceed  -- Cancel
*- Button No.    :- 40003.
IF gfModalGen('QRM40169B40003','DIALOG','convert additional style information') = 2
  RETURN
ENDIF

*- temp file use for convert data.
lcTmpData = gfTempName()   && Temp Table of converted data
CREATE TABLE (gcWorkDir+lcTmpData) FROM ARRAY laStr
lcTmpIndex = gfTempName()   && Temp Index of Code Table


DO WHILE .T.
  lcFileName = GETFILE('CSV' , 'Select the file:' , 'Convert')
  IF EMPTY(lcFileName)  && No files are selected
    *- Messeage text :- You did not select any file to convert , Convert will be Canceled !
    *- Messeage No.  :-  00000
    *- Button text   :- Yes  --  No
    *- Button No.    :- 42002.
    IF gfModalGen("TRM00000B42002","DIALOG",.F.,.F.,'You did not select any file to convert, are you sure you want to cancel the conversion?.') = 1
      llCanConvt = .T.
      EXIT
    ELSE
      LOOP
    ENDIF   
  ENDIF

  IF RIGHT(lcFileName,3)<>'CSV' && the selected file is not *.CSV
    *- Messeage text :- The selected file is not of proper format, it must be Excel file with *.CSV. Format.
    *- Messeage No.  :- 00000
    *- Button text   :- OK
    *- Button No.    :- 42000.
    =gfModalGen("TRM00000B42000","DIALOG",.F.,.F.,'The selected file is not of proper format, it must be Excel file with *.CSV. Format.')
    LOOP
  ELSE
    SELECT (lcTmpData)
    ZAP
    APPEND FROM &lcFileName DELIMITED
    *-- go to first record
    LOCATE
    SKIP
    SCATTER TO laSFields
    SKIP-1
    FOR lnCnt = 1 TO 11
      IF laSFields[lnCnt] <> laFields[lnCnt]  && the structure of selected file is not correct
        *- Messeage text :- The structure of selected file is not correct
        *- Messeage No.  :- 00000
        *- Button text   :- OK
        *- Button No.    :- 42000.
        =gfModalGen("TRM00000B42000","DIALOG",.F.,.F.,'The structure of selected file is not correct')
        llCanConvt = .T.
        EXIT
      ENDIF
    ENDFOR
    IF llCanConvt   && the structure of selected file is not correct
      llCanConvt = .F.
      LOOP
    ENDIF
    *- Messeage text :- Are you sure you want to Convert the selected file 
    *- Messeage No.  :- 00000
    *- Button text   :- OK
    *- Button No.    :- 42002.
    IF gfModalGen("QRM00000B42002","DIALOG",.F.,.F.,'Are you sure you want to convert the selected file ?') = 2
      LOOP
    ENDIF
    EXIT
  ENDIF
ENDDO

IF llCanConvt
  *- call function to open needed files to convert.
  = lfClosFils ()
  RETURN
ENDIF

*- call function to open needed files to convert.

IF !lfOpenFils ()
  RETURN
ENDIF
*- call function to convert the data.
= lfConvert ()
*- call function to open needed files to convert.
= lfClosFils ()

RETURN
*-End Of Code.
*:**************************************************************************
*: Name      : lfOpenFils
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 09/26/2000
*: Purpose   : open needed files to convert.
*:**************************************************************************
*: Called from : Program.
*:**************************************************************************
*: Passed Parameters : None.
*:**************************************************************************
*: Return      : None
*:**************************************************************************
*: Example     : =lfOpenFils()
*:**************************************************************************
*
FUNCTION lfOpenFils

*- Open Style File
=gfOpenFile(gcDataDir+'STYLE',gcDataDir+'STYLE','SH')
*- Open Codes File
=gfOpenFile(gcDataDir+'CODES' ,gcDataDir+'CODES' ,'SH')
SELECT CODES
*B603947,1 ABD Get the discription in Upper Case. [Begin]
*INDEX ON ALLTRIM(CFLD_NAME) + ALLTRIM(CDISCREP) TAG (lcTmpIndex) OF (gcWorkDir+lcTmpIndex) 
INDEX ON ALLTRIM(CFLD_NAME) + ALLTRIM(UPPER(CDISCREP)) TAG (lcTmpIndex) OF (gcWorkDir+lcTmpIndex) 
*B603947,1 ABD [End]

*-- Cheak if fiels code was setup or not.
SELECT CODES
SET ORDER TO TAG Ccode_no IN CODES
IF !SEEK('N'+'CSTYGRP','CODES') .OR. !SEEK('N'+'CKIDSZGRP','CODES')
  *- Message No. : 40150
  *- Message Txet: One or more codes have not been setup. Please setup and try again.
  *- Button No   : 00000
  *- Button Text : Ok
  =gfModalGen('TRM40150B00000','ALERT')
  RETURN .F.
ENDIF


*-End OF lfOpenFils.
*:**************************************************************************
*: Name      : lfConvert
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 09/26/2000
*: Purpose   : Convert data from temp file to style file.
*:**************************************************************************
*: Called from : Program.
*:**************************************************************************
*: Passed Parameters : None.
*:**************************************************************************
*: Return      : None
*:**************************************************************************
*: Example     : =lfConvert()
*:**************************************************************************
*
FUNCTION lfConvert

SELECT (lcTmpData)
LOCATE
*-- Deleting the 2 recorde contined fields names.
DELETE ALL For EMPTY(STYLE)
LOCATE
*--Deleting the record contined fields names
DELETE

SELECT CODES
SET ORDER TO TAG (lcTmpIndex) IN CODES

SELECT (lcTmpData)
SCAN
  STORE .T. To llCstyGrp , llKidSzGrp
  STORE '' To lcCstyGrp  , lcKidSzGrp
  IF SEEK(PADR(STYLE,12)+'-'+ALLTRIM(COLOR1)+COLOR2,'STYLE')
   *B603956,1 ABD            2)Some style are missing the "Kids Size Group" code. [Begin]
    *- Cheak on code if found the fashion group in codes file.
    *IF !SEEK('CSTYGRP' + ALLTRIM(GROUP),'CODES')
    *  lcText = 'The fashion group '+ALLTRIM(UPPER(GROUP))+ ' for style '+PADR(STYLE,12)+'-'+ALLTRIM(COLOR1)+COLOR2+' did not found in codes file.'
    *  = lfUpdatErr(lcText)
    *  llCstyGrp  = .F.
    *ELSE      
    *  *- Get the Code No.
    *  lcCstyGrp = CODES.cCode_no
    *ENDIF
    
    *IF !SEEK('CKIDSZGRP' + ALLTRIM(UPPER(SIZEGROP)),'CODES')
    *  lcText = 'The kids size group '+ALLTRIM(SIZEGROP)+ ' for style '+PADR(STYLE,12)+'-'+ALLTRIM(COLOR1)+COLOR2+' did not found in codes file.'
    *  = lfUpdatErr(lcText)
    *  llKidSzGrp = .F.
    *ELSE
    *  *- Get the Code No.
    *  lcKidSzGrp = CODES.cCode_no  
    *ENDIF
    WAIT WINDOW 'Please wait .... Update '+PADR(STYLE,12)+'-'+ALLTRIM(COLOR1)+COLOR2  NOWAIT
    
    IF EMPTY(ALLTRIM(GROUP))
      lcText = 'The fashion group is empty for style '+PADR(STYLE,12)+'-'+ALLTRIM(COLOR1)+COLOR2
      = lfUpdatErr(lcText)
      llCstyGrp  = .F.
    ELSE
      IF SEEK('CSTYGRP' + ALLTRIM(UPPER(GROUP)),'CODES')
        lcCstyGrp = CODES.cCode_no
      ELSE
        lcText = 'The fashion group '+ALLTRIM(UPPER(GROUP))+ ' for style '+PADR(STYLE,12)+'-'+ALLTRIM(COLOR1)+COLOR2+' did not found in codes file.'
        = lfUpdatErr(lcText)
        llCstyGrp  = .F.
      ENDIF
    ENDIF
    
    IF EMPTY(ALLTRIM(SIZEGROP))
      lcText = 'The kids size group is empty for style '+PADR(STYLE,12)+'-'+ALLTRIM(COLOR1)+COLOR2
      = lfUpdatErr(lcText)
      llKidSzGrp = .F.
    ELSE
      IF SEEK('CKIDSZGRP' + ALLTRIM(UPPER(SIZEGROP)),'CODES')
        lcKidSzGrp = CODES.cCode_no
      ELSE
        lcText = 'The kids size group '+ALLTRIM(SIZEGROP)+ ' for style '+PADR(STYLE,12)+'-'+ALLTRIM(COLOR1)+COLOR2+' did not found in codes file.'
        = lfUpdatErr(lcText)
        llKidSzGrp = .F.
      ENDIF
    ENDIF
    *B603956,1 ABD [End]
    SELECT STYLE
    =RLOCK()
    *B603956,1 ABD 1)The program updates the file with the date as is without setting
    *B603956,1 ABD the "century" on and all dates in the file are updated with
    *B603956,1 ABD              1900 instead of 2000. [Begin]
    *REPLACE START     WITH CTOD(&lcTmpData..STARTDAT)          ,;
    *        SOLDOUT   WITH CTOD(LEFT(&lcTmpData..SOLDOUTE,10)) ,;
    *        CSTYGRP   WITH lcCstyGrp                           ,;
    *        CKIDSZGRP WITH lcKidSzGrp
    *--lfWriteY2k()
    
    lnMrsp     = &lcTmpData..MRSP
    *B603964,1 WAB (START) add update field content1 with contents field 
    *REPLACE START   WITH lfWriteY2k(CTOD(&lcTmpData..STARTDAT))         ,;
    *        SOLDOUT WITH lfWriteY2k(CTOD(LEFT(&lcTmpData..SOLDOUTE,10))),;
    *        NMSRP   WITH &lnMrsp
 
    REPLACE START    WITH lfWriteY2k(CTOD(&lcTmpData..STARTDAT))         ,;
            SOLDOUT  WITH lfWriteY2k(CTOD(LEFT(&lcTmpData..SOLDOUTE,10))),;
            NMSRP    WITH &lnMrsp,;
            Content1 WITH LEFT(ALLTRIM(&lcTmpData..CONTENTS),20)
            
    *B603964,1 WAB (END) 
    IF llCstyGrp
      REPLACE CSTYGRP   WITH lcCstyGrp
    ENDIF
    IF llKidSzGrp 
      REPLACE CKIDSZGRP WITH lcKidSzGrp   
    ENDIF   
    *B603956,1 ABD [End]
    
    UNLOCK
    SELECT(lcTmpData)
  ELSE
    lcText = 'Style '+STYLE+Color1+Color2+' did not found in style file.'
    = lfUpdatErr(lcText)
  ENDIF
ENDSCAN

*- Cheak if We have rejected record.
SELECT(lcErrors)
LOCATE

IF EOF()
  *- Messeage text :- All data are converting correctly.
  *- Messeage No.  :- 00000
  *- Button text   :- OK
  *- Button No.    :- 42000.
  =gfModalGen("INM00000B42000","DIALOG",.F.,.F.,'All data had been converted correctly.')
ELSE
  COPY TO (SUBSTR(lcFileName,1,RAT('\',lcFileName))+'Rejected.XLS') TYPE XLS
  *- Messeage text :- 'Some records could not be converted, please check the "Rejected .XLS" file.'
  *- Messeage No.  :- 00000
  *- Button text   :- OK
  *- Button No.    :- 42000.
  =gfModalGen("INM00000B42000","DIALOG",.F.,.F.,'Some records could not be converted, please check the '+'"Rejected .XLS"'+' file.')
ENDIF


*-End OF lfConvert.
*:**************************************************************************
*: Name      : lfUpdatErr
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 09/26/2000
*: Purpose   : Insert rejected record.
*:**************************************************************************
*: Called from : Program.
*:**************************************************************************
*: Passed Parameters : None.
*:**************************************************************************
*: Return      : None
*:**************************************************************************
*: Example     : =lfUpdatErr()
*:**************************************************************************
*
FUNCTION lfUpdatErr
PARAMETERS Text

INSERT INTO (lcErrors) (Comments) VALUES (Text)

*- End Of lfUpdatErr.
*:**************************************************************************
*: Name      : lfClosFils
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 09/26/2000
*: Purpose   : Close opened Files that used to convert and erase tepmfiles.
*:**************************************************************************
*: Called from : Program.
*:**************************************************************************
*: Passed Parameters : None.
*:**************************************************************************
*: Return      : None
*:**************************************************************************
*: Example     : =lfClosFils()
*:**************************************************************************
*
FUNCTION lfClosFils

*- Close Style File
IF USED('STYLE')
  =gfCloseFile('STYLE')
ENDIF
IF USED('CODES')
  =gfCloseFile('CODES')
ENDIF
*-- Delete  temporary file.
IF USED(lcTmpData)
  USE IN (lcTmpData)

  IF FILE(gcWorkDir+lcTmpData+'.DBF') 
    ERASE(gcWorkDir+lcTmpData+'.DBF')
  ENDIF
  IF FILE(gcWorkDir+lcTmpData+'.FPT') 
    ERASE(gcWorkDir+lcTmpData+'.FPT')
  ENDIF
ENDIF


IF USED(lcErrors)
  USE IN (lcErrors)

  IF FILE(gcWorkDir+lcErrors+'.DBF') 
    ERASE(gcWorkDir+lcErrors+'.DBF')
  ENDIF
  IF FILE(gcWorkDir+lcErrors+'.FPT') 
    ERASE(gcWorkDir+lcErrors+'.FPT')
  ENDIF
ENDIF

IF FILE(gcWorkDir+lcTmpIndex+'.CDX') 
  ERASE(gcWorkDir+lcTmpIndex+'.CDX')
ENDIF



*-End OF lfClosFils.
*:**************************************************************************
*: Name      : lfWriteY2k
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 09/26/2000
*: Purpose   : Convert date from to Y2K.
*:**************************************************************************
*: Called from : Program.
*:**************************************************************************
*: Passed Parameters : lcParam
*:**************************************************************************
*: Return      : None
*:**************************************************************************
*: Example     : =lfWriteY2k()
*:**************************************************************************
*B603956,1 ABD The program updates the file with the date as is without setting
*B603956,1 ABD the "century" on and all dates in the file are updated with
*B603956,1 ABD              1900 instead of 2000. [Begin]
FUNCTION lfWriteY2k
Parameter lcParam

PRIVATE lcY2kCent , ldY2kDate , lcY2kObj , lnY2kIncr , lnY2KNew , lcY2KAlias

IF YEAR(lcParam) <= 1930
  lcY2KCent = SET('CENT')
  SET CENT ON
  lcY2KObj     = SYS(18)
  lnY2KIncr    = YEAR(lcParam)
  lnY2KNew     = 2000+(lnY2KIncr/100-int(lnY2KIncr/100))*100
  ldY2KDate    = DTOC(lcParam)
  ldY2KDate    = STRTRAN(ldY2KDate , ALLTRIM(STR(YEAR(lcParam))) , ALLTRIM(STR(lnY2KNew)))
  ldY2KDate    = CTOD(ldY2KDate)
  lcParam      = ldY2KDate
  SET CENT &lcY2KCent
ENDIF
RETURN lcParam
*-- End Of lfWriteY2k.
*:**************************************************************************