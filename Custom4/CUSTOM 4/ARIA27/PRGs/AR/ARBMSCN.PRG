*:************************************************************************************************
*: PROGRAM   : ARBMSCN.PRG                FOR : BMS
*: MODULE    : Aria Apparel Series.
*: DATE      : 01/24/2000
*: DESC.     : Convert data program.
*: Developer : Adel Mohammed El Gazzar (ADEL)
*: Refer to  : (101757)
*:************************************************************************************************
*: Calls : 
*:         FUNCTION  : lfBrow()
*:                   : lfRefresh()
*:                   : lfvOkBut()
*:					 : lfOpenFile()
*:					 : lfConvert()
*:					 : lfAddAct()
*:					 : lfNewCode()
*:					 : lfAddCont()
*:					 : lfAddNotes() 
*:					 : lfClosFile()
*:************************************************************************************************
*: Modifications:
*: B603466,1 ADEL 22/02/2000 Take the account code from (Company) field instead of (Name) one. Be sure no blanks
*: B603466,1                 found in the account code. Compare UPPER(Company) with UPPER(BTName). When comparing by
*: B603466,1                 Don't update phone or fax with blanks. Show aduplicate report.
*:************************************************************************************************

*---Initialize needed variables.
*-- lcDirect     && Holds the selected data directory by the user.
*-- lcOkBMP      && Holds the OK buttom bmp.
*-- lcCanclBMP   && Holds the CANCEL buttom bmp.
*-- lnNoOfCov    && Holds the no of the converted records.
*-- lnNoOfNon    && Holds the no of the unconverted records.
*-- llAddRec     && Will we add a new account?

*B603466,1 (Begin) Create a temp file to hold reported duplicate records
*lcTempFile = gfTempName()
*B603466,1 (End)

STORE ' ' TO lcDirect
lcOkBmp    = gcBMPHome + 'OK.BMP'
lcCanclBmp = gcBMPHome + 'CAN.BMP'
STORE 0 TO lnNoOfCov,lnNoOfNon
*---Call the directory screen.
DO (gcScrDir+gcWinAppl+"\BMSCNVR.SPR")


*!*************************************************************
*! Name      : lfBrow
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 01/24/2000
*! Purpose   : Validate the browse bitton.
*!*************************************************************
FUNCTION lfBrow

lcDirect = GETDIR()
IF !EMPTY(lcDirect)
  SHOW GET pbOkBut ENABLE
ELSE
  SHOW GET pbOkBut DISABLE
ENDIF
=lfRefresh()

*!*************************************************************
*! Name      : lfvOkBut
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 01/24/2000
*! Purpose   : Validate the OK bitton.
*!*************************************************************
FUNCTION lfvOkBut

*-- False this variable to check the current directory.
*-- Initialize no of converted and unconverted records.
STORE 0 TO lnNoOfCov,lnNoOfNon
*--Open files.
IF !lfOpenFile()
  RETURN
ENDIF
*--Start converting
=lfConvert()
*--Close files used by 'USE..' comamnd rather than 'gfOpenFile' function.
=lfClosFile()

*!*************************************************************
*! Name      : lfOpenFile
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 01/24/2000
*! Purpose   : Open files.
*!*************************************************************
FUNCTION lfOpenFile

*--Open convert files.
IF !FILE(lcDirect+'FMR.DBF')
    *** "File 'FMR.dbf' does not exist in the current directory." ***
    *** < Ok > ***
    = gfModalGen('TRM00000B00000',.F.,.F.,.F.,"File 'FMR.dbf' does not exist in the current directory.")    
    RETURN .F.
ENDIF
IF !FILE(lcDirect+'FMR_B.DBF')
    *** "File 'FMR_B.dbf' does not exist in the current directory." ***
    *** < Ok > ***
    = gfModalGen('TRM00000B00000',.F.,.F.,.F.,"File 'FMR_B.dbf' does not exist in the current directory.")    
    RETURN .F.
ENDIF
IF !FILE(lcDirect+'FMR_B.NDX')
    *** "File 'FMR_B.NDX' does not exist in the current directory." ***
    *** < Ok > ***
    = gfModalGen('TRM00000B00000',.F.,.F.,.F.,"File 'FMR_B.ndx' does not exist in the current directory.")    
    RETURN .F.
ENDIF
*--Open Original files.
USE lcDirect+'FMR.DBF' IN 0
SELECT 0
USE lcDirect+'FMR_B.DBF'
SET INDEX TO lcDirect+'FMR_B.NDX'
*--Open master files.
=gfOpenFile(gcDataDir+'Customer',gcDataDir+'Customer','SH')
=gfOpenFile(gcDataDir+'Contact',gcDataDir+'Contact','SH')
=gfOpenFile(gcDataDir+'NotePad',gcDataDir+'NotePad','SH')

*B603466,1 (Begin) Create a temp file to hold reported duplicate records
SELECT FMR
COPY STRUCTURE TO (gcWorkDir+'DUBBMS')
*B603466,1 (End)

*!*************************************************************
*! Name      : lfClosFile
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 01/24/2000
*! Purpose   : Concert data.
*!*************************************************************
FUNCTION lfConvert

SELECT FMR
SCAN
  WAIT WINDOW 'Converting data for : ' + Name  NOWAIT
  *--Flag to see if it's ok to add a new customer.
  SELECT CUSTOMER
  SET ORDER TO CUSTOMPH
  *B603466,1 (Begin) Remove all blanks from phone no when comparing.
  *llAddRec = !SEEK('M'+FMR.Phone1)
  llAddRec  = !SEEK('M'+STRTRAN(FMR.Phone1,' ' ,''))
  *B603466,1 (End)

  SET ORDER TO CUSTOMER
  *--Will we careate a new customer?
  IF llAddRec
    SELECT CUSTOMER
    =SEEK('M')
    *B603466,1 (Begin) Compare UPPER(Company) with UPPER(BTName).
    *LOCATE REST WHILE type+account+store = 'M' FOR BTName = FMR.NAME
    LOCATE REST WHILE type+account+store = 'M' FOR UPPER(BTName) = UPPER(FMR.Company)
    *B603466,1 (End)
    llAddRec = !FOUND()
  ENDIF
  IF llAddRec
    *--Does the phone exist in the contact file?
    SELECT CONTACT
    =SEEK('C')
    *B603466,1 (Begin) Remove blanks from phone.
    *LOCATE REST WHILE cconttype+ccont_id+store+contact = 'C' FOR ALLTRIM(Phone) = ALLTRIM(FMR.Phone1)
    LOCATE REST WHILE cconttype+ccont_id+store+contact = 'C' FOR ALLTRIM(Phone) = STRTRAN(FMR.Phone1,' ' ,'')
    *B603466,1 (End)
    llAddRec = !FOUND()

  ENDIF
  IF llAddRec
    lnNoOfCov = lnNoOfCov + 1
    *--Add a new account.
    =lfAddAct()
    *--Add notes for the account.
    =lfAddNotes()
    *--Add new contacts.
    =lfAddCont()
  ELSE
    *--Increase the no of unconverted records.
    lnNoOfNon = lnNoOfNon +1

    *B603466,1 (Begin) Create a temp file to hold reported duplicate records
    SELECT FMR
    SCATTER MEMVAR
    INSERT INTO (gcWorkDir+'DUBBMS') FROM MEMVAR
    *B603466,1 (End)
  ENDIF
ENDSCAN
= gfModalGen('TRM00000B00000',.F.,.F.,.F.,"No. of accounts added is  " + ALLTRIM(STR(lnNoOfCov,7)) + " And No. of dublicates not added is " + ALLTRIM(STR(lnNoOfNon,7)))

*B603466,1 (Begin) Create a temp file to hold reported duplicate records
IF lnNoOfNon > 0
  =gfModalGen('TRM00000B00000',.F.,.F.,.F.,"Dupplicate records stored in "+gcWorkDir+'DUBBMS.DBF')
ENDIF
*B603466,1 (End)

*!*************************************************************
*! Name      : lfAddAct
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 01/24/2000
*! Purpose   : Add a new account.
*!*************************************************************
FUNCTION lfAddAct

SELECT CUSTOMER
*--Get a new account code
lcNewAct=lfNewCode()
APPEN BLANK
*B603466,1 (Begin) Remove blanks from phone no and fax.
*REPLACE TYPE       WITH 'M' ,;
        BillTo     WITH 'M' ,;
        Account    WITH lcNewAct ,;
        STATUS     WITH 'P',;
        Link_Code  WITH 'DEFDEF',;
        BtName     WITH FMR.Company,;
        StName     WITH FMR.Company,;
        DBA        WITH FMR.LabelTag,;
        Consol     WITH 'Y',;
        Phone1     WITH ALLTRIM(FMR.Phone1),;
        Note       WITH FMR.Comment,;
        SalesRep   WITH 'JIM',;
        Comm       WITH 10,;
        Priority   WITH '5',;
        Region     WITH '000017',;
        Class      WITH '000016',;
        cTermCode  WITH '000001',;
        ShipVia    WITH '000027',;
        cDivision  WITH 'AAS',;
        SPCINST    WITH '000028',;
        Pricelvl   WITH 'A',;
        CINSUR     WITH 'Y',;
        SKUTMPL    WITH 'DEF',;
        PRNT_STATM WITH 'N',;
        cCont_Code WITH 'USA',;
        CCONT_COD2 WITH 'USA',;
        CCURRCODE  WITH 'USD',;
        cAddress1  WITH FMR.ADDR1,;
        cAddress2  WITH FMR.ADDR2,;
        cAddress3  WITH FMR.CITY,;
        cAddress4  WITH FMR.STATE,;
        cAddress5  WITH FMR.ZIP,;
        cAddress6  WITH FMR.COUNTRY,;
        cAddress12 WITH FMR.ADDR1,;
        cAddress22 WITH FMR.ADDR2,;
        cAddress32 WITH FMR.CITY,;
        cAddress42 WITH FMR.STATE,;
        cAddress52 WITH FMR.ZIP,;
        cAddress62 WITH FMR.COUNTRY,;
        FAX        WITH ALLTRIM(FMR.FAX1),;
        cSlsgllink WITH 'DEF',;
        cAdd_User  WITH 'BMS',;
        cAdd_Time  WITH TIME(),;
        dAdd_Date  WITH gdSysDate ,;
        USR_DFND3  WITH DTOC(FMR.Start)
REPLACE TYPE       WITH 'M' ,;
        BillTo     WITH 'M' ,;
        Account    WITH lcNewAct ,;
        STATUS     WITH 'P',;
        Link_Code  WITH 'DEFDEF',;
        BtName     WITH FMR.Company,;
        StName     WITH FMR.Company,;
        DBA        WITH FMR.LabelTag,;
        Consol     WITH 'Y',;
        Phone1     WITH STRTRAN(FMR.Phone1,' ' ,''),;
        Note       WITH FMR.Comment,;
        SalesRep   WITH 'JIM',;
        Comm       WITH 10,;
        Priority   WITH '5',;
        Region     WITH '000017',;
        Class      WITH '000016',;
        cTermCode  WITH '000001',;
        ShipVia    WITH '000027',;
        cDivision  WITH 'AAS',;
        SPCINST    WITH '000028',;
        Pricelvl   WITH 'A',;
        CINSUR     WITH 'Y',;
        SKUTMPL    WITH 'DEF',;
        PRNT_STATM WITH 'N',;
        cCont_Code WITH 'USA',;
        CCONT_COD2 WITH 'USA',;
        CCURRCODE  WITH 'USD',;
        cAddress1  WITH FMR.ADDR1,;
        cAddress2  WITH FMR.ADDR2,;
        cAddress3  WITH FMR.CITY,;
        cAddress4  WITH FMR.STATE,;
        cAddress5  WITH FMR.ZIP,;
        cAddress6  WITH FMR.COUNTRY,;
        cAddress12 WITH FMR.ADDR1,;
        cAddress22 WITH FMR.ADDR2,;
        cAddress32 WITH FMR.CITY,;
        cAddress42 WITH FMR.STATE,;
        cAddress52 WITH FMR.ZIP,;
        cAddress62 WITH FMR.COUNTRY,;
        FAX        WITH STRTRAN(FMR.FAX1,' ' ,''),;
        cSlsgllink WITH 'DEF',;
        cAdd_User  WITH 'BMS',;
        cAdd_Time  WITH TIME(),;
        dAdd_Date  WITH gdSysDate ,;
        USR_DFND3  WITH DTOC(FMR.Start)
*B603466,1 (End) 

*!*************************************************************
*! Name      : lfNewCode
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 01/24/2000
*! Purpose   : Get a new account code.
*!*************************************************************
FUNCTION lfNewCode

*------ F I R S T   C H E C K 
*--(First three chrs with 2 numbers from 00 to 99)
*B603466,1 (Begin) Take 2 blank-free chrs from Company field instead of Name one.
*lcFrstPart = SUBSTR(ALLTRIM(FMR.NAME),1,3)
lcFrstPart = SUBSTR(STRTRAN(FMR.COMPANY,' ',''),1,3)
*B603466,1 (End)
*--Numeric check
FOR lnRec = 0 to 99
  IF !SEEK('M'+lcFrstPart+PADL(lnRec,2,'0'))
    RETURN( lcFrstPart+PADL(lnRec,2,'0') )
  ENDIF
ENDFOR
*--Charater check
*--(First three chrs with 2 characters from 0A TO 9Z)
lcAll = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
FOR lnRec = 0 to 9
  lcRec = STR(lnRec,1)
  FOR lnRecC = 1 TO 26
    IF !SEEK('M'+lcFrstPart+STR(lnRec,1)+SUBSTR(lcAll,lnRecC,1))
      RETURN ( lcFrstPart+STR(lnRec,1)+SUBSTR(lcAll,lnRecC,1))
    ENDIF
  ENDFOR
ENDFOR
*------ S E C O N D   C H E C K
*--(First two chrs with 3 numbers from 000 to 999)
*B603466,1 (Begin) Take 3 blank-free chrs from Company field instead of Name one.
*lcSecnPart = SUBSTR(ALLTRIM(FMR.NAME),2)
lcSecnPart = SUBSTR(STRTRAN(FMR.COMPANY,' ',''),1,2)
*B603466,1 (End)
*--Numeric check
FOR lnRec = 0 to 999
  IF !SEEK('M'+lcSecnPart+PADL(lnRec,3,'0'))
    RETURN( lcSecnPart+PADL(lnRec,3,'0') )
  ENDIF
ENDFOR

*!*************************************************************
*! Name      : lfAddCont
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 01/25/2000
*! Purpose   : Get new conatcts.
*!*************************************************************
FUNCTION lfAddCont

SELECT CONTACT
*--First conatct
APPEN BLANK
*B603466,1 (Begin) Remove all blanks from phone no AND FAX NO.
*REPLACE cContType WITH 'C',;
        cCont_Id  WITH Customer.Account,;
        Contact   WITH FMR.Name,;
        Phone     WITH ALLTRIM(FMR.Phone1),;
        Fax       WITH ALLTRIM(FMR.FAX1),;
        cContTtl  WITH FMR.Title,;
        cAdd_User WITH 'BMS',;
        cAdd_Time WITH TIME(),;
        dAdd_Date WITH gdSysDate 
REPLACE cContType WITH 'C',;
        cCont_Id  WITH Customer.Account,;
        Contact   WITH FMR.Name,;
        Phone     WITH STRTRAN(FMR.Phone1,' ' , '' ),;
        Fax       WITH STRTRAN(FMR.FAX1, ' ' , ''),;
        cContTtl  WITH FMR.Title,;
        cAdd_User WITH 'BMS',;
        cAdd_Time WITH TIME(),;
        dAdd_Date WITH gdSysDate 
*B603466,1 (End)
        
*--Second and third contacts.
FOR lnRec =  2 TO 3
  lcRec = STR(lnRec,1)
  IF !EMPTY(FMR.NAME&lcRec)
    APPEN BLANK
    *B603466,1 (Begin) Remove all blanks from phone no AND FAX NO.    
    *REPLACE cContType WITH 'C',;
            cCont_Id  WITH Customer.Account,;
            Contact   WITH FMR.Name&lcRec,;
            Phone     WITH ALLTRIM(FMR.Phone1),;
            Fax       WITH ALLTRIM(FMR.FAX1),;
            cContTtl  WITH FMR.Title&lcRec,;
            cAdd_User WITH 'BMS',;
            cAdd_Time WITH TIME(),;
            dAdd_Date WITH gdSysDate
    REPLACE cContType WITH 'C',;
            cCont_Id  WITH Customer.Account,;
            Contact   WITH FMR.Name&lcRec,;
            Phone     WITH STRTRAN(FMR.Phone1, ' ' , '' ),;
            Fax       WITH STRTRAN(FMR.FAX1, ' ', '' ),;
            cContTtl  WITH FMR.Title&lcRec,;
            cAdd_User WITH 'BMS',;
            cAdd_Time WITH TIME(),;
            dAdd_Date WITH gdSysDate
    *B603466,1 (End)
  ENDIF           
ENDFOR
        
*!*************************************************************
*! Name      : lfAddNotes
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 01/25/2000
*! Purpose   : Add notes for the customer.
*!*************************************************************
FUNCTION lfAddNotes

SELECT FMR
IF SEEK(STR(MemoNote,8),'FMR_B')
  SELECT FMR_B
  INSERT INTO NOTEPAD (TYPE,KEY,CDESC,mNotes);
              VALUES  ('A',CUSTOMER.Account,'Notes For Account :'+CUSTOMER.Account,'BMS '+DTOC(gdSysDate)+' '+TIME()+CHR(13))
  SCAN WHILE Memo_key = STR(FMR.MemoNote,8)
    SELECT NotePad
    REPLACE mNotes WITH ' '+ALLTRIM(FMR_B.MEMO_LINE) ADDITIVE
  ENDSCAN
  SELECT CUSTOMER
  REPLACE LHASNOTES WITH .T.
ENDIF

*!*************************************************************
*! Name      : lfClosFile
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 01/24/2000
*! Purpose   : Close files used with 'USE..' comamnd ather than 'gfOpenFile' function..
*!*************************************************************
FUNCTION lfClosFile

IF USED('FMR')
  USE IN FMR
ENDIF
IF USED('FMR_B')
  USE IN FMR_B
ENDIF
