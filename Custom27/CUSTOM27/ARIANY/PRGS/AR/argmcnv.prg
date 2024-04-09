*:**************************************************************************
*: PROGRAM   : ARBMSCN.PRG                
*: MODULE    : Aria Apparel Series.
*: DATE      : 02/01/2000
*: DESC.     : Convert data program. From Goldmine software
*: Developer : WAB - Walid A. Wahab 
*: Refer to  : (101758)
*:**************************************************************************
*: Calls : 
*:         FUNCTION  : lfBrow()
*:                   : lfRefresh()
*:                   : lfvOkBut()
*:					 : lfOpenFile()
*:					 : lfConvert()
*:					 : lfOpenFile()
*:					 : lfConvert()
*:					 : lfCreatNdx()
*:					 : lfGetAccnt()
*:					 : lfGenAccnt()
*:					 : lfDelIndex() 
*:					 : lfPhoneNo()
*:					 : lfRecFound() 
*:					 : lfGetFxData()
*:					 : lfGetVrData()
*:**************************************************************************
* Modification:
*B603467,1 KHM 02/22/2000 Using the upper of both BtName and Contract1.company
*B603467,1                when checking the existance of the customer. 
*B603467,1                Adding the replacement of the Usr_dfnr4
*:**************************************************************************
*---intialias variable 
*-- lcDirect   ---> hold goldmine software directory
*-- lnNoOfCov  ---> hold numbers of recorde converted
*-- lnNoOfNon  ---> hold numbers of recorde not converted
*-- lcContact  ---> hold temp name for temp. index created for file CONTACT
STORE '' TO lcDirect 
STORE 0 TO lnNoOfCov,lnNoOfNon
lcContact = gfTempName()

*-- call screen to get goldmine software directory and start converting
DO (gcScrDir+gcWinAppl+"\ARGMCNV.SPR")
*--call Function to delete temp index file
=lfDelIndex()
RETURN
******************END OF MAIN PROG *****************************************

*!**************************************************************************
*! Func. Name: lfBrow
*! Developer : WAB - Walid A. Wahab
*! Date      : 02/01/2000
*! Purpose   : browse directory and determine goldmine directory
*!**************************************************************************
*! Calls     : GETDIR() 
*!			 : lfRefresh()
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   :  None.
*!**************************************************************************
*! Example   : =lfBrow()
*!**************************************************************************
FUNCTION lfBrow
*--call main function to browse directoryes
lcDirect = GETDIR()
*--enable butt. OK if user select an directory
IF !EMPTY(lcDirect)
  SHOW GET pbOkBut ENABLE
ELSE
  SHOW GET pbOkBut DISABLE
ENDIF
=lfRefresh()

*!**************************************************************************
*! Func. Name: lfvOkBut
*! Developer : WAB - Walid A. Wahab
*! Date      : 02/01/2000
*! Purpose   : when user pressed ok. button to strat convert
*!**************************************************************************
*! Calls     : lfOpenFile() 
*!			 : lfConvert()
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   :  None.
*!**************************************************************************
*! Example   : =lfvOkBut()
*!**************************************************************************
FUNCTION lfvOkBut
*--calling function to Open files.
IF !lfOpenFile()
  RETURN
ENDIF
*--calling function to Start converting
=lfConvert()

*!**************************************************************************
*! Func. Name: lfOpenFile
*! Developer : WAB - Walid A. Wahab
*! Date      : 02/01/2000
*! Purpose   : open goldmine file and aria27 files
*!**************************************************************************
*! Calls     : gfModalGen() 
*!			 : lfCreatNdx()
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   :  .T. ---------> if GMW files is exist
*!              .F. ---------> if GMW files is not exist
*!**************************************************************************
*! Example   : =lfOpenFile()
*!**************************************************************************
FUNCTION lfOpenFile
*--Open convert files.
IF !FILE(lcDirect+'CONTACT1.DBF')
    *** "File 'CONTACT1.DBF' does not exist in the current directory." ***
    *** < Ok > ***
    = gfModalGen('TRM00000B00000',.F.,.F.,.F.,"File 'FCONTACT1.DBF' does not exist in the current directory.")    
    RETURN .F.
ENDIF
IF !FILE(lcDirect+'CONTACT1.DBT')
    *** "File 'CONTACT1.DBT' does not exist in the current directory." ***
    *** < Ok > ***
    = gfModalGen('TRM00000B00000',.F.,.F.,.F.,"File 'FCONTACT1.DBT' does not exist in the current directory.")    
    RETURN .F.
ENDIF
=gfOpenFile(lcDirect+"CONTACT1",'',"SH",'')

*B603467,1 KHM 02/22/2000 (Begin) Opening the temporary file in the work
*B603467,1                directory to hold the duplicated records.
COPY STRUCTURE TO gcWorkDir+'DupGMN'
=gfOpenFile(gcWorkDir+"DupGMN",'',"SH",'')
*B603467,1 KHM 02/22/2000 (End)

=gfOpenFile(gcDataDir+"CONTACT",'',"SH",'')
*--call function to create temp. index 
=lfCreatNdx()
=gfOpenFile(gcDataDir+'NotePad','NotePad','SH')
=gfOpenFile(gcDataDir+"CUSTOMER", "CUSTOMER","SH")
RETURN .T.

*!**************************************************************************
*! Func. Name: lfConvert
*! Developer : WAB - Walid A. Wahab
*! Date      : 02/01/2000
*! Purpose   : main function to convert data from goldmine to aria27
*!**************************************************************************
*! Calls     : lfRecFound() 
*!			 : lfGetFxData()
*!			 : lfGetVrData()
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   :  None.
*!**************************************************************************
*! Example   : =lfConvert()
*!**************************************************************************
FUNCTION lfConvert 
*--select goldmine file that we want to convert from
SELECT Contact1
SCAN
  WAIT WINDOW 'Converting data for : ' + Company  NOWAIT
  *-- Check if the customer in GMW file have not any record in our customer 
  *-- file
  IF lfRecFound()
    *--increase no of record not converted to aria27 file
    lnNoOfNon = lnNoOfNon + 1
    *B603467,1 KHM 02/22/2000 (Begin) In case of its a duplicate record
    *B603467,1                add it to the temporary file DupCust.
    SELECT Contact1
    SCATTER MEMVAR MEMO
    INSERT INTO DupGMN FROM MEMVAR
    *B603467,1 KHM 02/22/2000 (End)
    LOOP
  ENDIF
  SELECT CUSTOMER
  SCATTER MEMVAR MEMO BLANK
  *--get value to fileds do not exist in the Gmw file but we must update them 
  *--in our Customer file for all news records being added to the file.
  =lfGetFxData()

  *-- call function to generate customre code
  m.Account =lfGenAccnt(CONTACT1.COMPANY)
  *-- aria account code must me 5 character 
  *-- so wew can not add account less than 5 character and this case occures
  *-- only if the company name is empty or there is only one charecter in the field
  IF LEN(m.Account) < 5
    *--increase no of record not converted to aria27 file
    lnNoOfNon = lnNoOfNon + 1
    
    *B603467,1 KHM 02/22/2000 (Begin) In case of its a duplicate record
    *B603467,1                add it to the temporary file DupCust.
    SELECT Contact1
    SCATTER MEMVAR MEMO
    INSERT INTO DupGMN FROM MEMVAR
    *B603467,1 KHM 02/22/2000 (End)
    
    LOOP
  ENDIF
  *-- call function to get value from Gmw fields to customer fields
  =lfGetVrData()
  *-- if there are notes in contact1.note .
  *-- GMW system keeps the notepad for each customer record in a memo field 
  *-- in the same file. We will update our standard NotePad file.
  IF !EMPTY(CONTACT1.NOTES)
    INSERT INTO NOTEPAD (TYPE,KEY,CDESC,mNotes);
      VALUES  ('A',m.Account,'Notes For Account :'+m.Account,;
           'GMW '+ DTOC(gdSysDate)+' '+TIME()+CHR(13) +CONTACT1.NOTES)
    M.LHasNotes = .T.
  ENDIF
  *--insert record to aria27 file ( customer )
  INSERT INTO CUSTOMER FROM MEMVAR
  lnNoOfCov = lnNoOfCov + 1
ENDSCAN
*--display sno fo records converted and no of records not converted

*B603467,1 KHM 02/22/2000 (Begin) Changing the message by adding the
*B603467,1 KHM            name of the file that holds the duplicate 
*B603467,1 KHM            records and its path. If there is no duplicate
*B603467,1 KHM            records then erase the file.
*= gfModalGen('TRM00000B00000',.F.,.F.,.F.,"No. of accounts added is  " + ALLTRIM(STR(lnNoOfCov,7)) + " And No. of dublicates not added is " + ALLTRIM(STR(lnNoOfNon,7)))    
= gfModalGen('TRM00000B00000',.F.,.F.,.F.,;
"No. of accounts added is  " + ALLTRIM(STR(lnNoOfCov,7)) +;
" And No. of dublicates not added is " + ALLTRIM(STR(lnNoOfNon,7))+;
'.'+IIF(lnNoOfNon>0,;
"The file name for duplicate records is "+gcWorkDir+'DupGMN.DBF',''))

IF lnNoOfNon = 0
  USE IN DupGMN
  ERASE(gcWorkDir+'DupGMN.DBF')
  IF FILE(gcWorkDir+'DupGMN.FPT')
    ERASE(gcWorkDir+'DupGMN.FPT')
  ENDIF  
ENDIF
*B603467,1 KHM 02/22/2000 (End)

RETURN
*!**************************************************************************
*! Func. Name: lfCreatNdx 
*! Developer : WAB - Walid A. Wahab
*! Date      : 02/01/2000
*! Purpose   : Create temp index for contact
*!**************************************************************************
*! Calls     : None.
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   :  None.
*!**************************************************************************
*! Example   : =lfCreatNdx()
*!**************************************************************************
FUNCTION lfCreatNdx 
*-- If index file is already created 
IF !FILE(gcWorkDir +lcContact+ '.CDX') 
  INDEX ON PHONE TAG (lcContact) OF (gcWorkDir + lcContact + '.CDX')
ELSE
  SET ORDER TO TAG (lcContact) OF (gcWorkDir +  lcContact + '.CDX')
ENDIF

*!**************************************************************************
*! Func. Name: lfGetFxData
*! Developer : WAB - Walid A. Wahab
*! Date      : 02/01/2000
*! Purpose   : update fields do not exist in the GMW file but we must update 
*!             them in our Customer file .
*!**************************************************************************
*! Calls     : None.
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   :  None.
*!**************************************************************************
*! Example   : =lfGetFxData()
*!**************************************************************************
FUNCTION lfGetFxData 
PRIVATE lnAlias
m.Type       = 'M'
m.BillTo     = 'M'
m.Status     = 'P'
m.Link_Code  = 'DEFDEF'
m.SalesRep   = 'JIM'
m.Comm       = 10
m.Priority   = '5'
m.Region     = '000017'
m.Class      = '000016'
m.CtermCode  = '000001'
m.ShipVia    = '000027'
m.cDivision  = 'AAS'
m.Spcinst    = '000028'
m.PriceLvl   = 'A'
m.Prnt_Statm = 'N'
m.cCont_code = 'USA'
m.cCurrCode  = 'USD'
m.cSlsGlLink = 'DEF'
m.CONSOL     = 'N'

RETURN

*!**************************************************************************
*! Func. Name: lfRecFound
*! Developer : WAB - Walid A. Wahab
*! Date      : 02/01/2000
*! Purpose   : Check that we are not creating a duplicate record
*!**************************************************************************
*! Calls     : None.
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   :  .T. ----> record already exist.
*!              .F. ----> record is not found.
*!**************************************************************************
*! Example   : =lfRecFound()
*!**************************************************************************
FUNCTION lfRecFound  
PRIVATE lnAlias,lcOrder,llFound
*-* The duplicate check will be done in the following order:
*--1-Compare CONTACT1->company with Aria27.Customer->Stname. If a match is found, 
*--  then do not add this record to the Aria Customer file
*--2-Compare CONTACT1->Phone1 with Aria27.Customer->Phone1.  If a match is found,
*--  then do not add this record to the Aria Customer file.  
*--3-Compare CONTACT1->Phone with Aria27.Contact->Phone.  If a match is found,
*--  then do not add that record.  
*-*The reason we do this check is that some of our customers in our Customer 
*-*file may have 2 locations.  The main phone number in the Customer file may 
*-*be for a different location. However, it is quite possible that we may have 
*-*the all the numbers in the contact file.
lnAlias = SELECT()
SELECT CUSTOMER
lcOrder = ORDER()
llFound = .F.
SET ORDER TO TAG CustomNm
*--Compare CONTACT1->company with Aria27.Customer->Stname. (1)

*B603467,1 KHM 02/22/2000 (Begin) Changing the Seek command in order
*B603467,1                to be able to check for the upper of both 
*B603467,1                BtName and ConTact1.Company
*IF !SEEK('M'+PADR(ConTact1.Company,30))
SEEK 'M'
LOCATE REST WHILE type+stname = 'M' ;
            FOR UPPER(BTName)=UPPER(PADR(ConTact1.Company,30))
IF !FOUND()
*B603467,1 KHM 02/22/2000 (End)

  SET ORDER TO TAG CustomPh
  *--Compare CONTACT1->Phone1 with Aria27.Customer->Phone1 (2)
  *B603467,1 KHM 02/22/2000 (Begin) Getting the phone o. then in order to
  *B603467,1                check if its empty do not complete the checking.
  lcPhoneNub = lfPhoneNo(ConTact1.Phone1)
  *IF !SEEK('M'+lfPhoneNo(ConTact1.Phone1))
  IF !EMPTY(lcPhoneNub) 
    IF !SEEK('M'+lcPhoneNub)
  *B603467,1 KHM 02/22/2000 (End)
      SELECT ConTact
      *--Compare CONTACT1->Phone with Aria27.Contact->Phone  (3)
     *B603467,1 KHM 02/22/2000 (Begin) Changing the Seek command.
      *IF SEEK(lfPhoneNo(ConTact1.Phone1))    
      IF SEEK(lcPhoneNub)
      *B603467,1 KHM 02/22/2000 (End)
        llFound = .T.
      ENDIF
    ELSE
      llFound = .T.  
    ENDIF
  ENDIF
ELSE
  llFound = .T.
ENDIF
SELECT CUSTOMER
SET ORDER TO TAG (lcOrder)
lnAlias = SELECT()
RETURN llFound

*!**************************************************************************
*! Func. Name: lfPhoneNo
*! Developer : WAB - Walid A. Wahab
*! Date      : 02/01/2000
*! Purpose   : remove the blanks or other characters like "(",")","-" from any
*!             Phone  field.
*!**************************************************************************
*! Calls     : None.
*!**************************************************************************
*! Parameters: lcPhoneNo  ---> variable hold phone no fill with blanks or 
*!                             characters like "(",")","-" 
*!**************************************************************************
*! Returns   :  phone no without blank or characters like "(",")","-" .
*!**************************************************************************
*! Example   : =lfPhoneNo()
*!**************************************************************************
FUNCTION lfPhoneNo  
PARAMETER lcPhoneNo 
PRIVATE lnCount,lcAriaTlNo
lcAriaTlNo = ''
FOR lnCount = 1 TO LEN(lcPhoneNo)
 *--keep only numbers in the filed
 *B603467,1 KHM 02/22/2000 (Begin) Changing the IF command by using the
 *B603467,1                ISDIGIT function.
 *IF SUBSTR(lcPhoneNo,lnCount,1) $ '0123456789'
 IF ISDIGIT(SUBSTR(lcPhoneNo,lnCount,1))
 *B603467,1 KHM 02/22/2000 (End)
   lcAriaTlNo = lcAriaTlNo + SUBSTR(lcPhoneNo,lnCount,1)
 ENDIF
ENDFOR
RETURN ALLTRIM(lcAriaTlNo)

*!**************************************************************************
*! Func. Name: lfDelIndex
*! Developer : WAB - Walid A. Wahab
*! Date      : 02/01/2000
*! Purpose   : delete temp index files
*!**************************************************************************
*! Calls     : None.
*!**************************************************************************
*! Parameters: None.
*!**************************************************************************
*! Returns   :  None.
*!**************************************************************************
*! Example   : =lfDelIndex()
*!**************************************************************************
FUNCTION lfDelIndex 
IF FILE(gcWorkDir +lcContact+ '.CDX') 
  SELECT Contact
  CLOSE INDEX
  ERASE (gcWorkDir +lcContact+ '.CDX')
ENDIF

*!**************************************************************************
*! Func. Name: lfGenAccnt
*! Developer : WAB - Walid A. Wahab
*! Date      : 02/01/2000
*! Purpose   : generate account code
*!**************************************************************************
*! Calls     : lfGetAccnt()
*!**************************************************************************
*! Parameters: lcCompany---> hold account name that we want to create a code for it
*!**************************************************************************
*! Returns   :  code no for account ('ABC01')
*!**************************************************************************
*! Example   : =lfGenAccnt()
*!**************************************************************************
FUNCTION lfGenAccnt 
PARAMETER lcCompany
PRIVATE lcAccount,lnCount,lnAlias,lnTime,lnc
*--We will generate an account Code for each record added in the following :
*--Take first 3 nonblank characters of the name and add a sequential number 
*--starting from 00 .
*--If this account code already exist then go to the next sequential number 
*--for the last 2 characters keeping the same first 3 characters.
*--If this account code still exist then change the last 2 sequential digits 
*--like this: 0A,0B,0C….0Z,1A,1B…1Z….9A…9Z.
*--If this account code still exist then take only the first 2 nonblank characters 
*--of the name and make the last 3 characters to be numeric sequence starting 
*--from 000.

*--lcAccount ---> hold the first part of the account code
*--lncount   ---> counter for increase second part of account code
*--lnAlias   ---> hold current alias()
*--lnTime    ---> counter to reloop the code once with 3 charc and the second 
*--               with 2 charc.
*--lnc       ---> counter for alphabetic increase
lcAlias = SELECT()
SELECT CUSTOMER
*-- this loop for starting the code with 3 character ,and the scond time with 
*-- 2 charcter
FOR lnTime = 1 TO 2 
  *--call function to take the first part (2/3 first nonblank charcacters ) 
  *-- of the name.
  lcAccount = lfGetAccnt(lcCompany,lnTime)
  *--check if first part is 2 char and we start add 00 so we loop to add 000 
  IF LEN(lcAccount) = 2 AND lnTime = 1
    LOOP
  ENDIF
  *--check if the code [ first part (2 or 3 char ) + second part (01 to 99 or 999) ]
  *-- if exist add 1 to the second part of the code
  FOR lnCount = 0 to IIF(lnTime=1,99,999)
    IF !SEEK('M'+lcAccount+PADL(lnCount,IIF(lnTime=1,2,3),'0'))
      RETURN(lcAccount+PADL(lnCount,IIF(lnTime=1,2,3),'0') )
    ENDIF
  ENDFOR
  *--check if the code [First part ( 2 or 3 char ) + second part 
  *-- (1 to 9 or 99 + 'A'->'Z').
  lcAlpha = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
  FOR lnCount = 0 to IIF(lnTime=1,9,99)
    FOR lnC = 1 TO 26
      IF !SEEK('M'+lcAccount+STR(lnCount,IIF(lnTime=1,1,2))+SUBSTR(lcAlpha,lnC,1))
        RETURN (lcAccount+STR(lnCount,IIF(lnTime=1,1,2))+SUBSTR(lcAlpha,lnC,1))
      ENDIF
    ENDFOR
  ENDFOR
ENDFOR

*!**************************************************************************
*! Func. Name: lfGetAccnt
*! Developer : WAB - Walid A. Wahab
*! Date      : 02/01/2000
*! Purpose   : Get First part of account code
*!**************************************************************************
*! Calls     : None.
*!**************************************************************************
*! Parameters: lcName   ---> hold account name that we want to create a code for it
*!             lnNumLen ---> hold the charcter lenght of the first part (2 or 3)
*!**************************************************************************
*! Returns   :  lcNewName - > First part of account code
*!**************************************************************************
*! Example   : =lfGetAccnt()
*!**************************************************************************
FUNCTION lfGetAccnt 
PARAMETER lcName,lnNumLen
*--lcNewName ----> holding the First part of account code
lcNewName = ''
FOR lnCount = 1 TO LEN(lcName)
  *-- only first (2 or 3) non blank character
  IF SUBSTR(lcName,lnCount,1) # ' '
    lcNewName = lcNewName + UPPER(SUBSTR(lcName,lnCount,1))
    IF LEN(lcNewName) = IIF(lnNumLen=1,3,2)
      EXIT
    ENDIF
  ENDIF
ENDFOR
RETURN lcNewName 

*!**************************************************************************
*! Func. Name: lfGetVrData
*! Developer : WAB - Walid A. Wahab
*! Date      : 02/01/2000
*! Purpose   : get value from Gmw fields to memvar fields. and add record 
*!             to contact file
*!**************************************************************************
*! Calls     : lfPhoneNo()
*!**************************************************************************
*! Parameters: None.
*!**************************************************************************
*! Returns   :  None.
*!**************************************************************************
*! Example   : =lfGetVrData()
*!**************************************************************************
FUNCTION lfGetVrData 
  m.Btname     = ALLTRIM(PADL(CONTACT1.COMPANY,30))
  m.Stname     = ALLTRIM(PADL(CONTACT1.COMPANY,30))
  m.cAddress1  = ALLTRIM(PADL(CONTACT1.Address1,30))
  m.cAddress12 = ALLTRIM(PADL(CONTACT1.Address1,30))
  m.cAddress2  = ALLTRIM(PADL(CONTACT1.Address2,30))
  m.cAddress22 = ALLTRIM(PADL(CONTACT1.Address2,30))
  m.cAddress3  = ALLTRIM(PADL(CONTACT1.City,30))
  m.cAddress32 = ALLTRIM(PADL(CONTACT1.City,30))
  m.cAddress4  = ALLTRIM(PADL(CONTACT1.State,30))
  m.cAddress42 = ALLTRIM(PADL(CONTACT1.State,30))
  m.cAddress5  = ALLTRIM(PADL(CONTACT1.Zip,30))
  m.cAddress52 = ALLTRIM(PADL(CONTACT1.Zip,30))
  m.cAddress6  = ALLTRIM(PADL(CONTACT1.Country,30))         
  m.cAddress62 = ALLTRIM(PADL(CONTACT1.Country,30))
  m.Phone1     = ALLTRIM(lfPhoneNo(ConTact1.Phone1)+lfPhoneNo(ConTact1.EXT1))
  m.Fax        = ALLTRIM(lfPhoneNo(ConTact1.Fax)+lfPhoneNo(ConTact1.EXT4))
  *B603467,1 KHM 02/22/2000 (Begin) Add it.
  m.Usr_Dfnd4  = ALLTRIM(Contact1.Source)
  *B603467,1 KHM 02/22/2000 (End)

  *--add record to contact file
  IF !EMPTY(CONTACT1.CONTACT)
    INSERT INTO Contact ;
         (cContType,cCont_Id,Contact,cContTtl,Phone,Fax,cContSalut,;
             cAdd_User,cAdd_Time,dAdd_Date) ;
      VALUES('C',m.Account,PADL(CONTACT1.CONTACT,30),CONTACT1.Title,;
             lfPhoneNo(ConTact1.Phone1)+lfPhoneNo(ConTact1.EXT1),;
             lfPhoneNo(ConTact1.Fax)+lfPhoneNo(ConTact1.EXT4),;
             'Dear ' + ConTact1.Dear,'GMW',TIME(),gdSysDate)
  ENDIF
  *--We do not have Second contact name but we have a phone number.(CONTACT1.Phone2)  
  *--If it is not empty, then create a second record in the contact file 
  *--for this customer.  For the "Contact" field update "No name"  
  IF !EMPTY(CONTACT1.Phone2)
    INSERT INTO Contact ;
      (cContType,cCont_Id,Contact,Phone,cAdd_User,cAdd_Time,dAdd_Date);
      VALUES('C',m.Account,'No Name',lfPhoneNo(ConTact1.Phone2)+;
             lfPhoneNo(ConTact1.EXT2),'GMW',TIME(),gdSysDate)
  ENDIF
  *--We do not have third contact name but we have a phone number.(CONTACT1.Phone3)
  *--If it is not empty, then create a third record in the contact file for this customer.  
  *--For the "Contact" field update "No name"
  IF !EMPTY(CONTACT1.Phone3)
    INSERT INTO Contact ;
      (cContType,cCont_Id,Contact,Phone,cAdd_User,cAdd_Time,dAdd_Date);
      VALUES('C',m.Account,'No Name',lfPhoneNo(ConTact1.Phone3)+;
             lfPhoneNo(ConTact1.EXT3),'GMW',TIME(),gdSysDate)
  ENDIF
  m.cAdd_User = 'GMW'
  m.cAdd_Time = Time()
  m.dAdd_Date = gdsysDate
RETURN  