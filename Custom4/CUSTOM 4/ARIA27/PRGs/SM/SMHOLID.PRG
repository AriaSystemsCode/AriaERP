*:************************************************************************
*:
*: Procedure file: SMHOLID.PRG 
*:                 (Holidays)
*:
*:         System: ARIA ADVANTAGE SYRESE
*:         Module: SYSTEM MANAGER
*:         Author: Reham Aly Alallamy
*:      Copyright (c) 
*:************************************************************************
* Last modified:  /  /
*E300683,4 AHMED 06/08/97 Add prgs directory path to the calling of programs
*E300692,4 Hesham 01/07/1997 
*E300692,4 change the fiscal year , holidays, calender to a data files
*E300692,4 insted of system files
*E300789,1 Hesham 03/04/99
*E300789,1 remove field ccomp_id from files ACCOD,FISHD,FSPRD,FSHLD,CODES
*E300789,1 Change Any seeking in these files

*:************************************************************************
EXTERNAL ARRAY laData,laScrMode ,laTstat

SELECT SYCFSHLD

DECLARE laAppInfo[2,1]
DECLARE laModyRec[1,1]
DECLARE laOldData[ALEN(laBlanks,1),1]

llFrstTim  = .T.
llOnChang  = .F.
llUpdate   = .F.

lnRecno    = 1
lnHoliNo   = 0
lcOld_ID   = ''
lcOldTabl  = lc_TablNam         && Saving the old table name
lc_TablNam = ' '                && Clear the variable so the record history
                                && will show the currunt alias

lcOldDate  = ''
llAddFlg   = .F.

*E300789,1 Hesham (Start)
*IF lcOldHold <> laData[1]+laData[2] .OR. !llUpdHd
IF lcOldHold <> laData[2] .OR. !llUpdHd
  *lcOldHold =laData[1]+laData[2]
  lcOldHold = laData[2]  
*E300789,1 Hesham (End)  
*E300692,4 Hesham (Start)
*  SELECT sycfshld.dFshhdate ;
        FROM (gcsyshome+"sycfshld") ;
        WHERE &lcIndExpHd = laData[1]+laData[2];
        INTO ARRAY laHolDates

*  SELECT *,RECNO() AS 'nRecNo' , "S" AS 'cStatus' ;
        FROM (gcsyshome+"sycfshld"),&lcCursor ;
        INTO DBF (gcWorkDir+lc_TempHd);
        WHERE &lcIndExpHd = laData[1]+laData[2]	;
        DISTINCT
*E300789,1 Hesham (Start)
*  SELECT dFshhdate ;
        FROM (lcDataDir+"fshld") ;
        WHERE &lcIndExpHd = laData[1]+laData[2];
        INTO ARRAY laHolDates

*  SELECT *,RECNO() AS 'nRecNo' , "S" AS 'cStatus' ;
        FROM (lcDataDir+"fshld"),&lcCursor ;
        INTO DBF (gcWorkDir+lc_TempHd);
        WHERE &lcIndExpHd = laData[1]+laData[2]	;
        DISTINCT

  SELECT dFshhdate ;
        FROM (lcDataDir+"fshld") ;
        WHERE &lcIndExpHd = laData[2];
        INTO ARRAY laHolDates

  SELECT *,RECNO() AS 'nRecNo' , "S" AS 'cStatus' ;
        FROM (lcDataDir+"fshld"),&lcCursor ;
        INTO DBF (gcWorkDir+lc_TempHd);
        WHERE &lcIndExpHd = laData[2]	;
        DISTINCT

*E300789,1 Hesham (End)        
*E300692,4 Hesham (End)
  APPEND FROM ARRAY laBlanks FOR RECCOUNT(lc_TempHd) < 5
    
    
  SELECT (lc_TempHd)
  INDEX ON &lcIndExpHd.&lcIndTagHd   && Indexing the temp file so any modification
                                     && will get the right position
ENDIF

SELECT (lc_TempHd)
*E300789,1 Hesham (Start)
*COUNT FOR ccomp_id<> CHR(255) .AND. !DELETED() TO lnHoliNo
COUNT FOR !DELETED() TO lnHoliNo
*E300789,1 Hesham (End)
GO TOP
SCAN 
  SCATTER TO laOldData
  SAVE ALL LIKE laOldData TO MEMO mMisc
ENDSCAN  
GO TOP

*** Default valu for list
lsHoliDay = DTOC(dfshhdate)+'|'+SUBSTR(cfshhdesc,1,35)+'|'+cfshhstat

lnRecNo     = RECNO()

*E300683,4 Call *.SPR from screens directory
* DO SMHOLID.SPR         && Execute the software screen
DO (gcScrDir + gcWinAppl + '\SMHOLID.SPR')
*E300683,4 end   
lc_TablNam = lcOldTabl

SELECT SYCFISHD
 
*!**************************************************************************
*!
*!      Procedure: lfShow
*!
*!**************************************************************************
*
FUNCTION lfShow
EXTERNAL ARRAY laTstat

IF laScrMode[2]
  SHOW GETS DISABLE ONLY
  SHOW GET  lsHoliday ENABLE
  SHOW GET  pbOk   ENABLE
ENDIF

IF lnHoliNo > 0 AND !laScrMode[2]
  SHOW GET pbRem ENABLE
ELSE
  SHOW GET pbRem DISABLE
ENDIF

puStat = IIF(ASCAN(laTStat,ALLTRIM(cfshhstat)) = 0 , ;
                   1 , ASCAN(laTStat,ALLTRIM(cfshhstat)))

*!**************************************************************************
*!
*!      Function: lfwDate
*!
*!**************************************************************************
*
FUNCTION lfwDate

lcOldDate = dFshhdate

*!**************************************************************************
*!
*!      Function: lfvDate
*!
*!**************************************************************************
*
FUNCTION lfvDate

IF EMPTY(dFshhdate)
  RETURN .F.
ELSE
  IF llAddFlg
    IF ASCAN(laHolDates,dFshhdate) = 0
      IF BETWEEN (dFshhdate,laData[6],laData[7])

        llAddFlg = .F.
        =lfvUpdate()                      && Update the change record flag
        IF !EMPTY(laHolDates[1])
          DIMENSION laHolDates [ALEN(laHolDates,1)+1,1]
        ENDIF
        laHolDates [ALEN(laHolDates,1)] = dFshhdate

        lsHoliDay = DTOC(dfshhdate)+'|'+SUBSTR(cfshhdesc,1,35)+'|'+cfshhstat
        SHOW GETS
      ELSE
        =gfModalGen("QRM00080B00000","DIALOG")
        REPLACE dFshhdate WITH {}
        SHOW GET dFshhdate ENABLE
        _CUROBJ = OBJNUM(dFshhdate)
      ENDIF
    ELSE
      =gfModalGen("QRM00079B00000","DIALOG")
      REPLACE dFshhdate WITH {}
      SHOW GET dFshhdate ENABLE
      _CUROBJ = OBJNUM(dFshhdate)
    ENDIF  
  ELSE
    IF lcOldDate <> dFshhdate
      IF ASCAN(laHolDates,dFshhdate) = 0
        IF BETWEEN (dFshhdate,laData[6],laData[7])

          =lfvUpdate()                      && Update the change record flag
          IF ASCAN(laHolDates,lcOldDate) <> 0
            laHolDates[ASCAN(laHolDates,lcOldDate)] = dFshhdate
          ENDIF
          lsHoliDay = DTOC(dfshhdate)+'|'+SUBSTR(cfshhdesc,1,35)+'|'+cfshhstat
          SHOW GETS
        ELSE
          =gfModalGen("QRM00080B00000","DIALOG")
          REPLACE dFshhdate WITH lcOldDate
          SHOW GET dFshhdate ENABLE
          _CUROBJ = OBJNUM(dFshhdate)
        ENDIF
      ELSE
        =gfModalGen("QRM00079B00000","DIALOG")
        REPLACE dFshhdate WITH lcOldDate
        SHOW GET dFshhdate ENABLE
        _CUROBJ = OBJNUM(dFshhdate)
      ENDIF  
    ENDIF
  ENDIF
ENDIF

*!**************************************************************************
*!
*!      Function: lfvDescrip
*!
*!**************************************************************************
*
FUNCTION lfvDescrip

=lfvUpdate()

lsHoliDay = DTOC(dfshhdate)+'|'+SUBSTR(cfshhdesc,1,35)+'|'+cfshhstat

SHOW GETS

*!**************************************************************************
*!
*!      Function: lfvStat
*!
*!**************************************************************************
*
FUNCTION lfvStat

EXTERNAL ARRAY laTstat
REPLACE  cFshhstat WITH laTStat[puStat]

=lfvUpdate()

lsHoliDay = DTOC(dfshhdate)+'|'+SUBSTR(cfshhdesc,1,35)+'|'+cfshhstat

SHOW GET lsHoliday 

*!**************************************************************************
*!
*!      Function: lfwHoliday
*!
*!**************************************************************************
* 
FUNCTION lfwHoliday
*E300789,1 Hesham (Start)
*IF cComp_id = CHR(255)
*  GO TOP
*  lsHoliDay = DTOC(dfshhdate)+'|'+SUBSTR(cfshhdesc,1,35)+'|'+cfshhstat
*ENDIF
*E300789,1 Hesham (eND)

IF llFrstTim                      && When of 1st object is excuted whenever
  llFrstTim = .F.                 && you enter the screen so there is no 
  lnRecNo = RECNO()               && need to show gets again, reset first
                                  && time flag and save the selected record
ELSE  
  *** If you do any modification to the record, save it's No. in the 
  *** laModyRec array ,reset the flag and change the status field
  IF llUpdate 
    llUpdate = .F.
    =lfStorOld()                 
    lcStatus = SUBSTR('MMA',AT(cStatus,'SMA'),1)   && Modify
    REPLACE cStatus WITH lcStatus
    =gfAdd_Info()                 && Add information to the record
  ENDIF

  lnRecNo = RECNO()               && Update the record No 
  SHOW GETS                       && Refresh the objects on the screen
ENDIF  

*!**************************************************************************
*!
*!      Function: lfvAdd
*!
*!**************************************************************************
* 
FUNCTION lfvAdd

EXTERNAL ARRAY laTstat
 
IF SEEK(CHR(255)) 
*E300789,1 Hesham (Start)
*  REPLACE cComp_id   WITH laData[1];
          cFisfyear  WITH laData[2];
          cFshhstat  WITH laTStat[puStat];
          cStatus    WITH 'A'

  REPLACE cFisfyear  WITH laData[2];
          cFshhstat  WITH laTStat[puStat];
          cStatus    WITH 'A'

*E300789,1 Hesham (end)
  GO TOP
ELSE
*E300789,1 Hesham (Start)
  *INSERT INTO (gcWorkDir+lc_TempHd) ;
         (cComp_id,cFisfyear,cFshhstat,cStatus);
	     VALUES (laData[1],laData[2],laTStat[puStat],'A')

  INSERT INTO (gcWorkDir+lc_TempHd) ;
         (cFisfyear,cFshhstat,cStatus);
	     VALUES (laData[2],laTStat[puStat],'A')
*E300789,1 Hesham (End)
  GO RECCOUNT()                     && Go to the newly added record
ENDIF

lnRecNo = RECNO()              && Get the new record no

=gfAdd_Info()
=lfStorOld()                      && Save record no to array
=lfvUpdate()

lnHoliNo  = lnHoliNo  + 1

lsHoliDay = 1

SHOW GETS                         && Empty all the fileds by show gets

llAddFlg = .T.

_CUROBJ = OBJNUM(dFshhdate)

*!**************************************************************************
*!
*!      Function: lfvRem
*!
*!**************************************************************************
* 
FUNCTION lfvRem
 
*** Confirm Removing of the record
IF gfModalGen("QRM00007B00007","ALERT") = 1

  IF ALEN(laHolDates,1) <> 1
    =ADEL(laHolDates,ASCAN(laHolDates,dFshhdate))
    DIMENSION laHolDates [ALEN(laHolDates,1)-1,1]
  ELSE
    laHolDates = {}
  ENDIF

  =lfStorOld()                    && Save record No to array

  *** Save old record data to memo 
  SCATTER TO laOldData
  SAVE ALL LIKE laOldData TO MEMO mMisc
 
  *** Change status flag befor delete
  lcStatus = SUBSTR('DDS',AT(cStatus,'SMA'),1)   && Delete
  REPLACE cStatus WITH lcStatus
  DELETE
  lnHoliNo  = lnHoliNo  -1
  IF lnHoliNo < 5
    APPEND BLANK
    GATHER FROM laBlanks
  ENDIF
  
  *** Check if you have to go to next record or the top one
  SKIP IN (lc_TempHd)
  IF EOF(lc_TempHd)
    GO TOP
  ENDIF    

  lsHoliDay = DTOC(dfshhdate)+'|'+SUBSTR(cfshhdesc,1,35)+'|'+cfshhstat
  =lfvUpdate()
  llUpdate = .F.
  
  SHOW GETS
  _CUROBJ = OBJNUM(lsHoliDay)

ENDIF

*!**************************************************************************
*!
*!      Function: lfvOk
*!
*!**************************************************************************
* 
FUNCTION lfvOk
 
*** Check if you update any field without going to the list to update
*** the record flag
IF llUpdate
  lcStatus = SUBSTR('MMA',AT(cStatus,'SMA'),1)   && Modify
  REPLACE cStatus WITH lcStatus
ENDIF

*** Update the llUpdCalls flag that going to be used in the save program
*** if the flag is .T. indicat that confirm some changes in privias setion

llUpdHd = llUpdHd .OR. llOnChang 

=gfUpdate()

*!**************************************************************************
*!
*!      Function: lfvCancel
*!
*!**************************************************************************
*
FUNCTION lfvCancel

*** Check if you mad at least one chang and you want to cancel the operation
*** and if so go to the rollback function to restor the privias data of the 
*** records
IF llOnChang 
  =lfStorOld()
  =gfRollBack(lc_TempHd,@laModyRec,@laOldData)
ENDIF

*!**************************************************************************
*!
*!      Function: lfvUpdate
*!
*!**************************************************************************
*
FUNCTION lfvUpdate

*** Check if you confirm action on one of the objects and chang the llUpdate
*** flag, and confirm that llOnChang flag still .T.
IF LASTKEY() <> 9
  lnRecNo   = RECNO()
  llOnChang = .T.
  llUpdate  = .T.
ENDIF  

*!**************************************************************************
*!
*!      Function: lfStorOld
*!
*!**************************************************************************
*
FUNCTION lfStorOld

*** Check if this record no have been saved in the laModyRec array
*** and if not enlarge the array and stor the record no in last element

IF ASCAN('laModyRec',lnRecNo) = 0 .AND.;
   lnRecNo <= RECCOUNT()
  IF !EMPTY(laModyRec[1,1])
    DIMENSION laModyRec [ALEN(laModyRec,1)+1,1]
  ENDIF  
  laModyRec[ALEN(laModyRec,1)] =  lnRecNo
ENDIF
