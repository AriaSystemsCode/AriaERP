*:************************************************************************
*: Program file  : GFAUDTRL.PRG
*: Program desc. : Program to add new records in the Audit Trail file, this
*:                 program is for the Audit Trail system which will track
*:                 the actions (events) that was executed in a certain
*:                 transaction.
*:
*:         System: Aria advantage series (V2.7)
*:         Module: Main system
*:           Date: 07/22/99
*:      Developer: HS (Haytham El_Sheltawi)
*:************************************************************************
*: Calls : 
*:         Procedures :
*:         Functions  : gfOpenFile()
*:                      gfGetTime()
*:************************************************************************
*: Passed Parameters  : 1) lcObjectID - Audit Trail Object ID, Object ID
*:                         of the screen that will display this Audit Trail
*:                         information, for example Sales Order screen
*:                         Object ID.
*:                      2) lcKey - Audit Trail Key, Key to be used to track
*:                         the Audit trail of certain Type.
*:                         Note: The Audit Trail uses the same key used
*:                               for the notes.
*:                      3) lcEvnObjID - Event Object ID, Object ID of the
*:                         screen from which this event happened.
*:                      4) lcEventID - Event ID, The ID of the Event (from
*:                         the System Events file) that added this Audit
*:                         Trail Record.
*:                      5) lcInform - Needed information, Any Information
*:                         Needed with this Audit Trail record In another
*:                         word any information the user wants to keep
*:                         track of with this event.
*:************************************************************************
*E301298,1 this program was added by HS for the entry E301298,1.
*! N039625,1 MAH 08/24/2005 Enhance Production Schadule Process.
*! E302741,1 WAM 08/25/2010 Convert Audit Trail file into SQL [T20100819.0020]
*:************************************************************************
*
*! N039625,1 MAH 08/24/2005 [BEGIN]
*-- PARAMETERS loFormSet , lcObjectID , lcKey , lcEvnObjID , lcEventID , lcInform
PARAMETERS loFormSet , lcObjectID , lcKey , lcEvnObjID , lcEventID , lcInform, lcUserComments

lcUserComments = IIF(TYPE('lcUserComments') # 'C' .OR. EMPTY(lcUserComments) .OR. ISNULL(lcUserComments) , '',  lcUserComments)
*! N039625,1 MAH 08/24/2005 [END]

PRIVATE lnSelect , lcAudTrlID

*-- Save the old alias
lnSelect = SELECT(0)

*-- Open the Audit Trail file if it was not opened
IF !USED('AUDTRAIL')
  *E302741,1 WAM 08/25/2010 Convert Audit Trail file into SQL 
  *=gfOpenFile(oAriaApplication.DataDir + 'AUDTRAIL' , 'AUDTRAIL' , 'SH')
  =gfOpenTable(oariaapplication.datadir +'AUDTRAIL',oariaapplication.datadir +'AUDTRAIL','SH')
  *E302741,1 WAM 08/25/2010 (End)

ENDIF    && End of IF !USED('AUDTRAIL')

SELECT AUDTRAIL

*-- Get a new sequence number for the new record
lcAudTrlID = gfSequence('cAudTralID')

*-- Add a new record in the Audit Trail file

*! N039625,1 MAH 08/24/2005 [BEGIN]
*-- INSERT;
*--   INTO AUDTRAIL;
*--        (cApObjNam  , key       , cEvntObjID , cEvent_ID , mNeededInf ,;
*--         cAudTralID , cAdd_User , dAdd_Date  , cAdd_Time);
*-- VALUES (lcObjectID , lcKey     , lcEvnObjID , lcEventID , lcInform   ,;
*--         lcAudTrlID , oAriaApplication.User_ID , DATE()     , gfGetTime())
INSERT INTO AUDTRAIL ;
                        (cApObjNam  , key       , cEvntObjID , cEvent_ID , mNeededInf , ;
                         cAudTralID , cAdd_User , dAdd_Date  , cAdd_Time, mUsrComm) ;
                        VALUES (lcObjectID , lcKey     , lcEvnObjID , lcEventID , lcInform   , ;
                                        lcAudTrlID , oAriaApplication.User_ID , DATE()     , gfGetTime(), lcUserComments)
*E302741,1 WAM 08/25/2010 Convert Audit Trail file into SQL 
=gfReplace('')
=gfReplace("mNeededInf WITH lcInform")
=gfReplace("mUsrComm WITH lcUserComments")
gfTableUpdate()
*E302741,1 WAM 08/25/2010 (End)

*! N039625,1 MAH 08/24/2005 [BEGIN]

*-- Restore the old alias
SELECT (lnSelect)
