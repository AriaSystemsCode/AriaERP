*:***************************************************************************
*: Program file  : SMPURGE
*: Program desc. : Purge Databases
*: For screen    : 
*:        System : Aria Advantage Series.
*:        Module : System Manager (SM)
*:     Developer : AHMED MAHER (AMH)
*:***************************************************************************
*: Calls : 
*:     Procedures : lpPurge
*:     Functions  : lfIntials, gfSetup, gfOpGrid
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO SmPurge
*:***************************************************************************
*:E301469,1 AMH 09/28/2000
*:B604743,1 MHM 08/15/2001 Fix the bug of purge Material inventory jurnal not work correct
*:B804119,1 AMH Fix Illegal value 
*:E301816,1 ABD 02-17-2002 add the ability to delete rolls that have zero qty.
*:B606114,1 AMH 06/19/2002 Fix the bug of purge Style inventory journal not work correct.
*:B607311,1 AMH 06/18/2003 Fix the bug of purge Style inventory journal not work correct.
*:B126198,1 ASH 02/09/2005 Fix the bug 'Alias not found' if MF is not installed.
*:B127368,1 EIH 06/26/2005 Fix the bug of not purge closed po's .
*:B127944,1 TMI 06/30/2005 Deny purging these data since it is a master files data not transactions
*:B040155,1 TMI 04/13/2006 Fixing purge problems
*:B607933,1 TMI 01/15/2007 add a condition to be applied to the apdist file ( Ticket # T20061212.0002 )
*:B608059,1 TMI 04/24/2007 purge the Closed Material Paurchase order
*:B608062,1 NNA 04/29/2007 Check the HistDate when Purge The Keyed Off Transactions specially if there is a transaction
*:B608062,1 NNA            done in a date before the Purge date and keyed off in date after the Purge date
*B608086,1 TMI 05/16/2007 when seeking the Ap invoice in APINVHDR seek in APDIST with the key CINVNO+CVENDCODE since 
*                         there are invoices with the same invoice # for different vendors, and so they are removed 
*                          from the purge while they must not
*B608475,1 MHM 03/09/2008 Modify Purge program to purge style inventory for all types
*!***************************************************************************
*
*---------------------------------------------------------------------------------------------*
*----------------------------------------Main Section-----------------------------------------*
*---------------------------------------------------------------------------------------------*
*
*-- Declear Arrays for valid entrys of company option.
DECLARE laRpCmpCod[1,5] , laRpCmpDsc[1,1] , laRpCmpNo[1,1]
STORE SPACE(0) TO laRpCmpCod , laRpCmpDsc , laRpCmpNo

*-- Declear Transaction array that be used in Transaction mover
DECLARE laRpSorTrn[1] , laRpTarTrn[1]
STORE SPACE(0) TO laRpSorTrn , laRpTarTrn

*-- Inatial value for variables of company .
STORE gcAct_Comp TO lcDefCmp , lcOldVAl , lcRpComp
STORE {  /  /    } TO ldDefPrDt , ldRpPrDt

lcFilePath = gcDataDir

*E301816,1 ABD - Add the ability to delete rolls that have zero qty. [Begin]
*-- Add New procedure to the purge call Purge Rolles in case
*DECLARE laTransact[16,9] && Array to hold the all transaction
STORE .F. TO llTrkRolls  && Variable to  keep track Rolles Y/N.
lnAddTrkRl = 0
DECLARE laTransact[17,9] && Array to hold the all transaction
*E301816,1 ABD 

*laTransact[*,1] = Transaction Name
laTransact[1,1]  = 'Invoice Details'
laTransact[2,1]  = 'Keyed Off Transactions'
laTransact[3,1]  = 'Sales Order Details'
laTransact[4,1]  = 'SalesRep Commission'
laTransact[5,1]  = 'Return Authorization'
laTransact[6,1]  = 'Return Details'

*B608475,1 MHM 03/09/2008 Modify Purge program to purge style inventory for all types[Start]
*laTransact[7,1]  = 'Style Inventory Adjustments'
laTransact[7,1]  = 'Style Inventory Journal'
*laTransact[8,1]  = 'Fabric Inventory Adjustments'
laTransact[8,1]  = 'Fabric Inventory Journal'

*B608475,1 MHM [End]

laTransact[9,1]  = 'Cutting Tickets Details'
laTransact[10,1] = 'Style Purchase Order'
laTransact[11,1] = 'Material Purchase Order'
laTransact[12,1] = 'Material Manufacturing Order'
laTransact[13,1] = 'A/P Invoice'
laTransact[14,1] = 'Picking Tickets'
laTransact[15,1] = 'Point of Sale'
laTransact[16,1] = 'Bill of Lading'

*E301816,1 ABD - Add the ability to delete rolls that have zero qty. [Begin]
*-- Add New procedure to the purge call Purge Rolles.
laTransact[17,1] = 'Purge Rolls'
*E301816,1 ABD - [End]

*laTransact[*,2] = Needed Module
laTransact[1,2]  = 'AR'
laTransact[2,2]  = 'AR'
laTransact[3,2]  = 'SO'
laTransact[4,2]  = 'SR'
laTransact[5,2]  = 'RM'
laTransact[6,2]  = 'RM'
laTransact[7,2]  = 'IC'
laTransact[8,2]  = 'MA'
laTransact[9,2]  = 'MF'
laTransact[10,2] = 'PO'
laTransact[11,2] = 'MA'
laTransact[12,2] = 'MA'
laTransact[13,2] = 'AP'
laTransact[14,2] = 'AL'
laTransact[15,2] = 'PS'
laTransact[16,2] = 'EB'

*E301816,1 ABD - Add the ability to delete rolls that have zero qty. [Begin]
laTransact[17,2] = 'MA'
*E301816,1 ABD - [End]


*laTransact[*,3]  = Selected (.T./.F.)

*laTransact[*,4] = Needed Files for Transaction
*Each file name end with ',' and the last file also must end with ','
*Some files repated for seek it with more than one seek expration
*the sorting of files is very important to determained the related files of any file
*the parant file most be before its chaild
laTransact[1,4]  = 'INVHDR,INVLINE,CONSINVH,CONSINVL,ARINSTMH,ARINSTMD,INVCHRG,'+;
                   'STYLE,STYDYE,CUSTOMER,CUSTOMER,DEBIT,UNCMSESS,NOTEPAD,NOTEPAD,'+;
                   'NOTEPAD,BOM,SALESREP,ARCUSHST,STYINVJL,'
laTransact[2,4]  = 'ARHIST,UNCMSESS,CUSTOMER,SALESREP,ARCUSHST,'
laTransact[3,4]  = 'ORDHDR,ORDLINE,ORDCANLN,CUTPICK,CUTPICK,CUTTKTH,CUTTKTL,POSHDR,POSLN,'+;
                   'STYLE,STYDYE,CUSTOMER,CUSTOMER,BOMVAR,UNCMSESS,NOTEPAD,NOTEPAD,NOTEPAD,'+;
                   'BOM,SALESREP,ARCUSHST,'
laTransact[4,4]  = 'REPCOMM,SALESREP,UNCMSESS,'
laTransact[5,4]  = 'RETAUTH,RALINE,STYLE,STYDYE,CUSTOMER,CUSTOMER,BOM,SALESREP,ARCUSHST,'
laTransact[6,4]  = 'RETHDR,RETLINE,STYLE,STYDYE,CUSTOMER,CUSTOMER,CREDIT,UNCMSESS,BOM,'+;
                   'SALESREP,ARCUSHST,STYINVJL,'
laTransact[7,4]  = 'STYINVJL,INVTADJ,UNCMSESS,STYLE,STYDYE,NOTEPAD,BOM,'
laTransact[8,4]  = 'MATINVJL,FINVTADJ,FABRIC,FABDYE,NOTEPAD,ROLLS,'
laTransact[9,4]  = 'CUTTKTH,CUTTKTL,CUTPICK,ORDLINE,ORDHDR,CTKTBOM,BOMLINE,BOMCOST,'+;
                   'STYLE,STYDYE,UNCMSESS,NOTEPAD,NOTEPAD,BOM,MFGOPRHD,MFGOPRDT,STYINVJL,'
laTransact[10,4] = 'POSHDR,POSLN,CUTPICK,ORDLINE,ORDHDR,CTKTBOM,BOMLINE,BOMVAR,BOMCOST,'+;
                   'STYLE,STYDYE,APVENDOR,UNCMSESS,NOTEPAD,NOTEPAD,BOM,STYINVJL,'+;
                   'MFGOPRHD,MFGOPRDT,'
laTransact[11,4] = 'POFHDR,POFLN,FABRIC,FABDYE,APVENDOR,NOTEPAD,NOTEPAD,ROLLS,MATINVJL,'
laTransact[12,4] = 'MMFGORDH,MMFGORDD,CTKTBOM,BOMLINE,BOMCOST,MFGOPRHD,MFGOPRDT,'+;
                   'FABRIC,FABDYE,UNCMSESS,ROLLS,MATINVJL,'
laTransact[13,4] = 'APINVHDR,APVINVDT,APINVTKT,APDIST,APPAYMNT,APVENDOR,'
laTransact[14,4] = 'PIKTKT,PIKLINE,ORDLINE,ORDHDR,ORDLINE,STYLE,STYDYE,UNCMSESS,BOM,'
laTransact[15,4] = 'POSTRAN,UNCMSESS,INVHDR,INVLINE,INVCHRG,ORDHDR,ORDLINE,RETHDR,RETLINE,'+;
                   'ARHIST,CREDIT,DEBIT,STYLE,STYDYE,CUSTOMER,NOTEPAD,NOTEPAD,'+;
                   'BOM,SALESREP,ARCUSHST,STYINVJL,'
laTransact[16,4] = 'BOL_HDR,BOL_LIN,PACK_HDR,PACK_LIN,ASN_SHIP,'

*E301816,1 ABD - Add the ability to delete rolls that have zero qty , Open 
*E301816,1 ABD - Needed files. [Begin]
laTransact[17,4] = 'MATINVJL,FABRIC,FABDYE,ROLLS,'
*E301816,1 ABD - [End]


*laTransact[*,5] = Indexes of Needed Files
*index of each file in laTransact[*,4]
laTransact[1,5]  = 'INVHDR,INVLINE,CONSINVH,CONSINVL,ARINSTMH,ARINSTMD,INVCHRG,'+;
                   'STYLE,STYDYE,CUSTOMER,CUSTOMER,DRTRAN,TRANS,NOTEPAD,NOTEPAD,'+;
                   'NOTEPAD,BOM,SALESREP,ACTHST,MFGOPR,'
laTransact[2,5]  = 'ARHISTHT,TRANS,CUSTOMER,SALESREP,ACTHST,'
laTransact[3,5]  = 'ORDHDR,ORDLINE,ORDCANLN,CUTORD,CUTORD,CUTTKTH,CUTTKTL,POSHDR,POSLN,'+;
                   'STYLE,STYDYE,CUSTOMER,CUSTOMER,BOMVAR,TRANS,NOTEPAD,NOTEPAD,NOTEPAD,'+;
                   'BOM,SALESREP,ACTHST,'
laTransact[4,5]  = 'REPCOMM,SALESREP,TRANS,'
laTransact[5,5]  = 'RETAUTH,RALINE,STYLE,STYDYE,CUSTOMER,CUSTOMER,BOM,SALESREP,ACTHST,'
laTransact[6,5]  = 'RETHDR,RETLINE,STYLE,STYDYE,CUSTOMER,CUSTOMER,CRTRAN,TRANS,BOM,'+;
                   'SALESREP,ACTHST,MFGOPR,'
laTransact[7,5]  = 'STYINVJL,INVTADJ,TRANS,STYLE,STYDYE,NOTEPAD,BOM,'

*B604743,1 MHM 08/15/2001 change index of ROLLS file [Start]
*laTransact[8,5]  = 'MATINVJL,FINVTADJ,FABRIC,FABDYE,NOTEPAD,ROLLITEM,'
laTransact[8,5]  = 'MATINVJL,FINVTADJ,FABRIC,FABDYE,NOTEPAD,SESSION,'
*B604743,1 MHM 08/15/2001 [End]

laTransact[9,5]  = 'CUTTKTH,CUTTKTL,CUTPICK,ORDLINE,ORDHDR,CTKTBOM,MFGOPR,POBOMCLS,'+;
                   'STYLE,STYDYE,TRANS,NOTEPAD,NOTEPAD,BOM,MFGOPRHD,MFGOPRDT,MFGOPR,'
laTransact[10,5] = 'POSHDR,POSLN,CUTPICK,ORDLINE,ORDHDR,CTKTBOM,MFGOPR,BOMVAR,POBOMCLS,STYLE,'+;
                   'STYDYE,VENCODE,TRANS,NOTEPAD,NOTEPAD,BOM,MFGOPR,MFGOPRHD,MFGOPRDT,'
laTransact[11,5] = 'POFHDR,POFLN,FABRIC,FABDYE,VENCODE,NOTEPAD,NOTEPAD,ROLLITEM,MFGOPR,'
laTransact[12,5] = 'MMFGORDH,MMFGORDD,CTKTBOM,MFGOPR,POBOMCLS,MFGOPRHD,MFGOPRDT,'+;
                   'FABRIC,FABDYE,TRANS,ROLLITEM,MFGOPR,'
laTransact[13,5] = 'VENDINV,ORGVINV,LNCONT,INVVEND,TYPMETHDOC,VENCODE,'
laTransact[14,5] = 'PIKTKT,PIKLINE,ORDLINE,ORDHDR,ORDLINE,STYLE,STYDYE,TRANS,BOM,'
laTransact[15,5] = 'POSTRAN,TRANS,INVHDR,INVLINE,INVCHRG,ORDHDR,ORDLINE,RETHDR,RETLINE,'+;
                   'ARHISTT,CRTRAN,DRTRAN,STYLE,STYDYE,CUSTOMER,NOTEPAD,NOTEPAD,'+;
                   'BOM,SALESREP,ACTHST,MFGOPR,'
laTransact[16,5] = 'BOL_HDR,BOL_LIN,PACK_HDR,PACK_LIN,ASN_SHIP,'

*E301816,1 ABD - Add the ability to delete rolls that have zero qty , open files
*E301816,1 ABD - With the Following Index. [Begin]
laTransact[17,5] = 'MTINVSEQ,FABRIC,FABDYE,SESSION,'
*E301816,1 ABD - [End]

*laTransact[*,6] = Status of Needed Files
*Status of needed files determined the opration will done with each file
*A=Append , D=Delete , B=Append and Delete , U=Update , N=Open only
laTransact[1,6]  = 'B,B,B,B,B,B,B,A,A,A,A,N,D,B,B,B,A,A,A,N,'
laTransact[2,6]  = 'B,D,A,A,A,'
laTransact[3,6]  = 'B,B,B,B,B,U,U,U,U,A,A,A,A,B,D,B,B,B,A,A,A,'
laTransact[4,6]  = 'B,A,D,'
laTransact[5,6]  = 'B,B,A,A,A,A,A,A,A,'
laTransact[6,6]  = 'B,B,A,A,A,A,N,D,A,A,A,N,'
laTransact[7,6]  = 'B,B,D,A,A,A,A,'
laTransact[8,6]  = 'B,B,A,A,A,A,'
laTransact[9,6]  = 'B,B,B,U,U,B,B,B,A,A,D,B,A,A,B,B,N,'
laTransact[10,6] = 'B,B,B,U,U,B,B,B,B,A,A,A,D,B,A,A,N,B,B,'
laTransact[11,6] = 'B,B,A,A,A,B,A,A,N,'
laTransact[12,6] = 'B,B,B,B,B,B,B,A,A,D,A,N,'
laTransact[13,6] = 'B,B,B,B,B,A,'
laTransact[14,6] = 'B,B,U,A,A,A,A,D,A,'
laTransact[15,6] = 'B,D,B,B,B,B,B,B,B,B,N,N,A,A,A,A,A,A,A,A,N,'
laTransact[16,6] = 'B,B,B,B,B,'

*E301816,1 ABD - Add the ability to delete rolls that have zero qty. [Begin]
laTransact[17,6] = 'B,B,B,B,'
*E301816,1 ABD - [End]

*laTransact[*,7] = Related Files
*for each file in laTransact[*,4] determaind the number of related file by its sort in 
*laTransact[*,4] , the end of each number determined by '-' and the end of each file
*determined by ','
*for each chaild file we determained its parant file number
*if the file is not chaild of any file we tack his number as related file
*ex1. (the file number 1 not chaild of any file and file number 2 is chaild of 1 = '1-,1-,')
*ex2. (the file number 1 not chaild of any file and file number 2 also = '1-,2-,')
*ex3. (the file number 1 not chaild of any file and file number 2 is chaild of 1
*      and file number 3 is chaild of 1 and 2 = '1-,1-,1-2-,')
laTransact[1,7]  = '1-,1-,3-,3-,1-,1-,1-,2-4-,2-4-,1-2-3-4-,1-2-3-4-,12-,13-,1-,8-,10-11-,8-,'+;
                   '10-10-,10-,1-3-,'
laTransact[2,7]  = '1-,2-,1-1-,3-3-,3-,'
laTransact[3,7]  = '1-,1-,1-,1-,1-,4-,4-,5-,5-,2-,2-,1-,1-,1-,15-,1-,10-,12-13-,10-,1-1-12-12-,'+;
                   '12-,'
laTransact[4,7]  = '1-,1-,3-,'
laTransact[5,7]  = '1-,1-,2-,2-,1-,1-,3-,5-5-,5-,'
laTransact[6,7]  = '1-,1-,2-,2-,1-,1-,7-,8-,3-,5-5-,5-,1-,'
laTransact[7,7]  = '1-,2-,3-,1-,1-,4-,4-,'
laTransact[8,7]  = '1-,2-,1-,1-,3-,3-,'
laTransact[9,7]  = '1-,1-,1-,3-,3-,1-,1-,1-,2-,2-,11-,1-,9-,9-,1-,1-,1-,'
laTransact[10,7] = '1-,1-,1-,3-,3-,1-,1-,1-,1-,2-,2-,1-,13-,1-,10-,10-,1-,1-,1-,'
laTransact[11,7] = '1-,1-,2-,2-,1-,1-,3-,3-,1-,'
laTransact[12,7] = '1-,1-,1-,1-,1-,1-,1-,2-,2-,10-,8-,1-,'
laTransact[13,7] = '1-,1-,1-,1-,5-,1-,'
laTransact[14,7] = '1-,1-,2-,1-,2-,2-,2-,8-,6-,'
laTransact[15,7] = '1-,2-,1-,3-,3-,3-,6-,1-,8-,1-,11-,12-,1-,1-,1-,13-,15-,13-,15-,15-,3-8-,'
laTransact[16,7] = '1-,1-,3-,3-,5-,'


*E301816,1 ABD - Add the ability to delete rolls that have zero qty. [Begin]
laTransact[17,7] = '1-,1-,1-,2-,'
*E301816,1 ABD - [End]

*laTransact[*,8] = Seek Expration for relation
*for each chaild file determain the seek expration for relation with parant
*as in laTransact[*,7] if the file is not chaild of any file we tack its number
laTransact[1,8]  = '1-,INVOICE-,3-,INVOICE-,INVOICE-,INVOICE-,INVOICE-,STYLE-STYLE-,STYLE-STYLE-,'+;
                   '"M"+ACCOUNT-"M"+ACCOUNT-"M"+ACCOUNT-"M"+ACCOUNT-,'+;
                   '"S"+ACCOUNT+STORE-"S"+ACCOUNT+STORE-"S"+ACCOUNT+STORE-"S"+ACCOUNT+STORE-,12-,13-,'+;
                   '"C"+INVOICE-,"F"+STYLE-,"A"+ACCOUNT-"A"+ACCOUNT-,CSTYMAJOR-,SALESREP-REP2-,ACCOUNT-,'+;
                   'INVOICE-INVOICE-,'
laTransact[2,8]  = '1-,2-,"M"+ACCOUNT-"S"+ACCOUNT-,SALESREP-REP2-,ACCOUNT-,'
laTransact[3,8]  = '1-,CORDTYPE+ORDER-,CORDTYPE+ORDER-,"1"+ORDER-,"2"+ORDER-,CTKTNO-,CTKTNO+STYLE-,'+;
                   '"P"+CTKTNO-,"P"+CTKTNO+STYLE-,STYLE-STYLE-,STYLE-STYLE-,"M"+ACCOUNT-,"S"+ACCOUNT+STORE-,'+;
                   '"SO"+ORDER-,15-,"B"+ORDER-,"F"+STYLE-,"A"+ACCOUNT-"A"+ACCOUNT-,CSTYMAJOR-,'+;
                   'REP1-REP2-SALESREP-REP2-,ACCOUNT-,'
laTransact[4,8]  = '1-,REPCODE-,3-,'
laTransact[5,8]  = '1-,RANO-,STYLE-,STYLE-,"M"+ACCOUNT-,"S"+ACCOUNT+STORE-,CSTYMAJOR-,SALESREP-REP2-,ACCOUNT-,'
laTransact[6,8]  = '1-,CRMEMO-,STYLE-,STYLE-,"M"+ACCOUNT-,'+;
                   '"S"+ACCOUNT+STORE-,7-,8-,CSTYMAJOR-,SALESREP-REP2-,ACCOUNT-,CRMEMO-,'
laTransact[7,8]  = '1-,2-,3-,STYLE-,STYLE-,"F"+STYLE-,CSTYMAJOR-,'
laTransact[8,8]  = '1-,2-,CFABRIC+CCOLOR-,CFABRIC+CCOLOR-,"G"+FABRIC-,FABRIC-,'
laTransact[9,8]  = '1-,CUTTKT-,"1"+CUTTKT-,"O"+ORDER+STYLE-,"O"+ORDER-,"M"+CUTTKT-,"M"+CUTTKT-,"M"+CUTTKT-,'+;
                   'STYLE-,STYLE-,11-,"I"+CUTTKT-,"F"+STYLE-,CSTYMAJOR-,"M"+CUTTKT-,"M"+CUTTKT-,CUTTKT-,'
laTransact[10,8] = '1-,CSTYTYPE+PO-,"2"+PO-,"O"+ORDER+STYLE-,"O"+ORDER-,"I"+PO-,"I"+PO-,"AD"+PO-,"I"+PO-,'+;
                   'STYLE-,STYLE-,VENDOR-,13-,"P"+PO-,"F"+STYLE-,CSTYMAJOR-,PO-,"I"+PO-,"I"+PO-,'
laTransact[11,8] = '1-,CMATTYPE+POMAT-,FABRIC+COLOR-,FABRIC+COLOR-,VENDOR-,"M"+POMAT-,"G"+FABRIC-,FABRIC-,POMAT-,'
laTransact[12,8] = '1-,CMFGORDNO-,"T"+CMFGORDNO-,"T"+CMFGORDNO-,"T"+CMFGORDNO-,"T"+CMFGORDNO-,"T"+CMFGORDNO-,'+;
                   'CFABRIC+COLOR-,CFABRIC+COLOR-,10-,FABRIC-,CMFGORDNO-,'
*B608086,1 TMI [Start] when seeking the Ap invoice in APINVHDR seek in APDIST with the key CINVNO+CVENDCODE 
*laTransact[13,8] = '1-,CVENDCODE+CINVNO-,CVENDCODE+CINVNO-,CINVNO-,5-,CVENDCODE-,'
laTransact[13,8] = '1-,CVENDCODE+CINVNO-,CVENDCODE+CINVNO-,CINVNO+CVENDCODE-,5-,CVENDCODE-,'
*B608086,1 TMI [End  ] 

laTransact[14,8] = '1-,PIKTKT-,"O"+ORDER+STYLE-,"O"+ORDER-,"O"+ORDER+STYLE-,STYLE-,STYLE-,8-,CSTYMAJOR-,'
laTransact[15,8] = '1-,2-,TRAN-,INVOICE-,INVOICE-,"O"+ORDER-,CORDTYPE+ORDER-,TRAN-,CRMEMO-,ACCOUNT+TRAN-,'+;
                   '11-,12-,STYLE-,STYLE-,"M"+ACCOUNT-,"F"+STYLE-,"A"+ACCOUNT-,'+;
                   'CSTYMAJOR-,SALESREP-REP2-,ACCOUNT-,INVOICE-CRMEMO-,'
laTransact[16,8] = '1-,BOL_NO-,3-,PACK_NO-,5-,'

*E301816,1 ABD - Add the ability to delete rolls that have zero qty. [Begin]
laTransact[17,8] = '1-,FABRIC+COLOR-,FABRIC+COLOR-,4-,'
*E301816,1 ABD - [End]

*laTransact[*,9] = Scan For Expration
*determain the scan for expration for each file
laTransact[1,9]  = 'INVDATE<=ldRpPrDt .AND. !SEEK("1"+INVOICE~"DEBIT"),.T.,'+;
                   'INVDATE<=ldRpPrDt .AND. !SEEK("1"+INVOICE~"DEBIT"),.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,'+;
                   'DTRANDATE<=ldRpPrDt .AND. (CUTRANTYPE = "IINVOICE" .OR. CUTRANTYPE = "DINVOICE"),'+;
                   '.T.,.T.,.T.,.T.,.T.,.T.,.T.,'

*B608062,1 NNA 04/29/2007 (Begin) Check the HistDate instead of TranDate with the Keyed off Transaction
*laTransact[2,9]  = 'TRANDATE<=ldRpPrDt,DTRANDATE<=ldRpPrDt .AND. CUTRANTYPE = "KEYOFF",.T.,.T.,.T.,'
laTransact[2,9]  = 'HISTDATE<=ldRpPrDt,DTRANDATE<=ldRpPrDt .AND. CUTRANTYPE = "KEYOFF",.T.,.T.,.T.,'
*B608062,1 NNA (END)

laTransact[3,9]  = 'COMPLETE<=ldRpPrDt .AND. STATUS$"CX",.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,'+;
                   'DTRANDATE<=ldRpPrDt .AND. (CUTRANTYPE = "SOORD" .OR. CUTRANTYPE = "SMMASCA"),.T.,.T.,.T.,'+;
                   '.T.,.T.,.T.,'
laTransact[4,9]  = 'DATE<=ldRpPrDt,.T.,'+;
                   'DTRANDATE<=ldRpPrDt .AND. (CUTRANTYPE = "SR_PAY" .OR. CUTRANTYPE = "SR_DR" .OR. CUTRANTYPE = "SR_CR"),'
laTransact[5,9]  = 'RADATE<=ldRpPrDt .AND. STATUS$"CX",.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,'
laTransact[6,9]  = 'CRDATE<=ldRpPrDt .AND. !SEEK("0"+CRMEMO~"CREDIT"),.T.,.T.,.T.,.T.,.T.,.T.,'+;
                   'DTRANDATE<=ldRpPrDt .AND. CUTRANTYPE = "CRDT_MEMO",.T.,.T.,.T.,.T.,'

*B608475,1 MHM 03/09/2008 Modify Purge program to purge style inventory for all types[Start]
*laTransact[7,9]  = 'DTRDATE<=ldRpPrDt .AND. CTRTYPE$"12",DATE<=ldRpPrDt,'+;
                   'DTRANDATE<=ldRpPrDt .AND. (CUTRANTYPE = "INVLOCK_M" .OR. CUTRANTYPE = "INVLOCK_S"),.T.,.T.,.T.,.T.,'
laTransact[7,9]  = 'DTRDATE<=ldRpPrDt ,DATE<=ldRpPrDt,'+;
                   'DTRANDATE<=ldRpPrDt .AND. (CUTRANTYPE = "INVLOCK_M" .OR. CUTRANTYPE = "INVLOCK_S"),.T.,.T.,.T.,.T.,'
*B608475,1 MHM [End]

*B604664 MHM 08/15/2001 include all trantype in purge program[START]
*laTransact[8,9]  = 'DTRANDATE<=ldRpPrDt .AND. CTRANTYPE$"23",DATE<=ldRpPrDt,.T.,.T.,.T.,.T.,'
laTransact[8,9]  = 'DTRANDATE<=ldRpPrDt ,DATE<=ldRpPrDt,.T.,.T.,.T.,.T.,'
*B604743,1 MHM 08/15/2001 [END]

*T20070514.0034 TMI [Start] *- purge only cuttickets with colsed date before the purge date
*laTransact[9,9]  = 'COMPLETE<=ldRpPrDt .AND. STATUS$"SX",.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,'+;
                   'DTRANDATE<=ldRpPrDt .AND. (CUTRANTYPE = "CUTTKT" .OR. CUTRANTYPE = "CTFROMORD" .OR. CUTRANTYPE = "CTFROMPL"),'+;
                   '.T.,.T.,.T.,.T.,.T.,.T.,'
laTransact[9,9]  = '(STATUS="S" .AND. DEDIT_DATE<=ldRpPrDt).OR. (STATUS="X" .AND. COMPLETE<=ldRpPrDt),.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,'+;
                   'DTRANDATE<=ldRpPrDt .AND. (CUTRANTYPE = "CUTTKT" .OR. CUTRANTYPE = "CTFROMORD" .OR. CUTRANTYPE = "CTFROMPL"),'+;
                   '.T.,.T.,.T.,.T.,.T.,.T.,'
*T20070514.0034 TMI [End  ] 

*B127368,1 EIH 06/26/2005 Include status (S) closed po's in criteria [Begin].
*laTransact[10,9] = 'COMPLETE<=ldRpPrDt .AND. STATUS$"CX",.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,'+;
                   'DTRANDATE<=ldRpPrDt .AND. CUTRANTYPE$"STYLE_PO\RECIVING\LC\ADJCFRCV\POFROMORD",'+;
                   '.T.,.T.,.T.,.T.,.T.,.T.,'
laTransact[10,9] = 'COMPLETE<=ldRpPrDt .AND. STATUS$"CXS",.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,'+;
                   'DTRANDATE<=ldRpPrDt .AND. CUTRANTYPE$"STYLE_PO\RECIVING\LC\ADJCFRCV\POFROMORD",'+;
                   '.T.,.T.,.T.,.T.,.T.,.T.,'
*B127368,1 EIH [End].

                   
*B608059,1 TMI [Start] purge the Closed Material Paurchase order
*laTransact[11,9] = 'COMPLETE<=ldRpPrDt .AND. STATUS$"CX",.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,'
laTransact[11,9] = 'COMPLETE<=ldRpPrDt .AND. STATUS$"CXL",.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,'
*B608059,1 TMI [End  ] 
laTransact[12,9] = 'COMPLETE<=ldRpPrDt .AND. STATUS$"SX",.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,'+;
                   'DTRANDATE<=ldRpPrDt .AND. CUTRANTYPE = "MAMFORD",.T.,.T.,'
*B607933,1 TMI [Start] add a condition to be applied to the apdist file
*laTransact[13,9] = 'DINVDATE<=ldRpPrDt .AND. NINVAMNT=NINVPAID+NINVDISTK+NINVADJ,.T.,.T.,.T.,DPAYDATE<=ldRpPrDt,.T.,'
laTransact[13,9] = 'DINVDATE<=ldRpPrDt .AND. NINVAMNT=NINVPAID+NINVDISTK+NINVADJ,.T.,.T.,DAPDTRDAT<=ldRpPrDt,DPAYDATE<=ldRpPrDt,.T.,'
*B607933,1 TMI [End  ] 
laTransact[14,9] = 'DATE<=ldRpPrDt .AND. STATUS$"CX",.T.,.T.,.T.,.T.,.T.,.T.,'+;
                   'DTRANDATE<=ldRpPrDt .AND. CUTRANTYPE$"ALORDAL\ALAUTAL\ALRELPI\PACKLIST",.T.,'
laTransact[15,9] = 'TRANDATE<=ldRpPrDt,DTRANDATE<=ldRpPrDt .AND. CUTRANTYPE = "PNTOSALE",'+;
                   '!SEEK("1"+INVOICE~"DEBIT"),.T.,.T.,.T.,.T.,.T.,!SEEK("0"+CRMEMO~"CREDIT"),'+;
                   '.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,'
laTransact[16,9] = 'SHIP_DATE<=ldRpPrDt .AND. STATUS = "C",.T.,SHIP_DATE<=ldRpPrDt .AND. STATUS = "C",.T.,CANCELLED<=ldRpPrDt,'

*E301816,1 ABD - Add the ability to delete rolls that have zero qty. [Begin]
laTransact[17,9] = '.T.,.T.,DTRANDATE<=ldRpPrDt,.T.,'
*E301816,1 ABD - [End]

IF !gfSetup()
  RETURN
ENDIF

lcPathStat = SET('FULLPATH ')       && Varible to save the SET FULLPATH status
SET FULLPATH ON
=lfIntials()                        && Get Intial Values.
llExpr = gfOpGrid('SMPURGE' , .T.)  && Run selection grid.
*B604743,1 MHM 08/15/2001 variable to check material costing methid [Start]
IF !EMPTY(lcRpComp)
  lcCstMth   = gfGetMemVar('M_MATCSTMT',lcRpComp)
ENDIF
*B604743,1 MHM 08/15/2001 [End]

*B606114,1 AMH Variable to check Style costing method [Start]
IF !EMPTY(lcRpComp)
  lcStyCostM = gfGetMemVar('M_COST_MET',lcRpComp)
ENDIF
*B606114,1 AMH [End]

IF TYPE('llExpr')  <> 'L'
  *-- check if N/A Company.
  IF EMPTY(lcRpComp)
    =gfModalGen("TRM00000B00000","DIALOG","Purge",.F.,"No company selected, Cannot purge.")
  ELSE
    IF EMPTY(laRpTarTrn)
      =gfModalGen("TRM00000B00000","DIALOG","Purge",.F.,"No transactions selected to purge.")
    ELSE      
      DO lpPurge                        && Run purge program.
    ENDIF
  ENDIF
ENDIF

glQuitting = .T.  && Rise quit flag because it's modal screen.

* Reset the FULLPATH setting.
SET FULLPATH &lcPathStat
*--- end of main program.
 
*!*************************************************************
*! Name      : lfIntials
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/02/2000
*! Purpose   : Get Intial Values for Option Grid.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfGetComps, lfGetTrn, lfGetPrDt
*!*************************************************************
*! Called from : Main Program
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : = lfIntials()
*!*************************************************************

FUNCTION lfIntials
*-- Fill Company Array.
=lfGetComps()
*-- check the current company
IF EMPTY(lcDefCmp)
  STORE SPACE(0) TO lcDefCmp , lcOldVal
ELSE
  llHist = GfGetMemVar('LLHIST',lcDefCmp)
  lcHisCmpID = GfGetMemVar('M_COMP_ID',lcDefCmp)
  IF !llHist .AND. EMPTY(lcHisCmpID)
    laRpCmpCod[ASCAN(laRpCmpNo,lcDefCmp)-1,5] = 'N'
    STORE SPACE(0) TO lcDefCmp , lcOldVal
  ELSE
    laRpCmpCod[ASCAN(laRpCmpNo,lcDefCmp)-1,5] = IIF(llHist,'H','A')
  ENDIF
ENDIF

IF !EMPTY(lcDefCmp)
  =lfGetTrn(lcDefCmp)              && get company transaction
  ldDefPrDt = lfGetPrDt(lcDefCmp)  && get purge date
ENDIF
*-- end of lfIntials.

*!*************************************************************
*! Name      : lfGetComps
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/02/2000
*! Purpose   : Get company array
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ----
*!*************************************************************
*! Called from : lfIntials
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : =lfGetComps()
*!*************************************************************

FUNCTION lfGetComps
PRIVATE lnComNo , lnI
*-- Collect all companies
SELECT ccomp_id+" - "+cCom_Name,cCom_dDir,mComp_mdl,mModlSet,"" ;
  FROM SYCCOMP                            ;
  INTO ARRAY laRpCmpCod                   ;
  ORDER BY 1

lnComNo = _TALLY

DECLARE laRpCmpDsc[lnComNo+1,1] , laRpCmpNo[lnComNo+1,1]
laRpCmpDsc[1,1] ='N/A'
laRpCmpNo[1,1] = SPACE(0)
IF lnComNo > 0
  lnI = 1
  FOR lnI = 1 TO lnComNo
    laRpCmpDsc[lnI+1,1] = laRpCmpCod[lnI,1]
    laRpCmpNo[lnI+1,1]  = LEFT(laRpCmpCod[lnI,1],2)
  ENDFOR
ENDIF
*--- end of lfGetComps.

*!*************************************************************
*! Name      : lfChVPrCmp
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/02/2000
*! Purpose   : - Check Valid Purge Company
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : GfGetMemVar, gfModalGen
*!*************************************************************
*! Called from : OG
*!*************************************************************
*! Passed Parameters  : lcCompCod (Company code to check)
*!*************************************************************
*! Returns            : lcCompCod (Company code after check)
*!*************************************************************
*! Example   : = lfChVPrCmp(X)
*!*************************************************************

FUNCTION lfChVPrCmp
PARAMETER lcCompCod
PRIVATE lnCmpNo

IF !EMPTY(lcCompCod) 
  lnCmpNo = ASCAN(laRpCmpNo,lcCompCod)-1
  DO CASE
    CASE laRpCmpCod[lnCmpNo,5] = 'N'     && case not valid company
      *-- Message 
      *** The Selected Company don't have a History Company. Select Another One.***
      *** <  Ok  > ***
      =gfModalGen("TRM00000B00000","DIALOG","companies",.F.,;
                  "The selected company does not have a history company.")
      lcCompCod = lcOldVal
    CASE laRpCmpCod[lnCmpNo,5] =''
      llHist = GfGetMemVar('LLHIST',lcCompCod)
      lcHisCmpID = GfGetMemVar('M_COMP_ID',lcCompCod)
      IF !llHist .AND. EMPTY(lcHisCmpID)
        *-- Message 
        *** The Selected Company don't have a History Company. Select Another One.***
        *** <  Ok  > ***
        =gfModalGen("TRM00000B00000","DIALOG","companies",.F.,;
                    "The selected company does not have a history company.")
        laRpCmpCod[ASCAN(laRpCmpNo,lcCompCod)-1,5] = 'N'                 && not valid company
        lcCompCod = lcOldVal
      ELSE
        laRpCmpCod[ASCAN(laRpCmpNo,lcCompCod)-1,5] = IIF(llHist,'H','A') && H=History, A= Active
      ENDIF
  ENDCASE
ENDIF
RETURN lcCompCod
*--- end of lfChVPrCmp.

*!*************************************************************
*! Name      : lfGetTrn
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/02/2000
*! Purpose   : Get source & target transaction array
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ----
*!*************************************************************
*! Called from : lfIntials, OG
*!*************************************************************
*! Passed Parameters  : lcCompCod (Company code to get its
*!                      source & target transaction array)
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : =lfGetTrn(X)
*!*************************************************************

FUNCTION lfGetTrn
PARAMETER lcCompCod

* Copy Transaction array to source Transaction array.
lnCount = 0
lnCmpCodNo = ASCAN(laRpCmpNo,lcCompCod)-1

*E301816,1 ABD - Add the ability to delete rolls that have zero qty. [Begin]
*E301816,1 ABD - Check if selected company is Keep track lot.
*FOR lnI = 1  TO  16
STORE .F. TO llTrkRolls
llTrkRolls = IIF(gfGetMemVar('M_TrkRolls',lcRpComp)='Y',.T.,.F.)
lnAddTrkRl = IIF(llTrkRolls,1,0)
FOR lnI = 1  TO  16 + lnAddTrkRl
  *E301816,1 ABD - [End]

  IF laTransact[lnI,2]$laRpCmpCod[lnCmpCodNo,3]
    IF SEEK(laTransact[lnI,2],'SYDAPPL')
      IF !SYDAPPL.lSetreq .OR. laTransact[lnI,2]$laRpCmpCod[lnCmpCodNo,4]
        lnCount = lnCount + 1
        DECLARE laRpSorTrn[lnCount] , laRpTarTrn[lnCount]
        laRpSorTrn[lnCount]  =  laTransact[lnI , 1]
        laRpTarTrn[lnCount]  =  laTransact[lnI , 1]
      ENDIF
    ENDIF
  ENDIF
ENDFOR
*--- end of lfGetTrn.

*!*************************************************************
*! Name      : lfGetPrDt
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/02/2000
*! Purpose   : Get Company Purge Date
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ----
*!*************************************************************
*! Called from : lfIntials, Option Grid
*!*************************************************************
*! Passed Parameters  : lcCompCod (Company code to get its 
*!                                 purge date)
*!*************************************************************
*! Returns            : ldGetDate (Purge date)
*!*************************************************************
*! Example   : =lfGetPrDt(X)
*!*************************************************************

FUNCTION lfGetPrDt
PARAMETER lcCompCod
PRIVATE ldGetDate

lnCmpCodNo = ASCAN(laRpCmpNo,lcCompCod)-1
=gfOpenFile(ALLTRIM(laRpCmpCod[lnCmpCodNo,2])+'FISHD','Compfyear','SH')

lnAlias = SELECT(0)
SELECT FISHD
LOCATE FOR CFISYSTAT ='P'
ldGetDate = DFISBGDAT-1
SELECT (lnAlias)

IF USED('FISHD')
  USE IN FISHD
ENDIF
RETURN ldGetDate
*--- end of lfGetPrDt.

*---------------------------------------------------------------------------------------------*
*-------------------------------------Option Grid Section-------------------------------------*
*---------------------------------------------------------------------------------------------*

*!*************************************************************
*! Name      : lfvCompany
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/02/2000
*! Purpose   : - Valid Function for Company
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : OG
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : = lfvCompany()
*!*************************************************************

FUNCTION lfvCompany

*-- Check if the selected company have a history company.
lcRpComp =lfChVPrCmp(lcRpComp)

*-- update transaction arrays and purge date.
IF !(lcRpComp == lcOldVal) .AND. !EMPTY(lcRpComp)
  =lfGetTrn(lcRpComp)
  ldRpPrDt = lfGetPrDt(lcRpComp)
ENDIF
CLEAR READ

*-- update lcOldVal with current company.
lcOldVal = lcRpComp
*-- end of lfvCompany.

*!*************************************************************
*! Name      : lfvTransct
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/02/2000
*! Purpose   : - Call Categories mover function 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfMover
*!*************************************************************
*! Called from : OG
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : = lfvTransct()
*!*************************************************************

FUNCTION lfvTransct
= gfMover(@laRpSorTrn,@laRpTarTrn,'Select Module',.T.,'')  && call mover function.
*-- end of lfvTransct.

*!*************************************************************
*! Name      : lfvPrDt
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/02/2000
*! Purpose   : valid function of purge date
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ----
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : = lfvPrDt()
*!*************************************************************

FUNCTION lfvPrDt

*-- Chech if Purge Date is Empty.
IF EMPTY(ldRpPrDt)
  *-- Message 
  *** Date Can't be Empty ***
  *** <  Ok  > ***
  =gfModalGen("TRM00000B00000","DIALOG","companies",.F.,"Date cannot be empty.")
  _CUROBJ = _CUROBJ
  RETURN
ENDIF

*-- Check Purge date belonging in the Previous Year.
lnCmpCodNo = ASCAN(laRpCmpNo,lcRpComp)-1
=gfOpenFile(ALLTRIM(laRpCmpCod[lnCmpCodNo,2])+'FISHD','Compfyear','SH')

lnAlias = SELECT(0)
SELECT FISHD
LOCATE FOR CFISYSTAT ='P'
IF ldRpPrDt > DFISBGDAT-1
  *-- Message 
  *** Date Can't be greater than or equal the Start Day of Previous Year ***
  *** <  Ok  > ***
  =gfModalGen("TRM00000B00000","DIALOG","companies",.F.,;
              "Date cannot be greater than or equal the Start Date of the Previous Year.")
  _CUROBJ = _CUROBJ
ENDIF
SELECT (lnAlias)

IF USED('FISHD')
  USE IN FISHD
ENDIF
*--- end of lfvPrDt.

*---------------------------------------------------------------------------------------------*
*----------------------------------------Purge Section----------------------------------------*
*---------------------------------------------------------------------------------------------*

*!*************************************************************
*! Name      : lpPurge
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/02/2000
*! Purpose   : Purge DataBase Procedure.
*!*************************************************************
*! Calls     : 
*!             Procedures : lpPrTrn
*!             Functions  : gfGetMemVar, lfGetNdFls, lfAdGlDist,
*!                          lfAddMsFls, lfAddMsFls, lfOpenFile,
*!                          lfCpMsFls, lfPackFile
*!*************************************************************
*! Called from : Main Program
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : DO lpPurge
*!*************************************************************

PROCEDURE lpPurge
PRIVATE lnSubsc, lcACmpDir, lcCmpStat, lcHCmpCod, lcHCmpDir, lnCount, lnI, lnJ
PRIVATE lnFPosFl, lnLPosFl, lnFPosIn, lnLPosIn, lnPosSt, lcFileSt
PRIVATE lnFPosRF, lnLPosRF, lnFPosRD, lnLPosRD, lnFileNo, lcStySeq, lcMatSeq

*-- Fill Transaction array col 3
lnI = 1
*E301816,1 ABD - Add the ability to delete rolls that have zero qty. [Begin]
*FOR lnI = 1 TO 16
FOR lnI = 1 TO 16 + lnAddTrkRl
  *E301816,1 ABD - [End]
  laTransact[lnI,3]=IIF(ASCAN(laRpTarTrn,laTransact[lnI,1]) <> 0 , .T. , .F.)
ENDFOR

*-- Get Active and History compnay information
lnSubsc = ASCAN(laRpCmpNo,lcRpComp)-1
lcACmpDir = ALLTRIM(laRpCmpCod[lnSubsc,2])
lcCmpStat = laRpCmpCod[lnSubsc,5]
llLinkGL  = IIF(gfGetMemVar('M_Link_GL',lcRpComp)='Y',.T.,.F.)
IF lcCmpStat = 'A'
  lcHCmpCod = GfGetMemVar('M_COMP_ID',lcRpComp)
  lnSubsc = ASCAN(laRpCmpNo,lcHCmpCod)-1
  lcHCmpDir = ALLTRIM(laRpCmpCod[lnSubsc,2])
ENDIF

*-- Add GLDIST file to Transaction array.
= lfAdGlDist()

*-- Declare arrays to holed needed files to open for each transaction.
*-- Declare array laNeedFls to needed files with 9 cols.
*-- col 1 = Transaction name
*-- col 2 = File full path and name
*-- col 3 = file index
*-- col 4 = alias
*-- col 5 = company and file status
*-- col 6 = related file number
*-- col 7 = seek expration for the relationship
*-- col 8 = scan for expration 
*-- col 9 = file purged (.T./.F.)
DECLARE laNeedFls[1,9]
lnCount = lfGetNdFls()

*-- Add Master files to needed files array.
= lfAddMsFls(lnCount)

*-- Copy Master Files.
*-- independent files
IF lcCmpStat = 'A'
  = lfCpMsFls()
ENDIF

*-- Purge Selected Transactions.
*-- dependent files
STORE '' TO lcStySeq , lcMatSeq
lnI = 1

*E301816,1 ABD - Add the ability to delete rolls that have zero qty. [Begin]
*FOR lnI = 1 TO 16
FOR lnI = 1 TO  16 + lnAddTrkRl
  *E301816,1 ABD - [End]

  IF laTransact[lnI,3]
    DO lpPrTrn WITH laTransact[lnI,1]
  ENDIF
ENDFOR

*-- Copy Style Inventory Journal.
IF lcCmpStat = 'A'
  IF FILE(lcACmpDir+'styinvjl.dbf') .AND. FILE(lcHCmpDir+'styinvjl.dbf');
     .AND. FILE(lcHCmpDir+'style.dbf')
    IF !EMPTY(lcStySeq)
      =lfPurgStJl(lcStySeq)
    ENDIF
  ENDIF
ENDIF

*-- Copy Fabric Inventory Journal.
IF lcCmpStat = 'A'
  IF FILE(lcACmpDir+'matinvjl.dbf') .AND. FILE(lcHCmpDir+'matinvjl.dbf');
     .AND. FILE(lcHCmpDir+'fabric.dbf')
    IF !EMPTY(lcMatSeq)
      =lfPurgMtJl(lcMatSeq)
    ENDIF
  ENDIF
ENDIF

*-- Run the rebalance routine.
=lfRunRebal(lcRpComp)
IF lcCmpStat = 'A'
  =lfRunRebal(lcHCmpCod)
ENDIF

*B607933,1 TMI [Start] be sure to close the thermometer
=gfThermo(0,0,'Closing .. ',' ')
*B607933,1 TMI [End  ] 

=gfModalGen("TRM00000B00000","DIALOG","Purge",.F.,"Purge is complete.")

*--- end of Purge Program.

*!*************************************************************
*! Name      : lfAdGlDist
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/02/2000
*! Purpose   : Add GLDIST file to transaction array
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ----
*!*************************************************************
*! Called from : lPPurge
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : =lfAdGlDist()
*!*************************************************************

FUNCTION lfAdGlDist
IF llLinkGL
  lnJ = 1
  FOR lnJ = 1 TO 16
    IF INLIST(lnJ,3,4,5,13,14,16)
      LOOP
    ENDIF
    IF laTransact[lnJ,3]
      laTransact[lnJ,4] = laTransact[lnJ,4] + 'GLDIST,'
      laTransact[lnJ,5] = laTransact[lnJ,5] + 'GLDISTNO,'
      laTransact[lnJ,6] = laTransact[lnJ,6] + 'B,'
      laTransact[lnJ,7] = laTransact[lnJ,7] + IIF(INLIST(lnJ,7,8,9,12),'1-1-,','1-,')
      laTransact[lnJ,9] = laTransact[lnJ,9] + '.T.,'
    ENDIF
  ENDFOR
  laTransact[01,8] = laTransact[01,8] + IIF(laTransact[01,3],'INVOICE+"IN"-,','')
  laTransact[02,8] = laTransact[02,8] + IIF(laTransact[02,3],'TRAN+"KO"-,','')
  laTransact[06,8] = laTransact[06,8] + IIF(laTransact[06,3],'CRMEMO+"RM"-,','')
  laTransact[07,8] = laTransact[07,8] + IIF(laTransact[07,3],'CTRCODE+"IA"-CTRCODE+"IP"-,','')
  laTransact[08,8] = laTransact[08,8] + IIF(laTransact[08,3],'CTRAN+"MA"-CTRAN+"MP"-,','')
  laTransact[09,8] = laTransact[09,8] + IIF(laTransact[09,3],'CUTTKT+"CT"-CUTTKT+"JP"-,','')
  laTransact[10,8] = laTransact[10,8] + IIF(laTransact[10,3],'PO+"PO"-,','')
  laTransact[11,8] = laTransact[11,8] + IIF(laTransact[11,3],'POMAT+"MO"-,','')
  laTransact[12,8] = laTransact[12,8] + IIF(laTransact[12,3],'CMFGORDNO+"MM"-CMFGORDNO+"MC"-,','')
ENDIF
*--- end of lfAdGlDist.

*!*************************************************************
*! Name      : lfGetNdFls
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/02/2000
*! Purpose   : Get Needed Files.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfGetRFile
*!*************************************************************
*! Called from : lPPurge
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : lnCount (number of needed files)
*!*************************************************************
*! Example   : =lfGetNdFls()
*!*************************************************************

FUNCTION lfGetNdFls
lnCount = 0        && Counter of needed files
lnI = 1

*E301816,1 ABD - Add the ability to delete rolls that have zero qty. [Begin]
*FOR lnI = 1 TO 16  && loop for all transations
FOR lnI = 1 TO  16 + lnAddTrkRl && loop for all transations
  *E301816,1 ABD - [End]
  
  lnFileNo = 0     && counter of needed files for transation no lnI
  lnNFleNo = 0     && counter of not needed files for transation no lnI
  IF laTransact[lnI,3]      && if selected transactin
    *-- lnFPosFl = frist posation of file name in array
    *-- lnFPosIn = frist posation of index in array
    *-- lnFPosRF = frist posation of related file in array
    *-- lnFPosRD = frist posation of related field in array
    *-- lnFPosEx = frist posation of scan for expration in array
    *-- lnPosSt  = frist posation of file status in array
    *-- lnLPosFl = last posation of file name in array
    *-- lnLPosIn = last posation of index in array
    *-- lnLPosRF = last posation of related file in array
    *-- lnLPosRD = last posation of related field in array
    *-- lnLPosEx = last posation of scan for expration in array
    STORE 0 TO lnFPosFl , lnFPosIn , lnFPosRF , lnFPosRD , lnFPosEx
    lnPosSt = -1
    DO WHILE .T.
      lnLPosFl = AT(',',SUBSTR(laTransact[lnI,4],lnFPosFl+1))
      lnLPosIn = AT(',',SUBSTR(laTransact[lnI,5],lnFPosIn+1))
      lnPosSt = lnPosSt + 2
      lcFileSt = SUBSTR(laTransact[lnI,6],lnPosSt,1)
      lnLPosRF = AT(',',SUBSTR(laTransact[lnI,7],lnFPosRF+1))
      lnLPosRD = AT(',',SUBSTR(laTransact[lnI,8],lnFPosRD+1))
      lnLPosEx = AT(',',SUBSTR(laTransact[lnI,9],lnFPosEx+1))
      *-- lnMax determained the number of times we will need the file
      lnMax = IIF(lcCmpStat = 'A' .AND. lcFileSt $ 'AB', 2 , ;
              IIF(lcCmpStat = 'H' .AND. lcFileSt $ 'AD', 0 , 1))
      IF lnMax > 0
        lnCount = lnCount + 1
        lnFileNo = lnFileNo + 1
        =lfGetRFile(lnI,lnFPosRF,lnFPosRF+lnLPosRF-1,lnFPosRD,lnFPosRD+lnLPosRD-1,lnFileNo,lnCount,lnNFleNo)
      ELSE
        lnFileNo = lnFileNo + 1
        lnNFleNo = lnNFleNo + 1
      ENDIF
      lnJ = 1
      FOR lnJ = 1 TO lnMax
        DECLARE laNeedFls[lnCount,9]
        laNeedFls[lnCount,1] = laTransact[lnI,1]
        laNeedFls[lnCount,2] = IIF(lnJ=1,lcACmpDir,lcHCmpDir);
                             + SUBSTR(laTransact[lnI,4],lnFPosFl+1,lnLPosFl-1)
        laNeedFls[lnCount,3] = SUBSTR(laTransact[lnI,5],lnFPosIn+1,lnLPosIn-1)
        laNeedFls[lnCount,4] = IIF(SUBSTR(laTransact[lnI,4],lnFPosFl+1,lnLPosFl-1)='DEBIT','DEBIT',;
                               IIF(SUBSTR(laTransact[lnI,4],lnFPosFl+1,lnLPosFl-1)='CREDIT','CREDIT',;
                                   gfTempName()))
        laNeedFls[lnCount,5] = lcCmpStat + lcFileSt
        laNeedFls[lnCount,6] = ''
        laNeedFls[lnCount,7] = ''
        laNeedFls[lnCount,8] = STRTRAN(SUBSTR(laTransact[lnI,9],lnFPosEx+1,lnLPosEx-1),'~',',')
        laNeedFls[lnCount,9] = IIF(laNeedFls[lnCount,4]$'DEBITCREDIT',.T.,;
                               IIF(!FILE(laNeedFls[lnCount,2]+'.dbf'),.T.,.F.))
        IF lnJ = 2
          IF laNeedFls[lnCount,9]
            laNeedFls[lnCount-1,9] = .T.
          ELSE
            IF laNeedFls[lnCount-1,9]
              laNeedFls[lnCount,9] = .T.
            ENDIF
          ENDIF
        ENDIF
        IF lnMax = 2 .AND. lnJ = 1
          lnCount = lnCount + 1
          lnFileNo = lnFileNo + 1
        ENDIF
      ENDFOR
      lnFPosFl = lnFPosFl + lnLPosFl
      lnFPosIn = lnFPosIn + lnLPosIn
      lnFPosRF = lnFPosRF + lnLPosRF
      lnFPosRD = lnFPosRD + lnLPosRD
      lnFPosEx = lnFPosEx + lnLPosEx
      IF !(lnFPosFl < LEN(laTransact[lnI,4]))  && if the last file of transaction
        EXIT
      ENDIF                                 && end if the last file of transaction
    ENDDO
  ENDIF                                     && end if selected transaction
ENDFOR                                      && end loop for all transactions
RETURN lnCount
*--- end of lfGetNdFls.

*!*************************************************************
*! Name      : lfGetRFile
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/02/2000
*! Purpose   : Get Related Files.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ----
*!*************************************************************
*! Called from : lfGetNdFls
*!*************************************************************
*! Passed Parameters  : lnTrnId  (Transaction number)
*!                      lnStartR (frist posation of related file)
*!                      lnEndR   (last posation of related file)
*!                      lnStartD (frist posation of related filed)
*!                      lnEndD   (last posation of related file)
*!                      lnFleNo  (file number of this transaction)
*!                      lnLCount (file number of needed files)
*!                      lnNFleNo (number of not needed files)
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : =lfGetRFile()
*!*************************************************************

FUNCTION lfGetRFile
PARAMETER lnTrnId, lnStartR, lnEndR, lnStartD, lnEndD, lnFleNo, lnLCount, lnNFleNo
PRIVATE lnFPosR, lnLPosR, lnFPosD, lnLPosD, lnFrsFlTrn

lnFPosR = lnStartR        && frist posation of related file
lnFPosD = lnStartD        && frist posation of related field
DO WHILE lnFPosR < lnEndR && loop for existing another file
  lnLPosR = AT('-',SUBSTR(laTransact[lnTrnId,7],lnFPosR+1,lnEndR-lnStartR))
  lnLPosD = AT('-',SUBSTR(laTransact[lnTrnId,8],lnFPosD+1,lnEndD-lnStartD))
  lnRFleNo = VAL(SUBSTR(laTransact[lnTrnId,7],lnFPosR+1,lnLPosR-1))-lnNFleNo && file no in trans. array
  *-- file no in needed files array for this transaction
  lnFrsFlTrn = ASCAN(laneedfls,latransact[lnTrnId,1])
  lnRFleTrn = IIF(lnFrsFlTrn=0,lnLCount,ASUBSCRIPT(laNeedFls,lnFrsFlTrn,1))
  lnRCount = 1
  FOR lnK = 1 TO lnRFleNo-1
    lnRCount = lnRCount + IIF(laNeedFls[lnRFleTrn,5]$'AAB' , 2 , 1)
    lnRFleTrn = lnRFleTrn + IIF(laNeedFls[lnRFleTrn,5]$'AAB' , 2 , 1)
  ENDFOR
  IF lnFleNo-lnNFleNo > lnRCount  && if the file is a chaild to another one
    laNeedFls[lnRFleTrn,6] = laNeedFls[lnRFleTrn,6] + ALLTRIM(STR(lnLCount)) + ','
    laNeedFls[lnRFleTrn,7] = laNeedFls[lnRFleTrn,7] + ;
                            SUBSTR(laTransact[lnTrnId,8],lnFPosD+1,lnLPosD-1) + ','
  ENDIF
  lnFPosR = lnFPosR + lnLPosR
  lnFPosD = lnFPosD + lnLPosD
ENDDO
*--- end of lfGetRFile.

*!*************************************************************
*! Name      : lfAddMsFls
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/02/2000
*! Purpose   : Add Master Files to needed files array.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ----
*!*************************************************************
*! Called from : lPPurge
*!*************************************************************
*! Passed Parameters  : lnCount (number of needed files)
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : =lfAddMsFls()
*!*************************************************************

FUNCTION lfAddMsFls
PARAMETER lnCount

IF lcCmpStat = 'A'
  *-- Add Codes file.
  IF FILE(lcACmpDir+'codes.dbf') .AND. FILE(lcHCmpDir+'codes.dbf')
    lnJ = 1
    DECLARE laNeedFls[lnCount+2,9]
    FOR lnJ = 1 TO 2
      laNeedFls[lnCount+lnJ,1] = 'All'
      laNeedFls[lnCount+lnJ,4] = gfTempName()
      laNeedFls[lnCount+lnJ,5] = ''
      laNeedFls[lnCount+lnJ,6] = ''
      laNeedFls[lnCount+lnJ,7] = ''
      laNeedFls[lnCount+lnJ,8] = '.T.'
    ENDFOR

    lnJ = 0
    FOR lnJ = 0 TO 1
      laNeedFls[lnCount+1+(lnJ),2] = IIF(lnJ=0,lcACmpDir,lcHCmpDir) + 'CODES'
      laNeedFls[lnCount+1+(lnJ),3] = 'CODES'
    ENDFOR
    lnCount = lnCount + 2
  ENDIF
  
  *-- Add Scale file.
  IF FILE(lcACmpDir+'scale.dbf') .AND. FILE(lcHCmpDir+'scale.dbf')
    lnJ = 1
    DECLARE laNeedFls[lnCount+2,9]
    FOR lnJ = 1 TO 2
      laNeedFls[lnCount+lnJ,1] = 'All'
      laNeedFls[lnCount+lnJ,4] = gfTempName()
      laNeedFls[lnCount+lnJ,5] = ''
      laNeedFls[lnCount+lnJ,6] = ''
      laNeedFls[lnCount+lnJ,7] = ''
      laNeedFls[lnCount+lnJ,8] = '.T.'
    ENDFOR

    lnJ = 0
    FOR lnJ = 0 TO 1
      laNeedFls[lnCount+1+(lnJ),2] = IIF(lnJ=0,lcACmpDir,lcHCmpDir) + 'SCALE'
      laNeedFls[lnCount+1+(lnJ),3] = 'SCALE'
    ENDFOR
    lnCount = lnCount + 2
  ENDIF
  
  *-- Add Apbanks file.
  IF FILE(lcACmpDir+'apbanks.dbf') .AND. FILE(lcHCmpDir+'apbanks.dbf')
    lnJ = 1
    DECLARE laNeedFls[lnCount+2,9]
    FOR lnJ = 1 TO 2
      laNeedFls[lnCount+lnJ,1] = 'All'
      laNeedFls[lnCount+lnJ,4] = gfTempName()
      laNeedFls[lnCount+lnJ,5] = ''
      laNeedFls[lnCount+lnJ,6] = ''
      laNeedFls[lnCount+lnJ,7] = ''
      laNeedFls[lnCount+lnJ,8] = '.T.'
    ENDFOR

    lnJ = 0
    FOR lnJ = 0 TO 1
      laNeedFls[lnCount+1+(lnJ),2] = IIF(lnJ=0,lcACmpDir,lcHCmpDir) + 'APBANKS'
      laNeedFls[lnCount+1+(lnJ),3] = 'BANKCODE'
    ENDFOR
    lnCount = lnCount + 2
  ENDIF
  
  *-- Add Warehous file.
  IF FILE(lcACmpDir+'warehous.dbf') .AND. FILE(lcHCmpDir+'warehous.dbf')
    lnJ = 1
    DECLARE laNeedFls[lnCount+2,9]
    FOR lnJ = 1 TO 2
      laNeedFls[lnCount+lnJ,1] = 'All'
      laNeedFls[lnCount+lnJ,4] = gfTempName()
      laNeedFls[lnCount+lnJ,5] = ''
      laNeedFls[lnCount+lnJ,6] = ''
      laNeedFls[lnCount+lnJ,7] = ''
      laNeedFls[lnCount+lnJ,8] = '.T.'
    ENDFOR

    lnJ = 0
    FOR lnJ = 0 TO 1
      laNeedFls[lnCount+1+(lnJ),2] = IIF(lnJ=0,lcACmpDir,lcHCmpDir) + 'WAREHOUS'
      laNeedFls[lnCount+1+(lnJ),3] = 'WAREHOUS'
    ENDFOR
    lnCount = lnCount + 2
  ENDIF
  
  *-- Add Whsloc file.
  IF FILE(lcACmpDir+'whsloc.dbf') .AND. FILE(lcHCmpDir+'whsloc.dbf')
    lnJ = 1
    DECLARE laNeedFls[lnCount+2,9]
    FOR lnJ = 1 TO 2
      laNeedFls[lnCount+lnJ,1] = 'All'
      laNeedFls[lnCount+lnJ,4] = gfTempName()
      laNeedFls[lnCount+lnJ,5] = ''
      laNeedFls[lnCount+lnJ,6] = ''
      laNeedFls[lnCount+lnJ,7] = ''
      laNeedFls[lnCount+lnJ,8] = '.T.'
    ENDFOR

    lnJ = 0
    FOR lnJ = 0 TO 1
      laNeedFls[lnCount+1+(lnJ),2] = IIF(lnJ=0,lcACmpDir,lcHCmpDir) + 'WHSLOC'
      laNeedFls[lnCount+1+(lnJ),3] = 'WHSLOC'
    ENDFOR
    lnCount = lnCount + 2
  ENDIF
  
  *-- Add Dye_rel file.
  IF FILE(lcACmpDir+'dye_rel.dbf') .AND. FILE(lcHCmpDir+'dye_rel.dbf')
    lnJ = 1
    DECLARE laNeedFls[lnCount+2,9]
    FOR lnJ = 1 TO 2
      laNeedFls[lnCount+lnJ,1] = 'All'
      laNeedFls[lnCount+lnJ,4] = gfTempName()
      laNeedFls[lnCount+lnJ,5] = ''
      laNeedFls[lnCount+lnJ,6] = ''
      laNeedFls[lnCount+lnJ,7] = ''
      laNeedFls[lnCount+lnJ,8] = '.T.'
    ENDFOR

    lnJ = 0
    FOR lnJ = 0 TO 1
      laNeedFls[lnCount+1+(lnJ),2] = IIF(lnJ=0,lcACmpDir,lcHCmpDir) + 'DYE_REL'
      laNeedFls[lnCount+1+(lnJ),3] = 'DYE_REL'
    ENDFOR
    lnCount = lnCount + 2
  ENDIF
  
  *-- Add Gl_link file.
  IF FILE(lcACmpDir+'gl_link.dbf') .AND. FILE(lcHCmpDir+'gl_link.dbf')
    lnJ = 1
    DECLARE laNeedFls[lnCount+2,9]
    FOR lnJ = 1 TO 2
      laNeedFls[lnCount+lnJ,1] = 'All'
      laNeedFls[lnCount+lnJ,4] = gfTempName()
      laNeedFls[lnCount+lnJ,5] = ''
      laNeedFls[lnCount+lnJ,6] = ''
      laNeedFls[lnCount+lnJ,7] = ''
      laNeedFls[lnCount+lnJ,8] = '.T.'
    ENDFOR

    lnJ = 0
    FOR lnJ = 0 TO 1
      laNeedFls[lnCount+1+(lnJ),2] = IIF(lnJ=0,lcACmpDir,lcHCmpDir) + 'GL_LINK'
      laNeedFls[lnCount+1+(lnJ),3] = 'GL_LINK'
    ENDFOR
    lnCount = lnCount + 2
  ENDIF
  
  *-- Add Factor file.
  IF FILE(lcACmpDir+'factor.dbf') .AND. FILE(lcHCmpDir+'factor.dbf')
    lnJ = 1
    DECLARE laNeedFls[lnCount+2,9]
    FOR lnJ = 1 TO 2
      laNeedFls[lnCount+lnJ,1] = 'All'
      laNeedFls[lnCount+lnJ,4] = gfTempName()
      laNeedFls[lnCount+lnJ,5] = ''
      laNeedFls[lnCount+lnJ,6] = ''
      laNeedFls[lnCount+lnJ,7] = ''
      laNeedFls[lnCount+lnJ,8] = '.T.'
    ENDFOR

    lnJ = 0
    FOR lnJ = 0 TO 1
      laNeedFls[lnCount+1+(lnJ),2] = IIF(lnJ=0,lcACmpDir,lcHCmpDir) + 'FACTOR'
      laNeedFls[lnCount+1+(lnJ),3] = 'FACTOR'
    ENDFOR
    lnCount = lnCount + 2
  ENDIF
  
  *-- Add Glacchar file.
  IF FILE(lcACmpDir+'glacchar.dbf') .AND. FILE(lcHCmpDir+'glacchar.dbf')
    lnJ = 1
    DECLARE laNeedFls[lnCount+2,9]
    FOR lnJ = 1 TO 2
      laNeedFls[lnCount+lnJ,1] = 'All'
      laNeedFls[lnCount+lnJ,4] = gfTempName()
      laNeedFls[lnCount+lnJ,5] = ''
      laNeedFls[lnCount+lnJ,6] = ''
      laNeedFls[lnCount+lnJ,7] = ''
      laNeedFls[lnCount+lnJ,8] = '.T.'
    ENDFOR

    lnJ = 0
    FOR lnJ = 0 TO 1
      laNeedFls[lnCount+1+(lnJ),2] = IIF(lnJ=0,lcACmpDir,lcHCmpDir) + 'GLACCHAR'
      laNeedFls[lnCount+1+(lnJ),3] = 'ACCTCODE'
    ENDFOR
  ENDIF
ENDIF
*--- end of lfAddMsFls.

*!*************************************************************
*! Name      : lfCpMsFls
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/02/2000
*! Purpose   : Copy Master Files to History Company.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ----
*!*************************************************************
*! Called from : lPPurge
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : =lfCpMsFls()
*!*************************************************************

FUNCTION lfCpMsFls
PRIVATE lnAlias, lnFrsFl, lnLstFl, lnI, lcLasKey, lcKey, llCopied

lnAlias = SELECT(0)
lnFrsFl = ASUBSCRIPT(laNeedFls,ASCAN(laNeedFls,'All'),1)  && the frist master file
*-- Open master files.
IF !lfOpenFile('All',lnFrsFl)
  =gfModalGen("TRM00000B00000","DIALOG","Purge",.F.,"Copy master files failed.")
  RETURN
ENDIF
lnLstFl = ALEN(laNeedFls,1)                               && the last master file
lnI = 0
FOR lnI = 0 TO (lnLstFl-lnFrsFl-1) STEP 2   && loop for master files of active company
  SELECT (laNeedFls[lnFrsFl+lnI,4])
  LOCATE
  *-- lcLasKey = key value of preivous record
  *-- lcKey = key value of current record
  *-- llCopied = flag to determined current record copied in history company or not
  lcLasKey = ''
  llCopied = .F.
  SCAN
    lcKey = EVALUATE(KEY())
    IF !(lcKey == lcLasKey)  && if curr key value different to preivous key value 
      llCopied = SEEK(lcKey,laNeedFls[lnFrsFl+lnI+1,4])
    ENDIF
    IF !llCopied  && if the current record not copied to history company
      SCATTER MEMO MEMVAR
      SELECT (laNeedFls[lnFrsFl+lnI+1,4])
      APPEND BLANK
      GATHER MEMO MEMVAR
      SELECT (laNeedFls[lnFrsFl+lnI,4])
    ENDIF
    lcLasKey = lcKey
  ENDSCAN
ENDFOR
SELECT (lnAlias)
*-- Close master files.
=lfClsFile('All',lnFrsFl)
WAIT WINDOW 'Copy Master files Complete' NOWAIT
*--- end of lfCpMsFls.

*!*************************************************************
*! Name      : lpPrTrn
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/02/2000
*! Purpose   : Purge Transactions
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ----
*!*************************************************************
*! Called from : lPPurge
*!*************************************************************
*! Passed Parameters  : lcTrn (transaction name)
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : DO lpPrTrn
*!*************************************************************

PROCEDURE lpPrTrn
PARAMETER lcTrn
PRIVATE lnFrsFl , lnI

lnFrsFl = ASUBSCRIPT(laNeedFls,ASCAN(laNeedFls,lcTrn),1) && frist file of transaction
*-- Open needed files.
IF !lfOpenFile(lcTrn,lnFrsFl)
  =gfModalGen("TRM00000B00000","DIALOG","Purge",.F.,'Transaction '+lcTrn+' failed.')
  RETURN
ENDIF

lnAlias = SELECT(0)

*B606114,1 AMH we start calculate STYINVJL after Purge in case of [Start]
*B606114,1     AVARGE or STANDERD :
*B606114,1     we purge and all remaining record we calculate to get STK's
*B606114,1     LIFO ,FIFO :
*B606114,1     we will delete only all records with balance of STK's
*B606114,1     equal zero that has the same session Number
*B606114,1     we comment this code to purge first and call lfApSyPhIv Function to calculate STK's
*IF lcTrn = 'Style Inventory Adjustments'
*  SELECT (laNeedFls[lnFrsFl,4])
*  *-- append style physical invtory record to reflects the total balance of the purged records.
*  =lfApStPhIv(lnFrsFl)
*ENDIF
*--check for choosing purge "Style Inventory Adjustments" or purge other 

*B608475,1 MHM 03/09/2008 Modify Purge program to purge style inventory for all types[Start]
*llStycheck = IIF(lcTrn = 'Style Inventory Adjustments',lcStyCostM $'AS',.T.)
llStycheck = IIF(lcTrn = 'Style Inventory Journal',lcStyCostM $'AS',.T.)
*B608475,1 MHM [End]

*B606114,1 AMH [End]

*B604743,1 MHM 08/15/2001 we start calculate MATINVJL after Purge in case of  [Start]
*B604743,1                AVARGE or STANDERD :
*B604743,1                we purge and all remaning record we calculate to get ON HAND 
*B604743,1                LIFO ,FIFO,LOT :
*B604743,1                we will delete only all records with balance of ON HAND
*B604743,1                equal zero that has the same session Number
*B604743,1 we comment this code to purge first and call lfApFaPhIv Function to calculate ONHAND
*IF lcTrn = 'Fabric Inventory Adjustments'
*  SELECT (laNeedFls[lnFrsFl,4])
*  *-- append fabric physical invtory record to reflects the total balance of the purged records.
*  =lfApFaPhIv(lnFrsFl)
*ENDIF
*--check for choosing purge "Fabric Inventory Adjustments" or purge other 

*B608475,1 MHM 03/09/2008 Modify Purge program to purge style inventory for all types[Start]
*llcheck = IIF(lcTrn = 'Fabric Inventory Adjustments',lcCstMth $'AS',.T.)
llcheck = IIF(lcTrn = 'Fabric Inventory Journal',lcCstMth $'AS',.T.)
*B608475,1 MHM [END]


*E301816,1 ABD - Add the ability to delete rolls that have zero qty. [Begin]
IF lcTrn = 'Purge Rolls'
  llcheck = .F.
  *-- scan file for purged records
  WAIT WINDOW 'Purge Transaction '+lcTrn NOWAIT
  SELECT (laNeedFls[lnFrsFl,4]) 
  = lfPurgRols (lnFrsFl)
ENDIF
*E301816,1 ABD - [End]

*B040155,1  TMI [Start] Define a pre-purge functions to be run for each set of transactions befor purge process
*                       This includes
*                         1)  Add a summrization line for previouse transactions
*                         2)  for a specific file, create a temp table add in it records that can not be purged
*                             and befor purging any record for that table check from within lfScanFile that this record is
*                             purgable (i.e not found in this temp file)
DO lpPrePurge WITH lcTrn
*B040155,1  TMI [End  ]  

*B606114,1 AMH Don't purge style inventory adjustments with FIFO or LIFO costing methodes [Start]
*IF llcheck
IF llcheck .AND. llStycheck
*B606114,1 AMH [End]

*B604743,1 MHM 08/15/2001 [End]

  lnI = lnFrsFl
  DO WHILE laNeedFls[lnI,1] = lcTrn  && loop for files of transaction
    IF !laNeedFls[lnI,9]  && if the file not purged
      SELECT (laNeedFls[lnI,4])
      LOCATE
      *-- scan file for purged records
      WAIT WINDOW 'Purge Transaction '+lcTrn NOWAIT
      = lfScanFile(lnI,.F.,'')
    ENDIF
    lnI = lnI + 1
    IF lnI > ALEN(laNeedFls,1)
      EXIT
    ENDIF
  ENDDO

*B604743,1 MHM 08/15/2001 we move lfApFaPhIv to calculate after purge program [Start] 
ENDIF  

*B608475,1 MHM 03/09/2008 Modify Purge program to purge style inventory for all types[Start]
*IF lcTrn = 'Fabric Inventory Adjustments'
IF lcTrn = 'Fabric Inventory Journal'
*B608475,1 MHM [End]

  SELECT (laNeedFls[lnFrsFl,4])
  *-- append fabric physical invtory record to reflects the total balance of the purged records.
  =lfApFaPhIv(lnFrsFl)
ENDIF
*B604743,1 MHM 08/15/2001 [End]

*B606114,1 AMH we move lfApSyPhIv to calculate after purge program [Start]

*B608475,1 MHM 03/09/2008 Modify Purge program to purge style inventory for all types[Start]
*IF lcTrn = 'Style Inventory Adjustments'
IF lcTrn = 'Style Inventory Journal'
*B608475,1 MHM [End]

  SELECT (laNeedFls[lnFrsFl,4])
  *-- append style adjustment inventory record to reflects the total balance of the purged records.
  =lfApStPhIv(lnFrsFl)
ENDIF
*B606114,1 AMH [End]

  
SELECT (lnAlias)

*-- Pack needed files.
=lfPackFile(lcTrn,lnFrsFl)
WAIT WINDOW 'Transaction '+lcTrn+' Complete' NOWAIT
*-- end of lpPrTrn.

*!*************************************************************
*! Name      : lfOpenFile
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/02/2000
*! Purpose   : Open Needed Files of transaction
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ----
*!*************************************************************
*! Called from : lPPurge
*!*************************************************************
*! Passed Parameters  : lcTrn (transaction name)
*!                      lnFrsFl (frist file in transactoin)
*!*************************************************************
*! Returns            : .T. (all files opend)/.F. (error occur)
*!*************************************************************
*! Example   : =lfOpenFile()
*!*************************************************************

FUNCTION lfOpenFile
PARAMETER lcTrn, lnFrsFl
PRIVATE lnFleCount, lnI, lnJ

lnFleCount = ALEN(laNeedFls,1)
lcAllFiles = ''
lnI = lnFrsFl
DO WHILE laNeedFls[lnI,1] = lcTrn  && loop for files of transaction
  lnFilePos = RAT('\',laNeedFls[lnI,2]) + 1
  lcFileName = SUBSTR(laNeedFls[lnI,2],lnFilePos)
  lcFileStat = IIF(lcFileName$lcAllFiles,'SH','EX')
  lcAllFiles = lcAllFiles + IIF(lcFileStat='EX',lcFileName+'\','')
  lcAlias = laNeedFls[lnI,4]
  IF FILE(laNeedFls[lnI,2]+'.dbf')
    IF !gfOpenFile(laNeedFls[lnI,2], laNeedFls[lnI,3], lcFileStat , @lcAlias , .T.)
      lnJ = 1
      FOR lnJ = lnFrsFl TO lnI - 1
        =gfCloseFile(laNeedFls[lnI,4])
      ENDFOR
      RETURN .F.
    ENDIF
  ENDIF
  *B127944,1  TMI [Start] set filter on "NOTEPAD" file for types not one of the following 
  *-  {Account Number,Style Number,Item Number,Vendor Number,Salesrep Code,User Name,Template}
  *-  to deny purging notes for these types since it is a master files data not transactions.
  IF 'NOTEPAD'$DBF()
    SET FILTER TO !(TYPE$'AFGHJUT')
    GO TOP  
  ENDIF
  *B127944,1  TMI [End  ] 
  laNeedFls[lnI,4] = lcAlias
  lnI = lnI + 1
  IF lnI > lnFleCount
    EXIT
  ENDIF
ENDDO
RETURN .T.
*--- end of lfOpenFile.

*!*************************************************************
*! Name      : lfClsFile
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/02/2000
*! Purpose   : Close Needed Files of transaction
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ----
*!*************************************************************
*! Called from : lPPurge
*!*************************************************************
*! Passed Parameters  : lcTrn (transaction name)
*!                      lnFrsFl (frist file in transactoin)
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : =lfClsFile()
*!*************************************************************

FUNCTION lfClsFile
PARAMETER lcTrn, lnFrsFl
PRIVATE lnFleCount, lnI

lnFleCount = ALEN(laNeedFls,1)
lnI = lnFrsFl
DO WHILE laNeedFls[lnI,1] = lcTrn  && loop for files of transaction
  =gfCloseFile(laNeedFls[lnI,4])
  lnI = lnI + 1
  IF lnI > lnFleCount
    EXIT
  ENDIF
ENDDO
*--- end of lfClsFile.

*!*************************************************************
*! Name      : lfScanFile
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/02/2000
*! Purpose   : Scan File for Purge records
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ----
*!*************************************************************
*! Called from : lpPrTrn
*!*************************************************************
*! Passed Parameters  : lnPos      (file number)
*!                      llRelated  (file is chaild)
*!                      lcKeyValue (key value of previous rec.)
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : = lfScanFile()
*!*************************************************************

FUNCTION lfScanFile
PARAMETER lnPos , llRelated , lcKeyValue
PRIVATE lnPos, llRelated, lcKeyValue
PRIVATE lcLasKey, lcKey, llCopied
PRIVATE lnFPosR, lnLPosR, lnFPosD, lnLPosD
*-- lcLasKey = key value of previous record
*-- lcKey = key value of current record
*-- llCopied = current record copied to history company
lcLasKey = lcKeyValue
*-- if the file status not (append or both in active company) , llCopied = .F.
*B126198,1 ASH 02/09/2005 (Begin) Fix the bug 'Alias not found' if MF is not installed.
*llCopied = IIF(laNeedFls[lnPos,5]$'AAB',SEEK(lcLasKey,laNeedFls[lnPos+1,4]),.F.)
llCopied = IIF(laNeedFls[lnPos,5]$'AAB' AND !EMPTY(lcLasKey),SEEK(lcLasKey,laNeedFls[lnPos+1,4]),.F.)
*B126198,1 ASH 02/09/2005 (End)
IF RIGHT(laNeedFls[lnPos,2],7) = 'CUTPICK'
  IF laNeedFls[lnPos,3] = 'CUTPICK'
    lnOrdPos = lnPos + IIF(laNeedFls[lnPos,5]='AB',3,2)
    llHasOrd = SEEK("O"+ORDER,laNeedFls[lnOrdPos,4])
    laNeedFls[lnPos,5] = IIF(llHasOrd,IIF(laNeedFls[lnPos,5]='AB','AA','HA'),;
                         IIF(llCopied,'AD',laNeedFls[lnPos,5]))
  ELSE
    lnCktkPos = lnPos + IIF(laNeedFls[lnPos,5]='AB',4,IIF(trancd='1',2,3))
    *B126198,1 ASH 02/09/2005 (Begin) Fix the bug 'Alias not found' if MF is not installed.
    IF !('MF' $ gcCmpModules)
      lnCktkPos = lnCktkPos + 2
    ENDIF
    *B126198,1 ASH 02/09/2005 (End)    
    llHasCktk = SEEK(IIF(trancd='1','','P')+CTKTNO,laNeedFls[lnCktkPos,4])
    laNeedFls[lnPos,5] = IIF(llHasCktk,IIF(laNeedFls[lnPos,5]='AB','AA','HA'),;
                         IIF(llCopied,'AD',laNeedFls[lnPos,5]))
  ENDIF
  llCopied = .F.
ENDIF
SCAN REST FOR EVALUATE(laNeedFls[lnPos,8]) 
  lcKey = EVALUATE(KEY())
  WAIT WINDOW 'Purge file ' + laNeedFls[lnPos,2] + ' rec. ' + lcKey NOWAIT
  *-- if key value of current rec. different key value of previous rec.
  IF !(lcKey == lcLasKey) .AND. IIF(llRelated , !(lcKey = lcLasKey) , .T.)
    IF llRelated
      RETURN
    ENDIF
    llCopied = IIF(laNeedFls[lnPos,5]$'AAB',SEEK(lcKey,laNeedFls[lnPos+1,4]),;
               IIF(laNeedFls[lnPos,5]='HB',.F.,.T.))
  ENDIF
  IF !llCopied   && if record not copied to history company
    IF laNeedFls[lnPos,5]$'AAB'  && if file status is append or both in active company
      SCATTER MEMO MEMVAR
      SELECT (laNeedFls[lnPos+1,4])
      APPEND BLANK
      GATHER MEMO MEMVAR
      *-- Put Zero value for all numeric fields of STYLE , STYDYE , FABRIC , FABDYE
      =lfZeroUpdt(lnPos)
      SELECT (laNeedFls[lnPos,4])
    ENDIF  && end if file status is append or both in active company
    IF laNeedFls[lnPos,5]$'AUHU' && if file status is update
      lnUPos = RAT('\',laNeedFls[lnPos,2]) + 1    && get file name posation
      lcUFile = SUBSTR(laNeedFls[lnPos,2],lnUPos) && get file name
      *-- lcFieldU get fields to update
      lcFieldU = IIF(laNeedFls[lnPos,1] ='Sales Order Details','ORD',;
                 IIF(laNeedFls[lnPos,1] ='Picking Tickets','PIK','CUT'))
      *-- update tot field
      REPLACE TOT&lcFieldU. WITH TOT&lcFieldU. - laUpdate[9]
      *-- update 8 size fields for line files only (CUTTKTL,POSLN,ORDLINE)
      IF lcUFile = 'CUTTKTL' .OR. lcUFile = 'POSLN' .OR. lcUFile = 'ORDLINE'
        FOR lnZ = 1 TO 8
          lcZ = ALLTRIM(STR(lnZ))
          lcFieldUp = lcFieldU + lcZ
          REPLACE &lcFieldUp. WITH &lcFieldUp. - laUpdate[lnZ]
        ENDFOR
      ENDIF
    ENDIF  && end if file status is update
    IF !EMPTY(laNeedFls[lnPos,6]) && if the file has related files
      *-- lnFPosR = frist posatoin of related file number
      *-- lnFPosD = frist posation of seek expration of related file
      *-- lnLPosR = last posation of related file number
      *-- lnLPosD = last Posation of seek expration of related file
      *-- lnRPos = the number or related file
      *-- lcField = seek expration of related file
      *-- laUpdate array to hold the updated qtys
      STORE 0 TO lnFPosR , lnFPosD
      DO WHILE lnFPosR < LEN(laNeedFls[lnPos,6])  && loop for existing file
        lnLPosR = AT(',',SUBSTR(laNeedFls[lnPos,6],lnFPosR+1))
        lnRPos = VAL(SUBSTR(laNeedFls[lnPos,6],lnFPosR+1,lnLPosR-1))
        lnLPosD = AT(',',SUBSTR(laNeedFls[lnPos,7],lnFPosD+1))
        IF laNeedFls[lnRPos,5] <> 'HN'
          lcField = EVALUATE(SUBSTR(laNeedFls[lnPos,7],lnFPosD+1,lnLPosD-1))
          IF SEEK(lcField,laNeedFls[lnRPos,4]) && get the related rec. in related file
            IF laNeedFls[lnRPos,5]$'AUHU'  && if file status is update
              DECLARE laUpdate[9]
              SCATTER FIELDS qty1,qty2,qty3,qty4,qty5,qty6,qty7,qty8,Totqty TO laUpdate
            ENDIF  && end if file status is update
            IF !laNeedFls[lnRpos,9]
              SELECT (laNeedFls[lnRPos,4])
              = lfScanFile(lnRPos,.T.,lcField) && scan related file
            ENDIF
            SELECT (laNeedFls[lnPos,4])
          ENDIF  && end get the related rec. in related file
        ENDIF
        lnFPosR = lnFPosR + lnLPosR
        lnFPosD = lnFPosD + lnLPosD
      ENDDO
    ENDIF  && end if the file has related files
    *-- if the file status is delete or both in active company or both in history company
    IF laNeedFls[lnPos,5]$'ADABHB'
      DELETE
    ENDIF
    *-- if the file status is N in active company
    IF laNeedFls[lnPos,5]$'AN'
      IF laNeedFls[lnPos,1]='Material Purchase Order' .OR. ;
         laNeedFls[lnPos,1]='Material Manufacturing Order'
        lcMatSeq = MAX(ctrn_seq,lcMatSeq)
      ELSE
        lcStySeq = MAX(csession,lcStySeq)
      ENDIF
    ENDIF
  ENDIF  && end if record not copied to history company
  lcLasKey = IIF(llRelated,lcLasKey,lcKey)
ENDSCAN
IF !llRelated
  laNeedFls[lnPos,9] = .T.  && file is purged
  IF laNeedFls[lnPos,5]$'AAB'  && if file status is append or both in active company
    laNeedFls[lnPos+1,9] = .T.  && the file of history company is purged
  ENDIF
  =lfSkipFile(lnPos)
ENDIF
*--- end of lfScanFile.

*!*************************************************************
*! Name      : lfApStPhIv
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/02/2000
*! Purpose   : Append Style Physical Inventory reflects the 
*!             total balance of the purged records.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ----
*!*************************************************************
*! Called from : lpPrTrn
*!*************************************************************
*! Passed Parameters  : lnCurFile (file to append physical rec)
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : = lfApStPhIv()
*!*************************************************************

FUNCTION lfApStPhIv
PARAMETER lnCurFile

*B606114,1 AMH Comment lfApSyPhIv FUNCTION because we change the  way we purge [Start]
*lcGLFYear  = SPACE(04)   && Fiscal Year
*lcGLPeriod = SPACE(02)   && Period
*lcTmpFile = gfTempName() && temp name for dbf file hold adjust qtys
*IF laNeedFls[lnCurFile,5]$'AAB'
*  SELECT cSession,Style,cWareCode,cDyelot,dTrDate,cTrType,cTrCode,;
*         cIRType,cRSession,cISession,cAdjreason,;
*         SUM(nStk1) AS 'nStk1',SUM(nStk2) AS 'nStk2',SUM(nStk3) AS 'nStk3',;
*         SUM(nStk4) AS 'nStk4',SUM(nStk5) AS 'nStk5',SUM(nStk6) AS 'nStk6',;
*         SUM(nStk7) AS 'nStk7',SUM(nStk8) AS 'nStk8',;
*         SUM(nTotStk) AS 'nTotStk' ,SUM(nStkVal) AS 'nStkVal',;
*         SEEK(style+cwarecode+csession;
*         +DTOS(dtrdate)+ctrcode+STR(lineno,6),laNeedFls[lnCurFile+1,4]) AS 'lCopied' ;
*  FROM  (laNeedFls[lnCurFile,4]) ;
*  WHERE DTRDATE <= ldRpPrDt .AND. CTRTYPE$"12";
*  GROUP BY &laNeedFls[lnCurFile,4]..Style,&laNeedFls[lnCurFile,4]..cWareCode,;
*           &laNeedFls[lnCurFile,4]..cDyelot ;
*  HAVING !lCopied ;
*  INTO DBF (gcWorkDir+lcTmpFile)
*ELSE
*  SELECT cSession,Style,cWareCode,cDyelot,dTrDate,cTrType,cTrCode,;
*         cIRType,cRSession,cISession,cAdjreason,;
*         SUM(nStk1) AS 'nStk1',SUM(nStk2) AS 'nStk2',SUM(nStk3) AS 'nStk3',;
*         SUM(nStk4) AS 'nStk4',SUM(nStk5) AS 'nStk5',SUM(nStk6) AS 'nStk6',;
*         SUM(nStk7) AS 'nStk7',SUM(nStk8) AS 'nStk8',;
*         SUM(nTotStk) AS 'nTotStk' ,SUM(nStkVal) AS 'nStkVal' ;
*  FROM  (laNeedFls[lnCurFile,4]) ;
*  WHERE DTRDATE <= ldRpPrDt .AND. CTRTYPE$"12";
*  GROUP BY &laNeedFls[lnCurFile,4]..Style,&laNeedFls[lnCurFile,4]..cWareCode,;
*           &laNeedFls[lnCurFile,4]..cDyelot ;
*  INTO DBF (gcWorkDir+lcTmpFile)
*ENDIF

*lnDirLn = RAT('\',laNeedFls[lnCurFile,2])  && get the length of path of company
*lcDataDir = SUBSTR(laNeedFls[lnCurFile,2],1,lnDirLn)  && get the path of company
**-- open needed files
*=gfOpenFile(lcDataDir+'STYLE','STYLE','SH')
*=gfOpenFile(lcDataDir+'STYDYE','STYDYE','SH')
*IF llLinkGL && if linked with GL
*  =gfOpenFile(lcDataDir+'GLDIST','GLDISTAC','SH')
*  SELECT GLDIST
*  =AFIELDS(laGlData)
*  lcGLDTemp  = gfTempName()
*  CREATE TABLE gcWorkDir+(lcGLDTemp) FROM ARRAY laGlData
*ENDIF
*lcCurCmp = gcAct_Comp  && save the current company code
*lcCurDir = gcDataDir   && save the current data dir
*gcAct_Comp = lcRpComp
*gcDataDir = lcDataDir
*=CHECKPRD(ldRpPrDt+1,'lcGLFYear','lcGLPeriod','IA',.T.)
*gcDataDir = lcCurDir  && restore the current company code
*gcAct_Comp = lcCurCmp && restore the current company code
*SELECT (lcTmpFile)
*LOCATE
*DECLARE laAdjust[9]
*DIMENSION laOldStk[11]
*STORE 0 TO laOldStk
*laOldStk[11] = ldRpPrDt+1
*lnNextFile = lnCurFile + IIF(laNeedFls[lnCurFile,5]$'AAB' , 2 , 1) && get invtadj file number
*SCAN
*  *-- append physical balance in invtadj
*  SELECT (laNeedFls[lnNextFile,4])
*  APPEND BLANK
*  REPLACE Style     WITH &lcTmpFile..Style,;
*          cReason   WITH "Physical balance of Purge",; 
*          Date      WITH ldRpPrDt+1,;
*          DPOSTDate WITH ldRpPrDt+1,;
*          Type      WITH 'P',;
*          cFromWare WITH &lcTmpFile..cWareCode ,; 
*          Unt_Cost  WITH &lcTmpFile..nStkVal/IIF(&lcTmpFile..nTotStk=0,1,&lcTmpFile..nTotStk),;
*          Old_Cost  WITH 0
*
*  FOR lnZ=1 TO 8
*    lcZ = STR(lnZ,1)
*    REPLACE Adj&lcZ    WITH &lcTmpFile..nStk&lcZ.,;
*            OldQty&lcZ WITH laoldStk(lnZ)
*  ENDFOR
*
*  IF llLinkGL
*    lcLinkCode=IIF(SEEK(&lcTmpFile..Style+&lcTmpFile..cWareCode+&lcTmpFile..cDyelot,'STYDYE'),;
*               IIF(!EMPTY(STYDYE.GL_Link),STYDYE.GL_Link,'DEFDEF'),'DEFDEF')
*    REPLACE LINK_CODE WITH lcLinkCode,;
*            GLFYEAR   WITH lcGLFYear,;
*            GLPERIOD  WITH lcGLPeriod
*  ENDIF
*
*  REPLACE TotAdj     WITH &lcTmpFile..nTotStk,;
*          TotOld     WITH laOldStk(9) &&,;
*          dAdd_Date  WITH gdSysdate,;
*          cAdd_Time  WITH TIME(),;
*          cAdd_User  WITH "SMPURGE"
*  *--G/L Array difinition and initialization.
*  IF llLinkGL
*    DECLARE laGLDistAr[2,13]
*    laGLDistAr[1,1] = lcLinkCode
*    laGLDistAr[2,1] = lcLinkCode
*    laGLDistAr[1,2] = '006'
*    laGLDistAr[2,2] = '007'
*    laGLDistAr[1,3] = 1
*    laGLDistAr[2,3] = -1
*    STORE 'IA'       TO laGLDistAr[1,4],laGLDistAr[2,4]
*    STORE ''         TO laGLDistAr[1,5],laGLDistAr[2,5]
*    STORE ldRpPrDt+1 TO laGLDistAr[1,6],laGLDistAr[2,6]
*    STORE lcGLFYear  TO laGLDistAr[1,7],laGLDistAr[2,7]
*    STORE lcGLPeriod TO laGLDistAr[1,8],laGLDistAr[2,8]
*    STORE lcGLDTemp  TO laGLDistAr[1,9],laGLDistAr[2,9]
*    laGLDistAr[2,10] = ''
*  ELSE
*    DIME laGLDistAr[1,1]
*    laGLDistAr = ''
*  ENDIF
*  SELECT (lcTmpFile)
*  SCATTER FIELDS &lcTmpFile..nStk1,&lcTmpFile..nStk2,&lcTmpFile..nStk3,;
*                 &lcTmpFile..nStk4,&lcTmpFile..nStk5,&lcTmpFile..nStk6,;
*                 &lcTmpFile..nStk7,&lcTmpFile..nStk8,&lcTmpFile..nTotStk TO laAdjust
*  lnI = 1
*  FOR lnI = 1 TO 9
*    laAdjust[lnI] = laAdjust[lnI] * 2
*  ENDFOR
*  lcCurCmp = gcAct_Comp  && save the current company code
*  lcCurDir = gcDataDir   && save the current data dir
*  gcAct_Comp = lcRpComp
*  gcDataDir = lcDataDir
*  =gfStyCrl('2',Style,cWareCode,cDyelot,ldRpPrDt+1,'',@laAdjust,;
*            nStkVal/IIF(nTotStk=0,1,nTotStk),'',.T.,'',0,'','',@laGLDistAr,0,'','',@laOldStk)
*  gcDataDir = lcCurDir  && restore the current company code
*  gcAct_Comp = lcCurCmp && restore the current company code
*  SELECT (laNeedFls[lnCurFile,4])
*  lcOrder = SET('ORDER')  && save the order setting of styinvjl
*  SET ORDER TO            && remove order
*  GO BOTTOM               && get the physical last rec.
*  REPLACE cAdjreason WITH &lcTmpFile..cAdjreason
*  lcSession = cSession    && get session number
*  SELECT (laNeedFls[lnNextFile,4])
*  REPLACE cSession WITH lcSession  && save session number in invtadj
*  SELECT (laNeedFls[lnCurFile,4])
*  SET ORDER TO &lcOrder.  && restore the order setting
*ENDSCAN
*
**-- close files
*IF USED(lcTmpFile)
*  USE IN (lcTmpFile)
*ENDIF
*IF llLinkGl .AND. USED(lcGLDTemp)
*  USE IN (lcGLDTemp)
*ENDIF
*IF USED('GLDIST')
*  USE IN GLDIST
*ENDIF
*IF USED('STYDYE')
*  USE IN STYDYE
*ENDIF
*IF USED('STYLE')
*  USE IN STYLE
*ENDIF
**--Erase the temp. file.
*ERASE (gcWorkDir+lcTmpFile+'.DBF')
*IF llLinkGl
*  ERASE (gcWorkDir+lcGLDTemp+'.DBF')
*ENDIF

*--we start calculate STYINVJL after Purge in case of  
*--AVARGE or STANDERD :
*--                   we purge all remaning record we calculate to get STK's
*--LIFO ,FIFO :
*--            we will delete only all records with balance of STK's
*--            equal zero that has the same session Number

*--save link check variable
llStorLink = llLinkGL
llLinkGL = .F.
lcGLFYear  = SPACE(04)  && Fiscal Year
lcGLPeriod = SPACE(02)  && Period
lcTmpFile = gfTempName() && temp name of dbf file to hold adjust qtys
llWareHous = gfGetMemVar('M_WareHouse',lcRpComp)= 'Y'
lnDirLn = RAT('\',laNeedFls[lnCurFile,2])
lcDataDir = SUBSTR(laNeedFls[lnCurFile,2],1,lnDirLn)
=gfOpenFile(lcDataDir+'STYLE','STYLE','SH')
=gfOpenFile(lcDataDir+'STYDYE','STYDYE','SH')
IF llLinkGL
  =gfOpenFile(lcDataDir+'GLDIST','GLDISTAC','SH')
  SELECT GLDIST
  =AFIELDS(laGlData)
  lcGLDTemp  = gfTempName()
  CREATE TABLE gcWorkDir+(lcGLDTemp) FROM ARRAY laGlData
ENDIF
lcCurCmp = gcAct_Comp
lcCurDir = gcDataDir
gcAct_Comp = lcRpComp
gcDataDir = lcDataDir
=CHECKPRD(ldRpPrDt+1,'lcGLFYear','lcGLPeriod','IA',.T.)
gcDataDir = lcCurDir
gcAct_Comp = lcCurCmp

*B040155,1  TMI [Start] Avoid a bug in data that is some lines in stydye have empty warehouse code.
SELECT STYDYE
DELETE FOR EMPTY(CWARECODE)
GO TOP
*B040155,1  TMI [End  ] 

*-in case of Costing AVARAGE OR SATANDERD
IF lcStyCostM $'AS'
  STORE '' TO lcAdReason
  SELECT STYLE
  LOCATE
  lnNextFile = lnCurFile + IIF(laNeedFls[lnCurFile,5]$'AAB' , 2 , 1)
  DECLARE laAdjust[9]
  DIMENSION laOldStk[11]

  SCAN
    STORE 0 TO laOldStk
    laOldStk[11] = ldRpPrDt+1
    =SEEK(STYLE,'STYDYE')
    
    *B607311,1 AMH Scan only the purged styles [Start]
    lcSetDel = SET('DELETE')
    *B040155,3  TMI [Start] loop in stydye file 
    *SET DELETE OFF
    llFound = .F.
    SELECT STYDYE
    SCAN REST WHILE STYLE+CWARECODE+DYELOT = STYLE.STYLE
      SET DELETE OFF
      *B040155,3  TMI [End  ] 
      IF SEEK(STYDYE.STYLE+STYDYE.CWARECODE,laNeedFls[lnCurFile,4])
        SELECT (laNeedFls[lnCurFile,4])
        
        *B608475,1 MHM 03/09/2008 Modify Purge program to purge style inventory for all types[Start]
        *LOCATE REST WHILE STYLE+CWARECODE+CSESSION+DTOS(DTRDATE)+CTRCODE+STR(LINENO,6)=;
        *                  STYDYE.STYLE+STYDYE.CWARECODE;
        *            FOR CDYELOT = STYDYE.DYELOT .AND. DTRDATE<=ldRpPrDt .AND. CTRTYPE$"12"

        LOCATE REST WHILE STYLE+CWARECODE+CSESSION+DTOS(DTRDATE)+CTRCODE+STR(LINENO,6)=;
                          STYDYE.STYLE+STYDYE.CWARECODE;
                    FOR CDYELOT = STYDYE.DYELOT .AND. DTRDATE<=ldRpPrDt 
       *B608475,1 MHM [END]
       
        IF !FOUND()
          SET DELETE &lcSetDel.
          LOOP
          *B040155,3  TMI [Start] if found exit the loop
        ELSE
          llFound = .T.
          EXIT
        *B040155,3  TMI [End  ] 
        ENDIF
        *B040155,3  TMI [Start] move the following line below
      *ELSE
        *SET DELETE &lcSetDel.
        *LOOP
        *B040155,3  TMI [End  ] 
      ENDIF
      SET DELETE &lcSetDel.
      *B607311,1 AMH [End]
  
      *B040155,3  TMI [Start] if not found skip this style
    ENDSCAN
    IF !llFound
      LOOP
    ENDIF
    SET DELETE &lcSetDel.
    *B040155,3  TMI [End  ] 
    
    *B607311,1 AMH Move this part of code to update the INVTADJ file in case update the 
    *B607311,1 AMH STYINVJL file [Start]
    *--append in INVTADJ FILE
    *SELECT (laNeedFls[lnNextFile,4])
    *APPEND BLANK
    *IF SEEK(STYDYE.STYLE+STYDYE.CWARECODE,laNeedFls[lnCurFile,4])
    *  SELECT (laNeedFls[lnCurFile,4])
    *  LOCATE REST WHILE STYLE+CWARECODE+CSESSION+DTOS(DTRDATE)+CTRCODE+STR(LINENO,6)=;
    *                    STYDYE.STYLE+STYDYE.CWARECODE FOR CDYELOT = STYDYE.DYELOT
    *  IF FOUND()
    *    lnUnitCost = NCOST
    *  ELSE
    *    lnUnitCost = 0
    *  ENDIF
    *  SELECT (laNeedFls[lnNextFile,4])
    *ELSE
    *  lnUnitCost = 0
    *ENDIF
    *REPLACE Style     WITH STYLE.Style,;
    *        cReason   WITH "Physical Count",;
    *        Date      WITH ldRpPrDt+1,;
    *        DPOSTDate WITH ldRpPrDt+1,;
    *        Type      WITH 'A',;
    *        cFromWare WITH STYDYE.cWareCode,;
    *        Unt_Cost  WITH lnUnitCost,;
    *        Old_Cost  WITH 0
    *
    *IF llLinkGL
    *  lcLinkCode = IIF(!EMPTY(STYDYE.GL_Link),STYDYE.GL_Link,'DEFDEF')
    *  REPLACE LINK_CODE WITH lcLinkCode,;
    *          GLFYEAR   WITH lcGLFYear,;
    *          GLPERIOD  WITH lcGLPeriod
    *ENDIF
    *
    *REPLACE dAdd_Date  WITH gdSysdate,;
    *        cAdd_Time  WITH TIME(),;
    *        cAdd_User  WITH "SMPURGE"
    *B607311,1 AMH [End]
    
    *--G/L Array difinition and initialization.
    IF llLinkGL
      DECLARE laGLDistAr[2,13]
      laGLDistAr[1,1] = lcLinkCode
      laGLDistAr[2,1] = lcLinkCode
      laGLDistAr[1,2] = '006'
      laGLDistAr[2,2] = '007'
      laGLDistAr[1,3] = 1
      laGLDistAr[2,3] = -1
      STORE 'IA'       TO laGLDistAr[1,4],laGLDistAr[2,4]
      STORE ''         TO laGLDistAr[1,5],laGLDistAr[2,5]
      STORE ldRpPrDt+1 TO laGLDistAr[1,6],laGLDistAr[2,6]
      STORE lcGLFYear  TO laGLDistAr[1,7],laGLDistAr[2,7]
      STORE lcGLPeriod TO laGLDistAr[1,8],laGLDistAr[2,8]
      STORE lcGLDTemp  TO laGLDistAr[1,9],laGLDistAr[2,9]
      laGLDistAr[2,10] = ''
    ELSE
      DIME laGLDistAr[1,1]
      laGLDistAr = ''
    ENDIF

    SELECT STYLE

    lcCurCmp = gcAct_Comp
    lcCurDir = gcDataDir
    gcAct_Comp = lcRpComp
    gcDataDir = lcDataDir

    IF SEEK(STYLE,'STYDYE')
      SELECT STYDYE
      SCAN REST WHILE Style+cWareCode+Dyelot = STYLE.STYLE
        IF STYLE.cdye_flg = 'N' .OR. EMPTY(Dyelot)
          lnStyCost = IIF(llWareHous,STYDYE.AVE_COST,STYLE.AVE_COST)

          *B608475,1 MHM 03/09/2008 Recalc[Start]
          IF lcStyCostM $'A'
            lnStyCost = lfClcAvcst()
          ENDIF
          *B608475,1 MHM 03/09/2008 [End]
          
        ENDIF
        IF STYLE.cdye_flg = 'Y' AND EMPTY(Dyelot)
          LOOP
        ENDIF
        lnRecNo = RECNO()
        *--seek in MatInvJl to get total STK's for the same style color
        SCATTER FIELDS Stk1,Stk2,Stk3,Stk4,Stk5,Stk6,Stk7,Stk8,TotStk TO laAdjust
        SCATTER FIELDS Stk1,Stk2,Stk3,Stk4,Stk5,Stk6,Stk7,Stk8,TotStk TO laOldStk
        IF SEEK(Style+cWareCode,laNeedFls[lnCurFile,4])
          SELECT (laNeedFls[lnCurFile,4])
          LOCATE REST WHILE STYLE+CWARECODE+CSESSION+DTOS(DTRDATE)+CTRCODE+STR(LINENO,6)=;
                            STYDYE.STYLE+STYDYE.CWARECODE FOR CDYELOT = STYDYE.DYELOT
          IF FOUND()
            lcAdReason = "Physical Count"
            SCAN REST WHILE STYLE+CWARECODE+CSESSION+DTOS(DTRDATE)+CTRCODE+STR(LINENO,6)=;
                            STYLE.STYLE+STYDYE.cWareCode FOR CDYELOT = STYDYE.DYELOT
              lnI = 1
              FOR lnI = 1 TO 8
                lcI = STR(lnI,1)
                laAdjust[lnI] = laAdjust[lnI] - EVALUATE('NSTK'+lcI)
              ENDFOR
              laAdjust[9] = laAdjust[9] - NTOTSTK
            ENDSCAN
          ENDIF
        ENDIF
        *--initialize stydye
        IF laAdjust[9] <> 0
          REPLACE STYDYE.STK1    WITH 0,;
                  STYDYE.STK2    WITH 0,;
                  STYDYE.STK3    WITH 0,;
                  STYDYE.STK4    WITH 0,;
                  STYDYE.STK5    WITH 0,;
                  STYDYE.STK6    WITH 0,;
                  STYDYE.STK7    WITH 0,;
                  STYDYE.STK8    WITH 0,;
                  STYDYE.TOTSTK  WITH 0,;
                  STYDYE.NSTKVAL WITH 0
          *--ADD a new  StyInvJl record with new STK's balance
          
          *B607311,1 AMH Call the gfStyCrl only one times [Start]
          *DECLARE laIssAdj[9],laRcvAdj[9]
          *STORE 0 TO laIssAdj,laRcvAdj
          *FOR lnI = 1 TO 8
          *  IF laAdjust[lnI] < 0
          *    laIssAdj[lnI] = laAdjust[lnI]
          *    laIssAdj[9] = laIssAdj[9] + laIssAdj[lnI]
          *  ELSE
          *    laRcvAdj[lnI] = laAdjust[lnI]
          *    laRcvAdj[9] = laRcvAdj[9] + laRcvAdj[lnI]
          *  ENDIF
          *ENDFOR
          *IF laIssAdj[9] <> 0
          *  =gfStyCrl('1',STYLE.STYLE,STYDYE.CWARECODE,STYDYE.DYELOT,ldRpPrDt+1,'',@laIssAdj,;
          *            lnStyCost,'',.T.,'',0,'','',@laGLDistAr,0,'','',@laOldStk)
          *ENDIF
          *IF laRcvAdj[9] <> 0
          *  =gfStyCrl('1',STYLE.STYLE,STYDYE.CWARECODE,STYDYE.DYELOT,ldRpPrDt+1,'',@laRcvAdj,;
          *            lnStyCost,'',.T.,'',0,'','',@laGLDistAr,0,'','',@laOldStk)
          *ENDIF
          lnRecCnt = RECCOUNT(laNeedFls[lnCurFile,4])
          IF laAdjust[9] <> 0
            =gfStyCrl('1',STYLE.STYLE,STYDYE.CWARECODE,STYDYE.DYELOT,ldRpPrDt+1,'',@laAdjust,;
                      lnStyCost,'',.T.,'',0,'','',@laGLDistAr,0,'','',@laOldStk)
          ENDIF
          
          *--append in INVTADJ FILE
          SELECT (laNeedFls[lnNextFile,4])
          APPEND BLANK
          IF SEEK(STYDYE.STYLE+STYDYE.CWARECODE,laNeedFls[lnCurFile,4])
            SELECT (laNeedFls[lnCurFile,4])
            LOCATE REST WHILE STYLE+CWARECODE+CSESSION+DTOS(DTRDATE)+CTRCODE+STR(LINENO,6)=;
                              STYDYE.STYLE+STYDYE.CWARECODE FOR CDYELOT = STYDYE.DYELOT
            IF FOUND()
              lnUnitCost = NCOST
            ELSE
              lnUnitCost = 0
            ENDIF
            SELECT (laNeedFls[lnNextFile,4])
          ELSE
            lnUnitCost = 0
          ENDIF
          REPLACE Style     WITH STYLE.Style,;
                  cReason   WITH "Physical Count",;
                  Date      WITH ldRpPrDt+1,;
                  DPOSTDate WITH ldRpPrDt+1,;
                  Type      WITH 'A',;
                  cFromWare WITH STYDYE.cWareCode,;
                  Unt_Cost  WITH lnUnitCost,;
                  Old_Cost  WITH 0
      
          IF llLinkGL
            lcLinkCode = IIF(!EMPTY(STYDYE.GL_Link),STYDYE.GL_Link,'DEFDEF')
            REPLACE LINK_CODE WITH lcLinkCode,;
                    GLFYEAR   WITH lcGLFYear,;
                    GLPERIOD  WITH lcGLPeriod
          ENDIF
          
          REPLACE dAdd_Date  WITH gdSysdate,;
                  cAdd_Time  WITH TIME(),;
                  cAdd_User  WITH "SMPURGE"
          
          *--update in StyInvJl with Adjustment reason
          SELECT (laNeedFls[lnCurFile,4])
          lcOrder = SET('ORDER')
          SET ORDER TO
          GO BOTTOM
          FOR lnQ = 1 TO RECCOUNT(laNeedFls[lnCurFile,4]) - lnRecCnt
            IF lnQ = 2
              SKIP -1
            ENDIF
            REPLACE REFERENCE WITH lcAdReason
            lcSession = cSession
            SELECT (laNeedFls[lnNextFile,4])
            IF lnQ = 2
              SCATTER MEMVAR
              APPEND BLANK
              GATHER MEMVAR
            ENDIF
            REPLACE cSession WITH lcSession
            FOR lnZ=1 TO 8
              lcZ = STR(lnZ,1)
              REPLACE Adj&lcZ    WITH EVALUATE(laNeedFls[lnCurFile,4]+'.NSTK'+lcZ),;
                      OldQty&lcZ WITH laoldStk[lnZ]
              laoldStk[lnZ] = laoldStk[lnZ] + EVALUATE('ADJ'+lcZ)
            ENDFOR
            REPLACE TotAdj     WITH EVALUATE(laNeedFls[lnCurFile,4]+'.NTOTSTK'),;
                    TotOld     WITH laOldStk[9]
            laoldStk[9] = laoldStk[9] + TOTADJ
            SELECT (laNeedFls[lnCurFile,4])
          ENDFOR
          SET ORDER TO &lcOrder.
          *B607311,1 AMH [End]
          
        ENDIF
        SELECT STYDYE
        GOTO lnRecNo
      ENDSCAN
    ENDIF
    SELECT STYLE
    gcDataDir = lcCurDir
    gcAct_Comp = lcCurCmp
    
    *B607311,1 AMH Move this part of code to the correct position [Start]
    *--update in StyInvJl with Adjustment reason
    *SELECT (laNeedFls[lnCurFile,4])
    *lcOrder = SET('ORDER')
    *SET ORDER TO
    *GO BOTTOM
    *REPLACE cAdjreason WITH lcAdReason
    *lcSession = cSession
    *SELECT (laNeedFls[lnNextFile,4])
    *REPLACE cSession WITH lcSession
    *FOR lnZ=1 TO 8
    *  lcZ = STR(lnZ,1)
    *  REPLACE Adj&lcZ    WITH laAdjust[lnZ],;
    *          OldQty&lcZ WITH laoldStk(lnZ)
    *ENDFOR
    *REPLACE TotAdj     WITH laAdjust[9],;
    *        TotOld     WITH laOldStk(9)
    *SELECT (laNeedFls[lnCurFile,4])
    *SET ORDER TO &lcOrder.
    *B607311,1 AMH [End]
    
  ENDSCAN

  IF llLinkGl .AND. USED(lcGLDTemp)
    USE IN (lcGLDTemp)
  ENDIF
  IF USED('GLDIST')
    USE IN GLDIST
  ENDIF
  IF USED('STYDYE')
    USE IN STYDYE
  ENDIF
  IF USED('STYLE')
    USE IN STYLE
  ENDIF
  IF USED('STYINVJL')
    USE IN STYINVJL
  ENDIF
  *--Erase the temp. file.
  IF llLinkGl
    ERASE (gcWorkDir+lcGLDTemp+'.DBF')
  ENDIF

*-- in case of costing method LIFO , FIFO
ELSE
  *--get all styles with balance equal Zero in a temp. file
  SELECT cSession,Style,cWareCode,cDyelot,cRSession,cISession,;
         cTrCode,cTrType,dTrDate,nCost,SUM(nStk1) AS 'nBal1',SUM(nStk2) AS 'nBal2',;
         SUM(nStk3) AS 'nBal3',SUM(nStk4) AS 'nBal4',SUM(nStk5) AS 'nBal5',;
         SUM(nStk6) AS 'nBal6',SUM(nStk7) AS 'nBal7',SUM(nStk8) AS 'nBal8';
  FROM   (laNeedFls[lnCurFile,4]);
  WHERE    DTRDATE <= ldRpPrDt;
  GROUP BY &laNeedFls[lnCurFile,4]..Style,;
           &laNeedFls[lnCurFile,4]..cWareCode,;
           &laNeedFls[lnCurFile,4]..cDyelot,; 
           &laNeedFls[lnCurFile,4]..cRSession;
  HAVING nBal1 = 0 .AND. nBal2 = 0 .AND. nBal3 = 0 .AND. nBal4 = 0 .AND. nBal5 = 0 .AND.;
         nBal6 = 0 .AND. nBal7 = 0 .AND. nBal8 = 0;
  ORDER BY &laNeedFls[lnCurFile,4]..Style,;
           &laNeedFls[lnCurFile,4]..cWareCode,;
           &laNeedFls[lnCurFile,4]..cDyelot,;
           &laNeedFls[lnCurFile,4]..cRSession;
  INTO DBF (gcWorkDir+lcTmpFile)


  SELECT (lcTmpFile)
  *--we calculate Stk's if sum equal zero for the same seasion number
  *-- purge those records
  SCAN
    
    IF LEFT(laNeedFls[lnCurFile,5],1) = 'A'  && if purge the active company
      *--Note the Following 
      *--We use laNeedFls[lnCurFile,4] And Increase lnCurFile to open each file we need accourding to 
      *-- alias , we open every table Of our Purge and the equivelant in the history company
      *--for ex. STYINVJL Source laNeedFls[lnCurFile,4] Target laNeedFls[lnCurFile +1,4]
      *--INVTADJ  Source laNeedFls[lnCurFile +2,4]  Target laNeedFls[lnCurFile + 3,4]
      *--UNCMSESS Source laNeedFls[lnCurFile +4,4]  Target laNeedFls[lnCurFile + 5,4]
      *--STYLE    Source laNeedFls[lnCurFile +6,4]  Target laNeedFls[lnCurFile + 7,4]
      *--STYDYE   Source laNeedFls[lnCurFile +8,4]  Target laNeedFls[lnCurFile + 9,4]
      *--NOTEPAD  Source laNeedFls[lnCurFile +10,4] Target laNeedFls[lnCurFile + 11,4]
      *--BOM      Source laNeedFls[lnCurFile +12,4] Target laNeedFls[lnCurFile + 13,4]
      
      IF SEEK(STYLE,laNeedFls[lnCurFile+6,4])
        SELECT (laNeedFls[lnCurFile+6,4])
        IF !SEEK(STYLE,laNeedFls[lnCurFile +7,4])
          SCATTER MEMVAR MEMO
          SELECT(laNeedFls[lnCurFile+7,4])
          APPEND BLANK
          GATHER MEMVAR MEMO
        ENDIF
        IF SEEK("F"+STYLE,laNeedFls[lnCurFile+10,4])
          SELECT (laNeedFls[lnCurFile+10,4])
          IF !SEEK(STYLE,laNeedFls[lnCurFile +11,4])
            SCATTER MEMVAR MEMO
            SELECT(laNeedFls[lnCurFile+11,4])
            APPEND BLANK
            GATHER MEMVAR MEMO
          ENDIF
          SELECT (laNeedFls[lnCurFile+6,4])
        ENDIF
        IF SEEK(CSTYMAJOR,laNeedFls[lnCurFile+12,4])
          SELECT (laNeedFls[lnCurFile+12,4])
          SCAN REST WHILE cItmMajor+Typ+cItmMask+MfgCode+Item+Iclr =;
                          EVALUATE(laNeedFls[lnCurFile+6,4]+'.CSTYMAJOR')
            IF !SEEK(STYLE,laNeedFls[lnCurFile +13,4])
              SCATTER MEMVAR MEMO
              SELECT(laNeedFls[lnCurFile+13,4])
              APPEND BLANK
              GATHER MEMVAR MEMO
            ENDIF
          ENDSCAN
          SELECT (laNeedFls[lnCurFile+6,4])
        ENDIF
        SELECT (lcTmpFile)
      ENDIF

      IF SEEK(STYLE+CWARECODE+CDYELOT,laNeedFls[lnCurFile+8,4])
        SELECT laNeedFls[lnCurFile+8,4]
        IF !SEEK(STYLE+cWareCode+Dyelot,laNeedFls[lnCurFile+9,4])
          SCATTER MEMVAR MEMO
          SELECT(laNeedFls[lnCurFile+9,4])
          APPEND BLANK
          GATHER MEMVAR MEMO
        ENDIF
        SELECT (lcTmpFile)
      ENDIF
      
      IF SEEK(STYLE+CWARECODE,laNeedFls[lnCurFile,4])
        SELECT (laNeedFls[lnCurFile,4])
        SCAN REST WHILE STYLE+CWARECODE+CSESSION+DTOS(DTRDATE)+CTRCODE+STR(LINENO,6)=;
                      &lcTmpFile..STYLE+&lcTmpFile..CWARECODE;
                    FOR CDYELOT = &lcTmpFile..CDYELOT .AND. (Crsession =&lcTmpFile..Crsession;
                        .OR. Cisession=&lcTmpFile..Cisession)
          IF !SEEK(STYLE+CWARECODE+CSESSION+DTOS(DTRDATE)+CTRCODE+STR(LINENO,6),laNeedFls[lnCurFile+1,4])
            SCATTER MEMO MEMVAR
            SELECT (laNeedFls[lnCurFile+1,4])
            APPEND BLANK
            GATHER MEMO MEMVAR
          ENDIF
          SELECT (laNeedFls[lnCurFile,4])
          DELETE
        ENDSCAN
      ENDIF
    ELSE && if purge the history company
      IF SEEK(STYLE+CWARECODE,laNeedFls[lnCurFile,4])
        SELECT (laNeedFls[lnCurFile,4])
        DELETE REST WHILE STYLE+CWARECODE+CSESSION+DTOS(DTRDATE)+CTRCODE+STR(LINENO,6)=;
                      &lcTmpFile..STYLE+&lcTmpFile..CWARECODE;
                    FOR CDYELOT = &lcTmpFile..CDYELOT .AND. (Crsession =&lcTmpFile..Crsession;
                        .OR. Cisession=&lcTmpFile..Cisession)
      ENDIF
    ENDIF
  ENDSCAN  
  SELECT (laNeedFls[lnCurFile,4])

  *--Erase temp file 
  IF USED(lcTmpFile)
    USE IN (lcTmpFile)
  ENDIF
  ERASE (gcWorkDir+lcTmpFile+'.DBF')
ENDIF 

*--close opend files
IF USED('STYDYE')
  USE IN STYDYE
ENDIF
IF USED('STYLE')
  USE IN STYLE
ENDIF
*--Restore Link check variable
llLinkGL = llStorLink 

*B606114,1 AMH [End]
*--- end of lfApStPhIv

*!*************************************************************
*! Name      : lfApFaPhIv
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/02/2000
*! Purpose   : Append Fabric Physical Inventory reflects the 
*!             total balance of the purged records.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ----
*!*************************************************************
*! Called from : lpPrTrn
*!*************************************************************
*! Passed Parameters  : lnCurFile (file to append physical rec)
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : = lfApFaPhIv()
*!*************************************************************

FUNCTION lfApFaPhIv
PARAMETER lnCurFile

*B604743,1 MHM 08/15/2001 Comment lfApFaPhIv FUNCTION becase we change the  way we purge [Start]
*lcGLFYear  = SPACE(04)  && Fiscal Year
*lcGLPeriod = SPACE(02)  && Period
*lcTmpFile = gfTempName() && temp name of dbf file to hold adjust qtys
*IF laNeedFls[lnCurFile,5]$'AAB'
*  SELECT cTrn_Seq,cFabric,cColor,cWareCode,cDyelot,dTranDate,cTranType,cTran,;
*         cRSession,cISession,cAdjreason,;
*         SUM(nReceived) AS 'nReceived',SUM(nIssued) AS 'nIssued',SUM(nUnitCost) AS 'nUnitCost',;
*         SEEK(cfabric+ccolor+cwarecode+cdyelot;
*         +crsession+cisession,laNeedFls[lnCurFile+1,4]) AS 'lCopied' ;
*  FROM  (laNeedFls[lnCurFile,4]) ;
*  WHERE DTRANDATE <= ldRpPrDt .AND. CTRANTYPE$"23";
*  GROUP BY &laNeedFls[lnCurFile,4]..CFABRIC,&laNeedFls[lnCurFile,4]..CCOLOR,;
*           &laNeedFls[lnCurFile,4]..cWareCode,&laNeedFls[lnCurFile,4]..cDyelot ;
*  HAVING !lCopied ;
*  INTO DBF (gcWorkDir+lcTmpFile)
*ELSE
*  SELECT cTrn_Seq,cFabric,cColor,cWareCode,cDyelot,dTranDate,cTranType,cTran,;
*         cRSession,cISession,cAdjreason,;
*         SUM(nReceived) AS 'nReceived',SUM(nIssued) AS 'nIssued',SUM(nUnitCost) AS 'nUnitCost' ;
*  FROM  (laNeedFls[lnCurFile,4]) ;
*  WHERE DTRANDATE <= ldRpPrDt .AND. CTRANTYPE$"23";
*  GROUP BY &laNeedFls[lnCurFile,4]..CFABRIC,&laNeedFls[lnCurFile,4]..CCOLOR,;
*           &laNeedFls[lnCurFile,4]..cWareCode,&laNeedFls[lnCurFile,4]..cDyelot ;
*  INTO DBF (gcWorkDir+lcTmpFile)
*ENDIF

*lnDirLn = RAT('\',laNeedFls[lnCurFile,2])
*lcDataDir = SUBSTR(laNeedFls[lnCurFile,2],1,lnDirLn)
*=gfOpenFile(lcDataDir+'FABRIC','FABRIC','SH')
*=gfOpenFile(lcDataDir+'FABDYE','FABDYE','SH')
*IF llLinkGL
*  =gfOpenFile(lcDataDir+'GLDIST','GLDISTAC','SH')
*  SELECT GLDIST
*  =AFIELDS(laGlData)
*  lcGLDTemp  = gfTempName()
*  CREATE TABLE gcWorkDir+(lcGLDTemp) FROM ARRAY laGlData
*ENDIF
*lcCurCmp = gcAct_Comp
*lcCurDir = gcDataDir
*gcAct_Comp = lcRpComp
*gcDataDir = lcDataDir
*=CHECKPRD(ldRpPrDt+1,'lcGLFYear','lcGLPeriod','MA',.T.)
*gcDataDir = lcCurDir
*gcAct_Comp = lcCurCmp
*SELECT (lcTmpFile)
*LOCATE
*lnNextFile = lnCurFile + IIF(laNeedFls[lnCurFile,5]$'AAB' , 2 , 1)
*SCAN
*  SELECT (laNeedFls[lnNextFile,4])
*  APPEND BLANK
*  REPLACE Fabric     WITH &lcTmpFile..cFabric,;
*          Color      WITH &lcTmpFile..cColor,;
*          cReason    WITH "Physical Count",; 
*          Date       WITH ldRpPrDt+1,;
*          Type       WITH 'P',;
*          cFromWare  WITH &lcTmpFile..cWareCode ,; 
*          nfUnitCost WITH &lcTmpFile..nUnitCost,;
*          nUntCstbuy WITH &lcTmpFile..nUnitCost

*  IF llLinkGL
*    lcLinkCode=IIF(SEEK(&lcTmpFile..cFabric+&lcTmpFile..cColor+&lcTmpFile..cWareCode+;
*                   &lcTmpFile..cDyelot,'FABDYE'),;
*               IIF(!EMPTY(FABDYE.GL_Link),FABDYE.GL_Link,'DEFDEF'),'DEFDEF')
*    REPLACE LINK_CODE WITH lcLinkCode,;
*            GLFYEAR   WITH lcGLFYear,;
*            GLPERIOD  WITH lcGLPeriod
*  ENDIF

*  REPLACE nmTotAdj   WITH &lcTmpFile..nReceived-&lcTmpFile..nIssued,;
*          Oldqty     WITH 0 ,;
*          nOldtoqty  WITH 0 ,;
*          Used_qty   WITH 0 ,;
*          dAdd_Date  WITH gdSysdate,;
*          cAdd_Time  WITH TIME(),;
*          cAdd_User  WITH "SMPURGE"
*  *--G/L Array difinition and initialization.
*  IF llLinkGL
*    DECLARE laGLDistAr[2,13]
*    laGLDistAr[1,1] = lcLinkCode
*    laGLDistAr[2,1] = lcLinkCode
*    laGLDistAr[1,2] = '015'
*    laGLDistAr[2,2] = '016'
*    laGLDistAr[1,3] = 1
*    laGLDistAr[2,3] = -1
*    STORE 'MA'       TO laGLDistAr[1,4],laGLDistAr[2,4]
*    STORE ''         TO laGLDistAr[1,5],laGLDistAr[2,5]
*    STORE ldRpPrDt+1 TO laGLDistAr[1,6],laGLDistAr[2,6]
*    STORE lcGLFYear  TO laGLDistAr[1,7],laGLDistAr[2,7]
*    STORE lcGLPeriod TO laGLDistAr[1,8],laGLDistAr[2,8]
*    STORE lcGLDTemp  TO laGLDistAr[1,9],laGLDistAr[2,9]
*    laGLDistAr[2,10] = ''
*  ELSE
*    DIME laGLDistAr[1,1]
*    laGLDistAr = ''
*  ENDIF
*  SELECT (lcTmpFile)
*  lcCurCmp = gcAct_Comp
*  lcCurDir = gcDataDir
*  gcAct_Comp = lcRpComp
*  gcDataDir = lcDataDir
*  = gfMatCrl('3',cFabric,cColor,cWareCode,cDyelot,ldRpPrDt+1,ldRpPrDt+1,SPACE(6),;
*                (nReceived-nIssued)*2,nUnitCost,'','',0,'','',@laGLDistAr)
*  gcDataDir = lcCurDir
*  gcAct_Comp = lcCurCmp
*  SELECT (laNeedFls[lnCurFile,4])
*  lcOrder = SET('ORDER')
*  SET ORDER TO
*  GO BOTTOM
*  REPLACE cAdjreason WITH &lcTmpFile..cAdjreason
*  lcSession = cTrn_Seq
*  SELECT (laNeedFls[lnNextFile,4])
*  REPLACE cTrn_Seq WITH lcSession
*  SELECT (laNeedFls[lnCurFile,4])
*  SET ORDER TO &lcOrder.
*ENDSCAN

*IF USED(lcTmpFile)
*  USE IN (lcTmpFile)
*ENDIF
*IF llLinkGl .AND. USED(lcGLDTemp)
*  USE IN (lcGLDTemp)
*ENDIF
*IF USED('GLDIST')
*  USE IN GLDIST
*ENDIF
*IF USED('FABDYE')
*  USE IN FABDYE
*ENDIF
*IF USED('FABRIC')
*  USE IN FABRIC
*ENDIF
*IF USED('MATINVJL')
*  USE IN MATINVJL
*ENDIF
*--Erase the temp. file.
*ERASE (gcWorkDir+lcTmpFile+'.DBF')
*IF llLinkGl
*  ERASE (gcWorkDir+lcGLDTemp+'.DBF')
*ENDIF

*--we start calculate MATINVJL after Purge in case of  
*--AVARGE or STANDERD :
*--                   we purge all remaning record we calculate to get ON HAND 
*--LIFO ,FIFO,LOT :
*--                we will delete only all records with balance of ON HAND
*--                equal zero that has the same session Number

*--save link check variable
llStorLink = llLinkGL
llLinkGL = .F.
lcGLFYear  = SPACE(04)  && Fiscal Year
lcGLPeriod = SPACE(02)  && Period
lcTmpFile = gfTempName() && temp name of dbf file to hold adjust qtys

*B604743,4 MHM 10/09/2001 Define variable to get warehous[Start]
llWareHous = gfGetMemVar('M_WareHouse',lcRpComp)= 'Y'
*B604743,4 MHM 10/09/2001 [End]

lnDirLn = RAT('\',laNeedFls[lnCurFile,2])
lcDataDir = SUBSTR(laNeedFls[lnCurFile,2],1,lnDirLn)
=gfOpenFile(lcDataDir+'FABRIC','FABRIC','SH')
=gfOpenFile(lcDataDir+'FABDYE','FABDYE','SH')
IF llLinkGL
  =gfOpenFile(lcDataDir+'GLDIST','GLDISTAC','SH')
  SELECT GLDIST
  =AFIELDS(laGlData)
  lcGLDTemp  = gfTempName()
  CREATE TABLE gcWorkDir+(lcGLDTemp) FROM ARRAY laGlData
ENDIF
lcCurCmp = gcAct_Comp
lcCurDir = gcDataDir
gcAct_Comp = lcRpComp
gcDataDir = lcDataDir
=CHECKPRD(ldRpPrDt+1,'lcGLFYear','lcGLPeriod','MA',.T.)
gcDataDir = lcCurDir
gcAct_Comp = lcCurCmp

*-in case of Costing AVARAGE OR SATANDERD
IF lcCstMth $'AS'
  STORE '' TO lcAdReason
  SELECT FABRIC
  LOCATE
  lnNextFile = lnCurFile + IIF(laNeedFls[lnCurFile,5]$'AAB' , 2 , 1)

  SCAN
    STORE 0 TO lnFabCost,lnOnHand
    =SEEK(Fabric+Color,'FABDYE')

    *--append in FINVADJ FILE
    SELECT (laNeedFls[lnNextFile,4])
    APPEND BLANK
    IF SEEK(FABDYE.Fabric+FABDYE.Color+FABDYE.cWareCode+FABDYE.Dyelot,laNeedFls[lnCurFile,4])
      lnUnitCost = &laNeedFls[lnCurFile,4]..nUnitCost
    ELSE
      lnUnitCost = 0
    ENDIF
    REPLACE Fabric     WITH FABRIC.Fabric,;
            Color      WITH FABRIC.Color,;
            cReason    WITH "Physical Count",;  
            Date       WITH ldRpPrDt+1,;
            Type       WITH 'P',;
            cFromWare  WITH FABDYE.cWareCode ,; 
            nfUnitCost WITH lnUnitCost,;
            nUntCstbuy WITH lnUnitCost
   
    IF llLinkGL
      lcLinkCode=IIF(SEEK(FABRIC.Fabric+FABRIC.Color+FABRIC.cWareCode+;
                     FABRIC.Dyelot,'FABDYE'),;
                 IIF(!EMPTY(FABDYE.GL_Link),FABDYE.GL_Link,'DEFDEF'),'DEFDEF')

     REPLACE LINK_CODE WITH lcLinkCode,;
              GLFYEAR   WITH lcGLFYear,;
              GLPERIOD  WITH lcGLPeriod
    ENDIF
 
    REPLACE Oldqty     WITH 0 ,;
            nOldtoqty  WITH 0 ,;
            Used_qty   WITH 0 ,;
            dAdd_Date  WITH gdSysdate,;
            cAdd_Time  WITH TIME(),;
            cAdd_User  WITH "SMPURGE"
    *--G/L Array difinition and initialization.
    IF llLinkGL
      DECLARE laGLDistAr[2,13]
      laGLDistAr[1,1] = lcLinkCode
      laGLDistAr[2,1] = lcLinkCode
      laGLDistAr[1,2] = '015'
      laGLDistAr[2,2] = '016'
      laGLDistAr[1,3] = 1
      laGLDistAr[2,3] = -1
      STORE 'MA'       TO laGLDistAr[1,4],laGLDistAr[2,4]
      STORE ''         TO laGLDistAr[1,5],laGLDistAr[2,5]
      STORE ldRpPrDt+1 TO laGLDistAr[1,6],laGLDistAr[2,6]
      STORE lcGLFYear  TO laGLDistAr[1,7],laGLDistAr[2,7]
      STORE lcGLPeriod TO laGLDistAr[1,8],laGLDistAr[2,8]
      STORE lcGLDTemp  TO laGLDistAr[1,9],laGLDistAr[2,9]
      laGLDistAr[2,10] = ''
    ELSE
      DIME laGLDistAr[1,1]
      laGLDistAr = ''
    ENDIF

    SELECT FABRIC

    lcCurCmp = gcAct_Comp
    lcCurDir = gcDataDir
    gcAct_Comp = lcRpComp
    gcDataDir = lcDataDir

    IF SEEK(Fabric+Color,'FABDYE')
      SELECT FABDYE
      
      *B604743,4 MHM 10/09/2001 we have to consider dyelot too [Start]
      *SCAN REST WHILE Fabric+Color+cWareCode+Dyelot = FABRIC.FABRIC+FABRIC.COLOR FOR EMPTY(Dyelot)
      SCAN REST WHILE Fabric+Color+cWareCode+Dyelot = FABRIC.FABRIC+FABRIC.COLOR 
        IF FABRIC.cdye_flg = 'Y' AND EMPTY(Dyelot)
          lnFabCost = IIF(llWareHous,FabDye.nAveCstBuy,Fabric.NAveCstBuy)/Fabric.Conv
          LOOP
        ENDIF
      *B604743,1 MHM 10/09/2001 [END]
      
        lnRecNo = RECNO()
        *--seek in MatInvJl to get total ON HAND for the same fabric color

        *B604743,5 MHM 10/09/2001 get onhand [Start]
        lnOnHand  = FABDYE.ONHAND
        *B604743,5 MHM 10/09/2001 [End]

        IF SEEK(Fabric+Color+cWareCode+Dyelot,laNeedFls[lnCurFile,4])
          SELECT (laNeedFls[lnCurFile,4])

          *--lcAdReason  --------> to get adjast. reason
          lcAdReason = cAdjreason

          *B604743,4 MHM 10/09/2001 GET correct cost [Start]
          *lnFabCost = FABDYE.NFAVE_COST
          IF FABRIC.cdye_flg = 'N' 
            lnFabCost = IIF(llWareHous,FabDye.nAveCstBuy,Fabric.NAveCstBuy)/Fabric.Conv
          ENDIF  
          *B604743,4 MHM 10/09/2001 [Start]
          
          SCAN REST WHILE Cfabric+Ccolor+Cwarecode+Cdyelot+Crsession+Cisession=;
                          FABDYE.FABRIC+FABDYE.COLOR+FABDYE.cWareCode+;
                          FABDYE.DYELOT FOR DTRANDATE > ldRpPrDt
            lnOnHand = lnOnHand - NRECEIVED + NISSUED
          ENDSCAN
        ENDIF
        *--initialize fabdye
        *B604743,5 MHM 10/09/2001  [Start]
        IF lnOnHand<> 0 
        *B604743,5 MHM [End]
          REPLACE FABDYE.ONHAND WITH 0
          *--ADD a new  MatInvJl record with new ONHAND balance
          = gfMatCrl('2',FABDYE.Fabric,FABDYE.Color,FABDYE.cWareCode,FABDYE.Dyelot,ldRpPrDt+1,ldRpPrDt+1,SPACE(6),;
                     lnOnHand,lnFabCost,'','',0,'','',@laGLDistAr)
        *B604743,5 MHM 10/09/2001  [Start]
        ENDIF
        *B604743,5 MHM [End]
        SELECT FABDYE
        GOTO lnRecNo
      ENDSCAN
    ENDIF
    SELECT FABRIC
    gcDataDir = lcCurDir
    gcAct_Comp = lcCurCmp
    *--update in MatInvJl with Adjustement reason
    SELECT (laNeedFls[lnCurFile,4])
    lcOrder = SET('ORDER')
    SET ORDER TO
    GO BOTTOM
    REPLACE cAdjreason WITH lcAdReason
    lcSession = cTrn_Seq
    SELECT (laNeedFls[lnNextFile,4])
    REPLACE cTrn_Seq WITH lcSession,;
            nmTotAdj WITH lnOnHand
    SELECT (laNeedFls[lnCurFile,4])
    SET ORDER TO &lcOrder.
  ENDSCAN

  IF llLinkGl .AND. USED(lcGLDTemp)
    USE IN (lcGLDTemp)
  ENDIF
  IF USED('GLDIST')
    USE IN GLDIST
  ENDIF
  IF USED('FABDYE')
    USE IN FABDYE
  ENDIF
  IF USED('FABRIC')
    USE IN FABRIC
  ENDIF
  IF USED('MATINVJL')
    USE IN MATINVJL
  ENDIF
  *--Erase the temp. file.
  IF llLinkGl
    ERASE (gcWorkDir+lcGLDTemp+'.DBF')
  ENDIF

*-- in case of costing method LIFO , FIFO and LOT
ELSE
  
  *B040155,3  TMI [Start] fix a bug that if a physical inventory is done for material just entered with 0 stock then both 
  *                       CRSESSION ,CISESSION is empty
  SELECT (laNeedFls[lnCurFile,4])
  GO TOP
  REPLACE CISESSION WITH CTRN_SEQ  ;
          REFERENCE WITH 'issue sess. updated by Purge' ;
    FOR CTRANTYPE = '3' AND EMPTY(CRSESSION) AND EMPTY(CISESSION)
  *B040155,3  TMI [End  ] 

  *--get all fabrics with balance equal Zero in a temp. file
  *B040155,3  TMI [Start] check that both NRECEIVED+NISSUED > 0
  *-* SELECT cTrn_Seq,cFabric,cColor,cWareCode,cDyelot,cRSession,cISession ,;
  *-*        cTran,cTranType,dTranDate,dPostDate,nUnitCost,nUntCstBuy      ,;
  *-*        SUM(nReceived-nIssued) AS 'nBalance';
  *-* FROM   (laNeedFls[lnCurFile,4]);
  *-* WHERE    DTRANDATE <= ldRpPrDt;
  *-* GROUP BY &laNeedFls[lnCurFile,4]..cFabric,;
  *-*          &laNeedFls[lnCurFile,4]..cColor,;
  *-*          &laNeedFls[lnCurFile,4]..cWareCode,;
  *-*          &laNeedFls[lnCurFile,4]..cDyelot,; 
  *-*          &laNeedFls[lnCurFile,4]..cRSession;
  *-* HAVING nBalance = 0;
  *-* ORDER BY &laNeedFls[lnCurFile,4]..cFabric,;
  *-*          &laNeedFls[lnCurFile,4]..cColor ,;
  *-*          &laNeedFls[lnCurFile,4]..cWareCode,;
  *-*          &laNeedFls[lnCurFile,4]..cDyelot,;
  *-*          &laNeedFls[lnCurFile,4]..cRSession;
  *-* INTO DBF (gcWorkDir+lcTmpFile)
  SELECT cTrn_Seq,cFabric,cColor,cWareCode,cDyelot,cRSession,cISession ,;
         cTran,cTranType,dTranDate,dPostDate,nUnitCost,nUntCstBuy      ,;
         SUM(nReceived-nIssued) AS 'nBalance';
  FROM   (laNeedFls[lnCurFile,4]);
  WHERE    DTRANDATE <= ldRpPrDt AND ;
           NRECEIVED+NISSUED > 0 ;
  GROUP BY &laNeedFls[lnCurFile,4]..cFabric,;
           &laNeedFls[lnCurFile,4]..cColor,;
           &laNeedFls[lnCurFile,4]..cWareCode,;
           &laNeedFls[lnCurFile,4]..cDyelot,; 
           &laNeedFls[lnCurFile,4]..cRSession;
  HAVING nBalance = 0;
  ORDER BY &laNeedFls[lnCurFile,4]..cFabric,;
           &laNeedFls[lnCurFile,4]..cColor ,;
           &laNeedFls[lnCurFile,4]..cWareCode,;
           &laNeedFls[lnCurFile,4]..cDyelot,;
           &laNeedFls[lnCurFile,4]..cRSession;
  INTO DBF (gcWorkDir+lcTmpFile)
  *B040155,3  TMI [End  ] 
  
  SELECT (lcTmpFile)
  *--we calculate on hand if sum equal zero for the same seasion number
  *-- purge those records
  SCAN
    
    *B604743,4 MHM 10/09/2001 Get Fabric ,Fabdye , Notepad From its file If Found [Start]
    *--Note the Following 
    *--We use laNeedFls[lnCurFile,4] And Increase lnCurFile to open each file we need accourding to 
    *-- alias , we open every table Of our Purge and the equivelant in the history company
    *--for ex. MAtinvjl Source laNeedFls[lnCurFile,4] Target laNeedFls[lnCurFile +1,4]
    *--FinvAdj  Source laNeedFls[lnCurFile +2,4] Target laNeedFls[lnCurFile + 3,4]
    *--Fabric   Source laNeedFls[lnCurFile +4,4] Target laNeedFls[lnCurFile + 5,4]
    *--FabDye   Source laNeedFls[lnCurFile +6,4] Target laNeedFls[lnCurFile + 7,4]
    *--NotePad  Source laNeedFls[lnCurFile +8,4] Target laNeedFls[lnCurFile + 9,4]
    *--Rolls    Source laNeedFls[lnCurFile +10,4] Target laNeedFls[lnCurFile + 11,4]
    
    =SEEK(&lcTmpFile..CFABRIC+&lcTmpFile..CCOLOR,laNeedFls[lnCurFile+4,4])
    SELECT (laNeedFls[lnCurFile+4,4])
    IF !SEEK(Fabric+Color,laNeedFls[lnCurFile +5,4])
      SCATTER MEMVAR MEMO
      SELECT(laNeedFls[lnCurFile+5,4])
      APPEND BLANK
      GATHER MEMVAR MEMO
      IF !SEEK(Fabric+Color,laNeedFls[lnCurFile+7,4])
        IF SEEK(Fabric+Color,laNeedFls[lnCurFile+6,4])
          SELECT laNeedFls[lnCurFile+6,4]
          SCAN REST WHILE Fabric+Color+cWareCode+DyeLot = &laNeedFls[lnCurFile +4,4]..fabric + &laNeedFls[lnCurFile +4,4]..color
            SCATTER MEMVAR MEMO
            SELECT(laNeedFls[lnCurFile+7,4])
            APPEND BLANK
            GATHER MEMVAR MEMO
          ENDSCAN
        ENDIF  
      ENDIF
    ENDIF  

    *--get from NotePad For Material
    SELECT laNeedFls[lnCurFile+4,4]
    IF SEEK('G'+Fabric,laNeedFls[lnCurFile+8,4])
      SELECT(laNeedFls[lnCurFile + 8,4])
      IF !SEEK('G'+Fabric,laNeedFls[lnCurFile + 9,4])
        SCATTER MEMVAR MEMO
        SELECT(laNeedFls[lnCurFile + 9,4])
        APPEND BLANK
        GATHER MEMVAR MEMO
      ENDIF
    ENDIF
    SELECT (laNeedFls[lnCurFile,4])
    *B604743,4 MHM  [End]
    
    IF SEEK(&lcTmpFile..CFABRIC+&lcTmpFile..CCOLOR+&lcTmpFile..CWARECODE)
      SCAN REST WHILE Cfabric+Ccolor+Cwarecode+Cdyelot+Crsession+Cisession=;
                    &lcTmpFile..CFABRIC+&lcTmpFile..CCOLOR+&lcTmpFile..CWARECODE+;
                    &lcTmpFile..CDYELOT FOR Crsession =&lcTmpFile..Crsession ;
                    .OR. Cisession=&lcTmpFile..Cisession
        *--we handle ROLLS file here
        *--if costing method is LOT then move all ROLLS record for the same cTrn_Seq in the MatInvJl
        *-- and sume of them is zero means that we issue all this roll
        SELECT (laNeedFls[lnCurFile+10,4])
        IF lcCstMth = 'L' .AND. SEEK(&laNeedFls[lnCurFile,4]..cTrn_Seq)
          SCAN REST WHILE csession+crollitem+color+cwarecode+dyelot+crollid = &laNeedFls[lnCurFile,4]..cTrn_Seq 
            SCATTER MEMO MEMVAR
            SELECT (laNeedFls[lnCurFile+11,4])
            APPEND BLANK
            GATHER MEMO MEMVAR
            SELECT (laNeedFls[lnCurFile+10,4])
            DELE                
          ENDSCAN
        ENDIF
        SELECT (laNeedFls[lnCurFile,4])
        SCATTER MEMO MEMVAR
        SELECT (laNeedFls[lnCurFile+1,4])
        APPEND BLANK
        GATHER MEMO MEMVAR
        
        SELECT (laNeedFls[lnCurFile,4])
        DELE 
      ENDSCAN
    ENDIF
  ENDSCAN  
  SELECT (laNeedFls[lnCurFile,4])

  *--Erase temp file 
  IF USED(lcTmpFile)
    USE IN (lcTmpFile)
  ENDIF
  ERASE (gcWorkDir+lcTmpFile+'.DBF')
ENDIF 

*--close opend files
IF USED('FABDYE')
  USE IN FABDYE
ENDIF
IF USED('FABRIC')
  USE IN FABRIC
ENDIF
*--Restore Link check variable
llLinkGL = llStorLink 

*B604743,1 MHM 08/15/2001 [end]
*--- end of lfApFaPhIv

*!*************************************************************
*! Name      : lfPackFile
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/02/2000
*! Purpose   : Pack Needed Files.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ----
*!*************************************************************
*! Called from : lPPurge
*!*************************************************************
*! Passed Parameters  : lcTrn (transaction name)
*!                      lnFrsFl (frist file in transactoin)
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : =lfPackFile()
*!*************************************************************

FUNCTION lfPackFile
PARAMETER lcTrn, lnFrsFl
PRIVATE lnFleCount, lnI, lnPack, lnJ

lnFleCount = ALEN(laNeedFls,1)
lcAllFiles = ''  && get all files name
lnI = lnFrsFl
lnPack = 0  && number of file need to pack
DO WHILE laNeedFls[lnI,1] = lcTrn  && loop for files of transaction
  lnFilePos = RAT('\',laNeedFls[lnI,2]) + 1       && get file name posation
  lcFileName = SUBSTR(laNeedFls[lnI,2],lnFilePos) && get file name
  IF FILE(laNeedFls[lnI,2]+'.dbf')
    IF !(lcFileName$lcAllFiles)
      lnPack = lnPack +1
      DECLARE laPackFile[lnPack] && array of file need to pack
      laPackFile[lnPack] = laNeedFls[lnI,4]
    ELSE
      =gfCloseFile(laNeedFls[lnI,4])
    ENDIF
  ENDIF
  lcAllFiles = lcAllFiles + IIF(lcFileName$lcAllFiles,'',lcFileName+'\')
  lnI = lnI + 1
  IF lnI > lnFleCount
    EXIT
  ENDIF
ENDDO
lnJ = 1
FOR lnJ = 1 TO lnPack
  SELECT (laPackFile[lnJ])
  PACK
  =gfCloseFile(laPackFile[lnJ])
ENDFOR
*--- end of lfPackFile.

*!*************************************************************
*! Name      : lfRunRebal
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/02/2000
*! Purpose   : Run Rebalance.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ----
*!*************************************************************
*! Called from : lPPurge
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : =lfRunRebal()
*!*************************************************************

FUNCTION lfRunRebal
PARAMETER lcRebCmp
lcNeedRebal = ''
lnI = 1
FOR lnI = 1 TO 16
  IF laTransact[lnI,3]
    DO CASE
      CASE laTransact[lnI,2] = 'AR'
        lcNeedRebal = IIF('SH,ST'$lcNeedRebal,lcNeedRebal,lcNeedRebal + ',SH,ST' + '')
      CASE laTransact[lnI,2] = 'SO' .OR. laTransact[lnI,2] = 'AL'
        lcNeedRebal = IIF('OR,AL'$lcNeedRebal,lcNeedRebal,lcNeedRebal + ',OR,AL' + '')
      CASE laTransact[lnI,2] = 'RM'
        lcNeedRebal = IIF('RE,RA'$lcNeedRebal,lcNeedRebal,lcNeedRebal + ',RE,RA' + '')
      CASE laTransact[lnI,2] = 'IC'
        lcNeedRebal = IIF('ST'$lcNeedRebal,lcNeedRebal,lcNeedRebal + ',ST' + '')
      CASE laTransact[lnI,2] = 'MA'
        lcNeedRebal = IIF('ONO,MH'$lcNeedRebal,lcNeedRebal,lcNeedRebal + ',ONO,MH' + '')
      CASE laTransact[lnI,2] = 'MF' .OR. laTransact[lnI,2] = 'PO'
        lcNeedRebal = IIF('WI,WO'$lcNeedRebal,lcNeedRebal,lcNeedRebal + ',WI,WO' + '')
      CASE laTransact[lnI,2] = 'PO'
        lcNeedRebal = IIF('IN'$lcNeedRebal,lcNeedRebal,lcNeedRebal + ',IN' + '')
    ENDCASE
  ENDIF
ENDFOR
IF AT(',',lcNeedRebal) = 1
  lcNeedRebal = SUBSTR(lcNeedRebal,2)
ENDIF
IF !EMPTY(lcNeedRebal)
  DO (gcAppHome+"SM\SMREBAL.FXP") WITH lcNeedRebal, lcRebCmp, .T.
ENDIF
*--- end of lfRunRebal.

*!*************************************************************
*! Name      : lfSkipFile
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/02/2000
*! Purpose   : Flage the related files as purged.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ----
*!*************************************************************
*! Called from : lfScanFile
*!*************************************************************
*! Passed Parameters  : lnPos (file number)
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : =lfSkipFile()
*!*************************************************************

FUNCTION lfSkipFile
PARAMETER lnPos
PRIVATE lnFPosR, lnLPosR, lnFPosD, lnLPosD, lnRPos

IF !EMPTY(laNeedFls[lnPos,6]) && if the file has related files
  *-- lnFPosR = frist posatoin of related file number
  *-- lnLPosR = last posation of related file number
  *-- lnRPos = the number or related file
  STORE 0 TO lnFPosR , lnFPosD
  DO WHILE lnFPosR < LEN(laNeedFls[lnPos,6])  && loop for existing file
    lnLPosR = AT(',',SUBSTR(laNeedFls[lnPos,6],lnFPosR+1))
    lnRPos = VAL(SUBSTR(laNeedFls[lnPos,6],lnFPosR+1,lnLPosR-1))
    laNeedFls[lnRPos,9] = .T.
    IF laNeedFls[lnRPos,5]$'AAB'
      laNeedFls[lnRPos+1,9] = .T.
    ENDIF
    =lfSkipFile(lnRPos)
    lnFPosR = lnFPosR + lnLPosR
  ENDDO
ENDIF  && end if the file has related files
*--- end of lfSkipFile.

*!*************************************************************
*! Name      : lfZeroUpdt
*! Developer : AHMED MAHER (AMH)
*! Date      : 11/13/2000
*! Purpose   : update style , stydye , fabric , fabdye for 
*!             numeric fields by zeros
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ----
*!*************************************************************
*! Called from : lpPurge
*!*************************************************************
*! Passed Parameters  : lnUpPos (the row no. of laNeedFls array)
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : =lfZeroUpdt(X)
*!*************************************************************

FUNCTION lfZeroUpdt
PARAMETER lnUpPos
PRIVATE lnUpPos

IF RIGHT(laNeedFls[lnUpPos,2],5) = 'STYLE' .OR. RIGHT(laNeedFls[lnUpPos,2],6) = 'STYDYE'
  REPLACE ORD1 WITH 0 , ORD2 WITH 0 , ORD3 WITH 0 , ORD4 WITH 0 ,;
          ORD5 WITH 0 , ORD6 WITH 0 , ORD7 WITH 0 , ORD8 WITH 0 ,;
          WIP1 WITH 0 , WIP2 WITH 0 , WIP3 WITH 0 , WIP4 WITH 0 ,;
          WIP5 WITH 0 , WIP6 WITH 0 , WIP7 WITH 0 , WIP8 WITH 0 ,;
          STK1 WITH 0 , STK2 WITH 0 , STK3 WITH 0 , STK4 WITH 0 ,;
          STK5 WITH 0 , STK6 WITH 0 , STK7 WITH 0 , STK8 WITH 0 ,;
          ALO1 WITH 0 , ALO2 WITH 0 , ALO3 WITH 0 , ALO4 WITH 0 ,;
          ALO5 WITH 0 , ALO6 WITH 0 , ALO7 WITH 0 , ALO8 WITH 0 ,;
          SHP1 WITH 0 , SHP2 WITH 0 , SHP3 WITH 0 , SHP4 WITH 0 ,;
          SHP5 WITH 0 , SHP6 WITH 0 , SHP7 WITH 0 , SHP8 WITH 0 ,;
          RET1 WITH 0 , RET2 WITH 0 , RET3 WITH 0 , RET4 WITH 0 ,;
          RET5 WITH 0 , RET6 WITH 0 , RET7 WITH 0 , RET8 WITH 0 ,;
          RA1 WITH 0 , RA2 WITH 0 , RA3 WITH 0 , RA4 WITH 0 ,;
          RA5 WITH 0 , RA6 WITH 0 , RA7 WITH 0 , RA8 WITH 0 ,;
          INTRANS1 WITH 0 , INTRANS2 WITH 0 , INTRANS3 WITH 0 , INTRANS4 WITH 0 ,;
          INTRANS5 WITH 0 , INTRANS6 WITH 0 , INTRANS7 WITH 0 , INTRANS8 WITH 0 ,;
          NWO1 WITH 0 , NWO2 WITH 0 , NWO3 WITH 0 , NWO4 WITH 0 ,;
          NWO5 WITH 0 , NWO6 WITH 0 , NWO7 WITH 0 , NWO8 WITH 0 ,;
          TOTORD WITH 0 , TOTWIP WITH 0 , TOTSTK WITH 0 , TOTALO WITH 0 ,;
          TOTSHP WITH 0 , TOTRET WITH 0 , TOTRA WITH 0 , TOTINTRN WITH 0 , NTOTWO WITH 0
ENDIF
IF RIGHT(laNeedFls[lnUpPos,2],6) = 'FABRIC' .OR. RIGHT(laNeedFls[lnUpPos,2],6) = 'FABDYE'
  REPLACE ONHAND WITH 0 , ONORDER WITH 0 , ONRET WITH 0 , USAGE WITH 0 , NMATWIP WITH 0
ENDIF
*--- end of lfZeroUpdt.

*!*************************************************************
*! Name      : lfPurgStJl
*! Developer : AHMED MAHER (AMH)
*! Date      : 11/21/2000
*! Purpose   : Copy Style Invtory Jornal.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ----
*!*************************************************************
*! Called from : lpPurge
*!*************************************************************
*! Passed Parameters  : lcStySeq (the last Session Purged)
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : =lfPurgStJl(X)
*!*************************************************************
*
FUNCTION lfPurgStJl
PARAMETER lcStySeq
PRIVATE lcStySeq, lcActStJl, lcHstStJl, lcLstKey, lcKey, llCopied

lcActStJl = gfTempName()
lcHstStJl = gfTempName()
lcHstStyl = gfTempName()

=gfOpenFile(lcACmpDir+'STYINVJL','','SH', @lcActStJl , .T.)
=gfOpenFile(lcHCmpDir+'STYINVJL','STYINVJL','SH', @lcHstStJl , .T.)
=gfOpenFile(lcHCmpDir+'STYLE','STYLE','SH', @lcHstStyl , .T.)

SELECT (lcActStJl)
LOCATE
lcLstKey = ''
llCopied = SEEK(style+cwarecode+csession+DTOS(dtrdate)+ctrcode+STR(lineno,6),lcHstStJl);
           .OR. !SEEK(style,lcHstStyl)
SCAN REST WHILE csession <= lcStySeq
  lcKey = style+cwarecode+csession+DTOS(dtrdate)+ctrcode+STR(lineno,6)
  IF lcKey <> lcLstKey
    llCopied = SEEK(style+cwarecode+csession+DTOS(dtrdate)+ctrcode+STR(lineno,6),lcHstStJl);
               .OR. !SEEK(style,lcHstStyl)
  ENDIF
  
  *B804119,1 AMH Fix Illegal value [Start]
  *IF VAL(lcStySeq) > 0
   * =gfThermo(VAL(lcStySeq),VAL(csession),"Copy Style Inventory Journal :","Session No. : "+csession)
  *ELSE
    WAIT WINDOW "Copy Style Inventory Journal, Session No. : "+csession NOWAIT
  *ENDIF
  *B804119,1 AMH [End]
  IF !llCopied
    SCATTER MEMO MEMVAR
    SELECT (lcHstStJl)
    APPEND BLANK
    GATHER MEMO MEMVAR
    SELECT (lcActStJl)
  ENDIF
  lcLstKey = lcKey
ENDSCAN
WAIT CLEAR

=gfCloseFile(lcHstStyl)
=gfCloseFile(lcHstStJl)
=gfCloseFile(lcActStJl)
*--- end of lfPurgStJl.

*!*************************************************************
*! Name      : lfPurgMtJl
*! Developer : AHMED MAHER (AMH)
*! Date      : 11/22/2000
*! Purpose   : Copy Fabric Invtory Jornal.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ----
*!*************************************************************
*! Called from : lpPurge
*!*************************************************************
*! Passed Parameters  : lcMatSeq (the last Session Purged)
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : =lfPurgMtJl(X)
*!*************************************************************
*
FUNCTION lfPurgMtJl
PARAMETER lcMatSeq
PRIVATE lcMatSeq, lcActStJl, lcHstStJl, lcLstKey, lcKey, llCopied

lcActMtJl = gfTempName()
lcHstMtJl = gfTempName()
lcHstFabr = gfTempName()

=gfOpenFile(lcACmpDir+'MATINVJL','','SH', @lcActMtJl , .T.)
=gfOpenFile(lcHCmpDir+'MATINVJL','MATINVJL','SH', @lcHstMtJl , .T.)
=gfOpenFile(lcHCmpDir+'FABRIC','FABRIC','SH', @lcHstFabr , .T.)

SELECT (lcActMtJl)
LOCATE
lcLstKey = ''
llCopied = SEEK(cfabric+ccolor+cwarecode+cdyelot+crsession+cisession,lcHstMtJl);
           .OR. !SEEK(cfabric+ccolor,lcHstFabr)
SCAN REST WHILE ctrn_seq <= lcMatSeq
  lcKey = cfabric+ccolor+cwarecode+cdyelot+crsession+cisession
  IF lcKey <> lcLstKey
    llCopied = SEEK(cfabric+ccolor+cwarecode+cdyelot+crsession+cisession,lcHstMtJl);
               .OR. !SEEK(cfabric+ccolor,lcHstFabr)
  ENDIF
  *B804119,1 AMH Fix Illegal value [Start]
  *IF VAL(lcMatSeq) > 0
    *=gfThermo(VAL(lcMatSeq),VAL(ctrn_seq),"Copy Fabric Inventory Journal :","Session No. : "+ctrn_seq)
  *ELSE
    WAIT WINDOW "Copy Fabric Inventory Journal, Session No. : "+ctrn_seq NOWAIT
  *ENDIF
  *B804119,1 AMH [End]
  IF !llCopied
    SCATTER MEMO MEMVAR
    SELECT (lcHstMtJl)
    APPEND BLANK
    GATHER MEMO MEMVAR
    SELECT (lcActMtJl)
  ENDIF
  lcLstKey = lcKey
ENDSCAN
WAIT CLEAR

=gfCloseFile(lcHstFabr)
=gfCloseFile(lcHstMtJl)
=gfCloseFile(lcActMtJl)
*--- end of lfPurgMtJl.
*:*************************************************************
*: Name      : lfPurgRols
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 02/13/2002
*: Purpose   : Purge the rolls file to history comapny.
*:*************************************************************
*: Called from : Prog.
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : File Name.
*:*************************************************************
*: Return      : None.
*:*************************************************************
*: Example     : = lfPurgRols ()
*:*************************************************************
*E301816,1 ABD - Add the ability to delete rolls that have zero qty.
FUNCTION lfPurgRols
PARAMETER lnCurFile

*-- Note the Following 
*-- We use laNeedFls[lnCurFile,4] And Increase lnCurFile to open each file we need accourding to 
*-- Alias , we open every table Of our Purge and the equivelant in the history company
*-- for ex. MAtinvjl Source laNeedFls[lnCurFile,4] Target laNeedFls[lnCurFile +1,4]
*-- Fabric   Source laNeedFls[lnCurFile +2,4] Target laNeedFls[lnCurFile + 3,4]
*-- FabDye   Source laNeedFls[lnCurFile +4,4] Target laNeedFls[lnCurFile + 5,4]
*-- Rolls    Source laNeedFls[lnCurFile +6,4] Target laNeedFls[lnCurFile + 7,4]

*-- If we will Purge Master company to a onther company
*-- Else We will purge the purge company.

*- Check if the cost methoed is lot or not , if lot complete the process 
*-- else return
IF lcCstMth = 'L' 
  *-- Check if we Purge the history company or company has a purge company.
  
  IF left(laNeedfls[lnCurFile,5],1) = 'H'
    *-- Create relation bettween MAtinvjl Source File and Rolls source File.
    SELECT (laNeedFls[lnCurFile +3,4])
    SET RELATION TO csession INTO (laNeedFls[lnCurFile,4]) ADDITIVE

    =AFIELDS(laRolls)
    lcTmpFile = gfTempName() && temp name of dbf file to hold Rolls line.
    CREATE TABLE gcWorkDir+(lcTmpFile) FROM ARRAY laRolls
    INDEX ON cRollItem + Color + cWareCode+ Dyelot + cRSession TAG (lcTmpFile) ;
      OF (gcWorkDir+lcTmpFile+'.CDX')
    *-- Get all Rolls with balance equal Zero in a temp. file
    SELECT (laNeedFls[lnCurFile+3,4])
    SCAN FOR nqtybal = 0 .AND. &laNeedFls[lnCurFile,4]..DTRANDATE <= ldRpPrDt 
      SCATTER MEMVAR MEMO
      INSERT INTO (lcTmpFile) FROM MEMVAR
    ENDSCAN
    SELECT (laNeedFls[lnCurFile +3,4])
    SET RELATION TO
    SET ORDER TO Rolapl


    SELECT (laNeedFls[lnCurFile,4])
    SET ORDER TO Matinvjl
    
    SELECT (lcTmpFile)
    *-- We calculate Rolls if sum equal zero for the same seasion number
    *--  & delete from the rolles file.
    SCAN
  
      SELECT (laNeedFls[lnCurFile +3,4])
      IF SEEK(&lcTmpFile..crsession,laNeedFls[lnCurFile +3,4]) && Seek in the Master rolls file.
        SCAN REST WHILE crsession = &lcTmpFile..crsession FOR ;
          crollitem+color+cwarecode+dyelot+crollid = &lcTmpFile..crollitem ;
          +&lcTmpFile..color +&lcTmpFile..cwarecode +&lcTmpFile..dyelot +&lcTmpFile..crollid
          
          
          *--1) Delete Recored from the Material Inv. Journal First.

          SELECT (laNeedFls[lnCurFile,4])
          *-- Seek and scan in the material Inv Journal for the same session.
          *-- Index On cfabric+ccolor+cwarecode+cdyelot+crsession+cisession
          IF SEEK(&laNeedFls[lnCurFile+3,4]..cRollItem+&laNeedFls[lnCurFile+3,4]..Color;
              +&laNeedFls[lnCurFile+3,4]..cWareCode+&laNeedFls[lnCurFile+3,4]..Dyelot+;
              &laNeedFls[lnCurFile+3,4]..Crsession+&laNeedFls[lnCurFile+3,4]..Cisession)
            SCAN REST WHILE Cfabric+Ccolor+Cwarecode+Cdyelot+Crsession+Cisession=;
                      &laNeedFls[lnCurFile+3,4]..cRollItem+&laNeedFls[lnCurFile+3,4]..Color+;
                      &laNeedFls[lnCurFile+3,4]..cWareCode+&laNeedFls[lnCurFile+3,4]..Dyelot+;
                      &laNeedFls[lnCurFile+3,4]..Crsession+&laNeedFls[lnCurFile+3,4]..Cisession
          
              SELECT (laNeedFls[lnCurFile,4])
              =RLOCK()
              DELETE
              UNLOCK
            ENDSCAN
          ENDIF
          
          *--2) Delete recored from the Rolls File.
          SELECT (laNeedFls[lnCurFile +3,4])
          =RLOCK()
          DELETE
          UNLOCK
        ENDSCAN    
      ENDIF
      SELECT (lcTmpFile)
    ENDSCAN
  ELSE
    *-- Create relation bettween MAtinvjl Source File and Rolls source File.
    SELECT (laNeedFls[lnCurFile +6,4])
    SET RELATION TO csession INTO (laNeedFls[lnCurFile,4]) ADDITIVE
     
    =AFIELDS(laRolls)
    lcTmpFile = gfTempName() && temp name of dbf file to hold Rolls line.
    CREATE TABLE gcWorkDir+(lcTmpFile) FROM ARRAY laRolls
    INDEX ON cRollItem + Color + cWareCode+ Dyelot + cRSession TAG (lcTmpFile) ;
      OF (gcWorkDir+lcTmpFile+'.CDX')

    *-- Get all Rolls with balance equal Zero in a temp. file
    SELECT (laNeedFls[lnCurFile +6,4])
    SCAN FOR nqtybal = 0 .AND. &laNeedFls[lnCurFile,4]..DTRANDATE <= ldRpPrDt 
      SCATTER MEMVAR MEMO
      INSERT INTO (lcTmpFile) FROM MEMVAR
    ENDSCAN
    SELECT (laNeedFls[lnCurFile +6,4])
    SET RELATION TO
    SET ORDER TO Rolapl

    SELECT (laNeedFls[lnCurFile,4])
    SET ORDER TO Matinvjl
    SELECT (laNeedFls[lnCurFile+1,4])  
    SET ORDER TO Matinvjl
    
    SELECT (lcTmpFile)
    *-- We calculate Rolls if sum equal zero for the same seasion number
    *-- Purge those records and delete from the rolles file.
    SCAN
      *-- Update the purge company with the fabric color.
      =SEEK(&lcTmpFile..cRollItem+&lcTmpFile..Color,laNeedFls[lnCurFile+2,4])
      SELECT (laNeedFls[lnCurFile+2,4])
      IF !SEEK(Fabric+Color,laNeedFls[lnCurFile +3,4])
        SCATTER MEMVAR MEMO
        SELECT(laNeedFls[lnCurFile+3,4])
        APPEND BLANK
        m.StkVal  = 0
        m.OnHand  = 0
        m.OnOrder = 0
        GATHER MEMVAR MEMO
        
        IF !SEEK(Fabric+Color,laNeedFls[lnCurFile+5,4])
          IF SEEK(Fabric+Color,laNeedFls[lnCurFile+4,4])
            SELECT laNeedFls[lnCurFile+4,4]
            SCAN REST WHILE Fabric+Color+cWareCode+DyeLot = ;
              &laNeedFls[lnCurFile + 2,4]..Fabric + &laNeedFls[lnCurFile + 2,4]..Color
              SCATTER MEMVAR MEMO
              SELECT(laNeedFls[lnCurFile+5,4])
              APPEND BLANK
              m.StkVal  = 0
              m.OnHand  = 0
              m.OnOrder = 0
              m.usage     = 0
              GATHER MEMVAR MEMO
            
            ENDSCAN
          ENDIF  
        ENDIF
      ENDIF  


      *-- We handle ROLLS file here
      *-- If costing method is LOT then move all ROLLS record for the same cTrn_Seq in the MatInvJl
      *-- Scan in the Master Rolls File
  
      SELECT (laNeedFls[lnCurFile +6,4])
      IF SEEK(&lcTmpFile..crsession,laNeedFls[lnCurFile +6,4]) && Seek in the Master rolls file.
        SCAN REST WHILE crsession = &lcTmpFile..crsession FOR ;
          crollitem+color+cwarecode+dyelot+crollid = &lcTmpFile..crollitem ;
          +&lcTmpFile..color +&lcTmpFile..cwarecode +&lcTmpFile..dyelot +&lcTmpFile..crollid
          SCATTER MEMO MEMVAR
          *-- Select the History Rolls file and add the Rolls record.
          SELECT (laNeedFls[lnCurFile+7,4])
          APPEND BLANK
          *-- This variable will update the material inv. Adj.
          GATHER MEMO MEMVAR
          *-- Select the master trolls file and delete the Rolls record.
          SELECT (laNeedFls[lnCurFile+6,4])
          =RLOCK()
          DELETE
          UNLOCK
          SELECT (laNeedFls[lnCurFile +6,4])
        
          *-- Update the Mt Inv Journal.
          SELECT (laNeedFls[lnCurFile,4])
          *-- Seek and scan in the material Inv Journal for the same session.
          *-- Index On cfabric+ccolor+cwarecode+cdyelot+crsession+cisession
          IF SEEK(&laNeedFls[lnCurFile+7,4]..cRollItem+&laNeedFls[lnCurFile+7,4]..Color;
              +&laNeedFls[lnCurFile+7,4]..cWareCode+&laNeedFls[lnCurFile+7,4]..Dyelot+;
              &laNeedFls[lnCurFile+7,4]..Crsession+&laNeedFls[lnCurFile+7,4]..Cisession)
            SCAN REST WHILE Cfabric+Ccolor+Cwarecode+Cdyelot+Crsession+Cisession=;
                      &laNeedFls[lnCurFile+7,4]..cRollItem+&laNeedFls[lnCurFile+7,4]..Color+;
                      &laNeedFls[lnCurFile+7,4]..cWareCode+&laNeedFls[lnCurFile+7,4]..Dyelot+;
                      &laNeedFls[lnCurFile+7,4]..Crsession+&laNeedFls[lnCurFile+7,4]..Cisession
          
              SELECT (laNeedFls[lnCurFile,4])
              SCATTER MEMO MEMVAR
  
              *-- Check if we append this recored before.
              *-- cfabric+ccolor+cwarecode+cdyelot+crsession+cisession
              SELECT (laNeedFls[lnCurFile+1,4])
              IF SEEK(&laNeedFls[lnCurFile+7,4]..cRollItem+&laNeedFls[lnCurFile+7,4]..Color +;
                      &laNeedFls[lnCurFile+7,4]..cWareCode+&laNeedFls[lnCurFile+7,4]..Dyelot+;
                      &laNeedFls[lnCurFile+7,4]..Crsession+&laNeedFls[lnCurFile+7,4]..Cisession);
                      .AND. EMPTY(&laNeedFls[lnCurFile+1,4]..cisession)
                REPLACE nreceived WITH nreceived + &lcTmpFile..nQty ,;
                        NstkVal   WITH nreceived * nUnitCost
              ELSE
                APPEND BLANK
                GATHER MEMO MEMVAR
                *-- In case add Item with type 1 [Reciving session], 
                *-- I will update withe the current value only.
                IF EMPTY(&laNeedFls[lnCurFile+1,4]..cisession)
                  REPLACE nreceived WITH &lcTmpFile..nQty     ,;
                          NstkVal   WITH nreceived* nUnitCost
                ENDIF
              ENDIF
              SELECT (laNeedFls[lnCurFile,4])
            ENDSCAN
          ENDIF
          SELECT (laNeedFls[lnCurFile +6,4])
        ENDSCAN
      ENDIF
      SELECT (lcTmpFile)
    ENDSCAN
  ENDIF

  *--Erase temp file 
  IF USED(lcTmpFile)
    USE IN (lcTmpFile)
  ENDIF
  ERASE (gcWorkDir+lcTmpFile+'.DBF')
  ERASE (gcWorkDir+lcTmpFile+'.CDX')
  
ENDIF



RETURN
*-- End Of lfPurgRols.
*E301816,1 ABD - Add the ability to delete rolls that have zero qty.
*:*************************************************************


*:**************************************************************************
*:* Name        : lpPrePurge
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/02/2006
*:* Purpose     : Define a pre-purge functions to be run for each set of transactions befor purge process
*:***************************************************************************
*:* Called from : lpPrTrn
*:***************************************************************************
*:* Parameters : lcTrn
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
FUNCTION lpPrePurge
PARAMETERS lcTrn

*B607933,1 TMI [Start] Convert the file names to upper case to can be sought correctly in pre preparing functions
FOR lnI = 1 TO ALEN(laNeedFls,1)
  laNeedFls[lnI,2] = UPPER(laNeedFls[lnI,2])
ENDFOR
*B607933,1 TMI [End  ] 

DO CASE
CASE lcTrn = 'Keyed Off Transactions'
  =lfPrepAR()
CASE lcTrn = 'SalesRep Commission'
  =lfPrepSR()
ENDCASE

*-- end of lpPrePurge.

*:**************************************************************************
*:* Name        : lfPrepAR
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 04/13/2006
*:* Purpose     : Prepare AR module by setting the non-purgable records
*:***************************************************************************
*B040155,1
FUNCTION lfPrepAR
PRIVATE lnSlct,lnPos,lcArHst,lcKey,lcDenyPurg
lnSlct = SELECT()

lcArHst = gfTempName()

lnPos = ASUBSCRIPT( laNeedFls , ASCAN( laNeedFls , gcDataDir+'ARHIST' ) , 1 )

*- Open the arhist file again
USE (laNeedFls[lnPos,2]) AGAIN IN 0 ALIAS &lcArHst 
SELECT &lcArHst
INDEX ON ACCOUNT+HISTORY TO (gcWorkDir+lcArHst)

STORE gfTempName() TO lcDenyPurg
CREATE TABLE (gcWorkDir+lcDenyPurg) (ACCOUNT C(5),HISTORY C(6))
INDEX ON ACCOUNT+HISTORY TAG &lcDenyPurg

SELECT (laNeedFls[lnPos,4])
lcSvOrd = ORDER()
SET ORDER TO ARHIST
GO TOP
SCAN FOR TRANDATE<=ldRpPrDt 
  lcKey = ACCOUNT+HISTORY
  =SEEK(lcKey,lcArHst)
  SELECT (lcArHst)
  SCAN REST WHILE ACCOUNT+HISTORY = lcKey
    IF TRANDATE > ldRpPrDt
      INSERT INTO &lcDenyPurg VALUES (&lcArHst..ACCOUNT,&lcArHst..HISTORY) 
    ENDIF
  ENDSCAN
ENDSCAN

SELECT (laNeedFls[lnPos,4])
SET ORDER TO &lcSvOrd

IF RECCOUNT(lcDenyPurg)>0
  laNeedFls[lnPos,8] = laNeedFls[lnPos,8] + ".AND. !SEEK(ACCOUNT+HISTORY,'"+lcDenyPurg+"')"
ENDIF

USE IN &lcArHst

SELECT (lnSlct)
*-- end of lfPrepAR.


*:**************************************************************************
*:* Name        : lfPrepSR
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 04/13/2006
*:* Purpose     : Prepare SR module befor purge process by summarizing data on repcomm file
*:***************************************************************************
*B040155,1
FUNCTION lfPrepSR
PRIVATE lnSlct,lnPos,lcRepSumms,lcKey
lnSlct = SELECT()

IF !USED('SYCCURR')
  =gfOpenFile(gcSysHome+'SYCCURR','CCURRCODE','SH')
ENDIF
=SEEK(gcBaseCurr,'SYCCURR')
lnCurrUnit = SYCCURR.NCURRUNIT

lcRepSumms = gfTempName()

lnPos = ASUBSCRIPT( laNeedFls , ASCAN( laNeedFls , gcDataDir+'REPCOMM' ) , 1 )
SELECT (laNeedFls[lnPos,4])
GO TOP
TOTAL ON REPCODE FOR DATE <= ldRpPrDt TO (gcWorkDir+lcRepSumms) 
SELECT 0 
USE (gcWorkDir+lcRepSumms) 
SCAN FOR AMOUNT<>0
  SCATTER MEMVAR
  SELECT (laNeedFls[lnPos,4])
  APPEND BLANK
  REPLACE REPCODE   WITH M.REPCODE  ;
          AMOUNT    WITH M.AMOUNT   ;
          BALANCE   WITH M.BALANCE  ;
          DATE      WITH ldRpPrDt+1 ;
          DESC      WITH STR(MONTH(ldRpPrDt),2) +'/'+ STR(YEAR(ldRpPrDt),2) + ' purge summary' ;
          TRANTYPE  WITH IIF(M.AMOUNT<0,'3','4')  ;
          STATUS    WITH 'O' ;
          CCURRCODE WITH gcBaseCurr ;
          NCURRUNIT WITH lnCurrUnit ;
          NEXRATE   WITH 1 ;
          NFORAMNT  WITH M.AMOUNT ; 
          CADD_USER WITH gcUser_Id ;
          CADD_TIME WITH TIME()    ;
          DADD_DATE WITH gdSysDate 
ENDSCAN

USE IN &lcRepSumms
ERASE (gcWorkDir+lcRepSumms+'.DBF')

SELECT (lnSlct)
*-- end of lfPrepSR.

*:**************************************************************************
*:* Name        : lfPrepIC
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 04/18/2006
*:* Purpose     : Prepare styinvjl file for purge
*               1)  do not purge lines with related ones in POSHDR,RETHDR,INVHDR
*-              2)  summarize purged lines with new added line with reference "Purge Count"
*               3)  Delete purged lines from styinvjl 
*:***************************************************************************
FUNCTION lfPrepIC

*-- end of lfPrepIC.


*:**************************************************************************
*:* Name        : lfClcAvcst
*:* Developer   : MHM - Mohamed Shokry
*:* Date        : 07/06/2008
*:* Purpose     : Recalcualte Style Cost
*--B608475
*:***************************************************************************
FUNCTION lfClcAvcst

PRIVATE lnlcCost , lnAlias 
*B608475,3 TMI 09/01/2008 [start] define the variable lnlcCost
PRIVATE lcSvDelSt
lcSvDelSt = SET('DELETED')
lnlcCost = 0
lnTotStk = 0
lnAvCost = 0
*B608475,3 TMI 09/01/2008 [end  ] 

lnAlias = SELECT()
SELECT (laNeedFls[lnCurFile,4])

LOCATE
*B608475,3 TMI 09/01/2008 [start] locate the file STYINVJLS
SET DELETED OFF
=SEEK(STYDYE.STYLE+STYDYE.CWARECODE)
*B608475,3 TMI 09/01/2008 [END  ] 
SCAN REST WHILE STYLE+CWARECODE+CSESSION+DTOS(DTRDATE)+CTRCODE+STR(LINENO,6)=;
                          STYDYE.STYLE+STYDYE.CWARECODE;
                    FOR CDYELOT = STYDYE.DYELOT .AND. DTRDATE<=ldRpPrDt 
                    
  *B608475,3 TMI 09/02/2008 [START]
  *lnlcCost = ncost
  lnlcCost = lnlcCost + NSTKVAL
  lnTotStk = lnTotStk + NTOTSTK

  lnAvCost = IIF(lnTotStk=0,nCost,lnlcCost/lnTotStk)
  *B608475,3 TMI 09/02/2008 [END  ]

ENDSCAN
*B608475,3 TMI 09/03/2008 [Start] 
SET DELETED &lcSvDelSt
*B608475,3 TMI 09/03/2008 [End  ] 

SELECT (lnAlias)
RETURN lnAvCost
