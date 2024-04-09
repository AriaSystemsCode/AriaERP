*:****************************************************************
*: Program file  : ICINVDC
*: Program desc. : Generate Inventory Declaration file.
*: System        : Aria Apparel System - Version 2.7.
*: Module        : Inventory Control (IC)
*: Developer     : ABDOU ELGENDI -  (ABD)
*: Date          : 07/15/2003
*: Tracking Job Number : C#200568,1
*:****************************************************************
*: Calls         : G.FUNCTIONS:  gfSetup , gfOpGrid
*:               : FUNCTIONS  :  lfProceed , lfPrt1Part , lfPrt2Part
*:               :            :  lfPrt3Part, lfvFpath   , lfOpenFls
*:               :            :  lfClosFls , lfStySum   , lfsrvSty
*:               :            :  lfFltr_Exp, lfItmPos   , lfGetSkuNo
*:               :            :  lfGetScPos.
*:               -----------------------------------------------
*:               : PROCEDURE  : None.
*:****************************************************************
*: Passed Parameters  : None.
*:****************************************************************
*: Aria Tracking C200735,1.
*: Developer : Ahmed Salah Shalaby - SSH:*
*: Date      : 02/10/2007
*:****************************************************************
*:Modifications  :
*: T20070322.0020 
*: B#200787,1 SSH  Some changes in T20070322.0020 
*: B#608424,1 SSH  some changes as per T20071217.0001
*:****************************************************************
*:Modifications  :
*: T20080504.0001 - add email option    
*: C201050,1 MOS add new realted field to style purchase group called email address (Cmail) 
*: C201051,1 Mos as per T20080504.0001
*: C201103,1 MMT 02/15/2009 Add Declare by Pack option [T20081204.0003]
*: B608831,1 MMT 03/31/2009 fix bug of wrong values in inventory declaration report[T20080504.0001]
*: B608831,2 MMT 04/05/2009 fix bug of wrong values in inventory declaration report Subtotals[T20080504.0001]
*! B609282,1 MMT 06/02/2010 Fix bug of error when user press preview after sending email[T20100426.0001]
*: C201367,1 SAB 07/18/2011 Amend Inventory Declaration report to export report to excel.[T20110601.0001]
*: B609874,1 MMT 04/01/2012 Fix bug of error while sending email [T20120304.0011]
*:****************************************************************
*:
*-- laOpenFile :- Array Hold the Opened Files.
*-- lnSequence :- Sequence lines at the output file.
*-- lcRpPurGrp :- Variable hold the selected Purchasing Group.
*-- lcRpfName  :- Variable hold the File name.
*-- lcRpFPath  :- Variable hold the File Path.
*-- lcRplctext :- Variable hold the input text.
*-- lcLastPlNo :- Variable hold the last and Meximam No. of the PLD Number.
*-- lcDefWare  :- Variable hold the First Default WeareHouse.
*-- lcFile     :- Variable Hold The File Path and name. 
*-- lcChngtext :- Variable Hold The text that will change at the end of the processing.
*-- lnExtScPos :- Variable to hold the Scale Start Pos.
*-- lnExtScLen :- Variable to hold the Scale Len.
*-- lcRPpdf    :- Variable hold the PDF File Path.
*-- lnRunNo    :- Variable hold THE characters 3-5 of the ARF line in the MA3 File
*-- lcdept     :- Variable hold the selected Purchasing Group CODE.
*-- lcattachfile:- Variable hold the Name of pdf file
*-- lcRpMail   :- Variable hold the email address related to selected Purchasing Group 
*!-----------------------------------------------------------------------------------------------

*: C201367,1 SAB 07/18/2011 Amend Inventory Declaration report to export report to excel.[Start]
PRIVATE  llIsExcel
llIsExcel = .T.
*: C201367,1 SAB 07/18/2011 Amend Inventory Declaration report to export report to excel.[End]

IF !USED('SCALE')
 =gfOpenTable('SCALE','SCALE','SH')
ENDIF

IF !USED('stydye')
 =gfOpenTable('stydye','stydye','SH')
ENDIF

IF  EMPTY(lnRepPath)
  = gfModalGen('INM00000B00000','F','ALERT', ' ','You should select a file directory, Can not proceed.')
  RETURN
ENDIF

*!- MOS -ADD EMAIL OPTION AS PER -T20080504.0001-[START]
IF  EMPTY(lcRPpdf)
  *: C201367,1 SAB 07/18/2011 Amend Inventory Declaration report to export report to excel.[Start]
  *= gfModalGen('INM00000B00000','F','ALERT', ' ','You should select a PDF file directory, Can not proceed.')
  = gfModalGen('INM00000B00000','F','ALERT', ' ','You should select a history directory, Can not proceed.')
  *: C201367,1 SAB 07/18/2011 Amend Inventory Declaration report to export report to excel.[End]
  RETURN
ENDIF
*!- MOS -ADD EMAIL OPTION AS PER -T20080504.0001-[END]

*-- Check for selected group.
IF EMPTY(lcRpPurGrp)
  = gfModalGen('INM00000B00000','F','ALERT', ' ','You must select a purchasing group, Can not proceed.')
  RETURN
ENDIF
*: B#608424,1 SSH  some changes as per T20071217.0001

IF EMPTY(lcRpFact)
  WAIT WINDOW 'Factor Code Cannot be Empty!'
  RETURN
ELSE
  lcRpFact=ALLTRIM(lcRpFact)
ENDIF

  *: B609874,1 MMT 04/01/2012 Fix bug of error while sending email [T20120304.0011][Start]
  DIMENSION laOpenFile[8,4]
  STORE 0 TO lnFilHandl , lnO_T_S , lnSequence , lnSumSkAlo , lcLastPlNo ,lcFirstPlNo,;
       lnExtScPos , lnExtScSep , lnExtScLen,cTempStyAls,cTempWrhAls,lnTrailerSeqNo ,lnRunNo

  DIMENSION laMajSegs[1,1]
  STORE '' TO lcRpfName , lcRplctext ,  lcDefWare , lcFile , lcChngtext

*-- Open needed files 
=lfOpenFls() 
lcWorkFile=gfTempName()
  DO lfCreateTemp
  loogScroll.cCROrientation = 'P'
  
  llSelectusers = .F.
  lnPosusers  = ASCAN(loOgScroll.laOgFXFlt,"SYUUSER.CUSER_ID")
 IF lnPosusers > 0 
  lnPosusers  = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosusers ,1)
  lcusersSel =IIF(!EMPTY(laOgFxFlt[lnPosusers,6]),laOgFxFlt[lnPosusers,6],'')
  IF !EMPTY(lcusersSel ) AND USED(lcusersSel )
    SELECT(lcusersSel)
    LOCATE
     IF  !EOF()
       llSelectusers =.T.
    ENDIF 
   ENDIF  
 ENDIF 
 USERLIST =''
 IF llSelectusers =.T.
   SELECT (lcusersSel) 
    SCAN 
     SELECT SYUUSER  
       IF gfseek(&lcusersSel..CUSER_ID,'SYUUSER','CUSER_ID' )
         SCAN REST WHILE CUSER_ID = &lcusersSel..CUSER_ID
           USERLIST = ALLTRIM(SYUUSER.CEMAIL_ADD)+';'+ USERLIST        
         ENDSCAN 
       ENDIF 
    ENDSCAN  
 ENDIF 
  lcdept =ALLTRIM(gfCodDes(lcRpPurGrp,'CPURCODE'))
   *SAB[Start]
   *lcattachfile =lcRpFact+lcRppurgrp+lcRpfName
   lcattachfile =lcRpFact+lcRppurgrp+lcRpfName+DTOS(DATE())+CHRTRAN(LEFT(TIME(),5),':','')
  *: B609874,1 MMT 04/01/2012 Fix bug of error while sending email [T20120304.0011][END]


*: B#608424,1 SSH  some changes as per T20071217.0001

*!- MOS -ADD EMAIL OPTION AS PER -T20080504.0001-[START]
IF  !(TYPE('_screen.ActiveForm.Name') = 'C' AND _screen.ActiveForm.Name = "sendmail")


  *: B609874,1 MMT 04/01/2012 Fix bug of error while sending email [T20120304.0011][Start]
*!*	  DIMENSION laOpenFile[8,4]
*!*	  STORE 0 TO lnFilHandl , lnO_T_S , lnSequence , lnSumSkAlo , lcLastPlNo ,lcFirstPlNo,;
*!*	       lnExtScPos , lnExtScSep , lnExtScLen,cTempStyAls,cTempWrhAls,lnTrailerSeqNo ,lnRunNo

*!*	  DIMENSION laMajSegs[1,1]
*!*	  STORE '' TO lcRpfName , lcRplctext ,  lcDefWare , lcFile , lcChngtext

*!*	*-- Open needed files 
*!*	=lfOpenFls() 
*!*	lcWorkFile=gfTempName()
*!*	  DO lfCreateTemp
*!*	  loogScroll.cCROrientation = 'P'
*!*	  
*!*	  llSelectusers = .F.
*!*	  lnPosusers  = ASCAN(loOgScroll.laOgFXFlt,"SYUUSER.CUSER_ID")
*!*	 IF lnPosusers > 0 
*!*	  lnPosusers  = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosusers ,1)
*!*	  lcusersSel =IIF(!EMPTY(laOgFxFlt[lnPosusers,6]),laOgFxFlt[lnPosusers,6],'')
*!*	  IF !EMPTY(lcusersSel ) AND USED(lcusersSel )
*!*	    SELECT(lcusersSel)
*!*	    LOCATE
*!*	     IF  !EOF()
*!*	       llSelectusers =.T.
*!*	    ENDIF 
*!*	   ENDIF  
*!*	 ENDIF 
*!*	 USERLIST =''
*!*	 IF llSelectusers =.T.
*!*	   SELECT (lcusersSel) 
*!*	    SCAN 
*!*	     SELECT SYUUSER  
*!*	       IF gfseek(&lcusersSel..CUSER_ID,'SYUUSER','CUSER_ID' )
*!*	         SCAN REST WHILE CUSER_ID = &lcusersSel..CUSER_ID
*!*	           USERLIST = ALLTRIM(SYUUSER.CEMAIL_ADD)+';'+ USERLIST        
*!*	         ENDSCAN 
*!*	       ENDIF 
*!*	    ENDSCAN  
*!*	 ENDIF 
*!*	  lcdept =ALLTRIM(gfCodDes(lcRpPurGrp,'CPURCODE'))
*!*	   *SAB[Start]
*!*	   *lcattachfile =lcRpFact+lcRppurgrp+lcRpfName
*!*	   lcattachfile =lcRpFact+lcRppurgrp+lcRpfName+DTOS(DATE())+CHRTRAN(LEFT(TIME(),5),':','')
  *: B609874,1 MMT 04/01/2012 Fix bug of error while sending email [T20120304.0011][END]

  
   loOGScroll.llDontSaveLocalCopy = .T.
  
   *SAB[End]
   loOGScroll.lcToEmail = lcRpMail
   loOGScroll.lcCcEmail= USERLIST 
   *SAB[Start]
   *loOGScroll.lcEmailAttachmment= lcRPpdf + lcattachfile
   loOGScroll.lcEmailAttachmment= ADDBS(lcRPpdf) + lcattachfile
   *SAB[End]
   loOGScroll.lcEmailSubject= 'Declaration For Dept '+lcdept+''+'-'+''+'Series NO '+lcRpFact   
   lfmail()
   *! B609282,1 MMT 06/02/2010 Fix bug of error when user press preview after sending email[Start]
   IF USED('SYUUSER')
     USE IN 'SYUUSER'
   ENDIF
   =gfOpenFile(gcsyshome + "SYUUSER" , "CUSER_ID" , 'SH')
   *! B609282,1 MMT 06/02/2010 Fix bug of error when user press preview after sending email[End]
    RETURN 
ENDIF  

*!- MOS -ADD EMAIL OPTION AS PER -T20080504.0001-[START]
lnCurrPos   = lfItmPos("WAREHOUS.CWARECODE")
IF lnCurrPos>0
  cTempWrhAls =  laOGFxFlt[lnCurrPos,6]
  IF !EMPTY(cTempWrhAls) AND USED(cTempWrhAls) AND RECCOUNT(cTempWrhAls)>0
    SELECT * FROM (cTempWrhAls) INTO CURSOR cSelWarHs
  ENDIF
ELSE
  cTempWrhAls =  ""
ENDIF



lcExpr = lfGetFilterExp()

llTermColl = .F.
SELECT RepStyle
LOCATE FOR &lcExpr AND lfVldStyWhs(RepStyle.Style)
IF EOF()
  llTermColl = .T.
  *---Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
ENDIF
IF !llTermColl 
  lnTrailerSeqNo=0
  *-- Funtion to collect the data into text file.
  IF !EMPTY(cTempWrhAls) AND USED(cTempWrhAls) AND RECCOUNT(cTempWrhAls)>0
      PRIVATE lcWareCode
      lcWareCode = CWARECODE 
      lcLastPlNo = 0
      lcFirstPlNo = ""
      = lfProceed (lcRpFact)
      = lfClosCretedFils() 
  ELSE
    lcLastPlNo = 0
    lcFirstPlNo = ""
      = lfProceed (lcRpFact)
      = lfClosCretedFils()
   ENDIF
ENDIF

*! B609282,1 MMT 06/02/2010 Fix bug of error when user press preview after sending email[Start]
IF USED('SYUUSER')
  USE IN 'SYUUSER'
ENDIF
=gfOpenFile(gcsyshome + "SYUUSER" , "CUSER_ID" , 'SH')
*! B609282,1 MMT 06/02/2010 Fix bug of error when user press preview after sending email[End]


*-- Close opened files 
*= lfClosFls ()
IF llTermColl
  RETURN
ENDIF

*-- END of Code.
*:*************************************************************
*: Name      : lfProceed
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 07/15/2003
*: Purpose   : Funtion to collect the data into text file.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfProceed ()
*:*************************************************************
FUNCTION lfProceed
LPARAMETERS cpWareCod
lcFileSeq = gfsequence('CSEQUENMA3','','','')
*- Abdou Change 'S-' To be '$I'
*lcRpfName = 'S-'+RIGHT(lcRpPurGrp,3)+'_'+RIGHT(lcFileSeq,2)
PRIVATE lcCrntGrpD
lcCrntGrpD = LEFT(ALLTRIM(gfCodDes(lcRpPurGrp,'CPURCODE')),3)
lcRpfName = ALLTRIM('$I'+lcCrntGrpD+'_'+RIGHT(lcFileSeq,2))
*- Abdou
lcFile = ALLTRIM(lnRepPath)+ALLTRIM(lcRpfName)+'.MA3'
lnFilHandl = FCREAT(lcFile)

IF lnFilHandl < 0
  *- Message Text   :- File can not be create. can not proceed.
  *- Message No.    :- 000000.
  *- Buttom Message :- Ok
  *- Buttom Number  :- 00000.
  = gfModalGen('INM00000B00000','F','ALERT',' ','File can not be create. can not proceed.')
  RETURN
ENDIF
 
*-- Function to get the Scale Pos. 
= lfGetScPos()

*-- Function to Get the 1 part.

= lfPrt1Part (cpWareCod)

*-- Function to Get the 2 part.
= lfPrt2Part (cpWareCod)

*-- Function to Get the 3 part.

= lfPrt3Part ()
DIMENSION laMajSegs[1,1]
		= gfItemMask(@laMajSegs)
		*-- Loop Around Non Major elements.
		STORE '' TO lcNonMajPi 
		FOR lnI =  1 TO ALEN(laMajSegs,1)
		  IF laMajSegs[lnI,1] = 'C'
		    lnNonMajSt = laMajSegs[lnI,4]      && This item hold seg. start position.
		    lcNonMajPi = IIF(EMPTY(lcNonMajPi) .OR. laMajSegs[lnI,1]='C',;
		                 laMajSegs[lnI,3],;
		                 lcNonMajPi + laMajSegs[lnI-1,6] + laMajSegs[lnI,3])
		    lcNonMajPos = laMajSegs[lnI,4]                 
		    EXIT
		  ENDIF
		ENDFOR
		STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
IF USED(cTempStyAls)
SELECT(cTempStyAls)
  SCAN
	SELECT RepStyle
	gfseek(ALLTRIM(&cTempStyAls..CSTYMAJOR),'RepStyle','STYLE')
  
  *: B608831,1 MMT 03/31/2009 fix bug of wrong values in inventory declaration report[Start]
*!*  	SCAN REST WHILE STYLE = ALLTRIM(&cTempStyAls..CSTYMAJOR) FOR ;
*!*  	gfseek('S'+ RepStyle.scale,'SCALE','SCALE') AND gfseek(RepStyle.style,'stydye','stydye')
  SCAN REST WHILE STYLE = ALLTRIM(&cTempStyAls..CSTYMAJOR) FOR ;
  gfseek('S'+ RepStyle.scale,'SCALE','SCALE') AND gfseek(RepStyle.style+lcRpFact,'stydye','stydye')
  *: B608831,1 MMT 03/31/2009 fix bug of wrong values in inventory declaration report[End]
  
   *: C201103,1 MMT Add Declare by Pack option [Start]
   IF llRpPack AND !gfSEEK('P'+RepStyle.Style,'RepSPCKLNST','SPCKLNST')
     LOOP 
   ENDIF 
   *: C201103,1 MMT Add Declare by Pack option [End]
  
		m.cstymajor=RepStyle.Cstymajor
		m.stk1=stydye.stk1-stydye.alo1
		m.stk2=stydye.stk2-stydye.alo2
		m.stk3=stydye.stk3-stydye.alo3
		m.stk4=stydye.stk4-stydye.alo4
		m.stk5=stydye.stk5-stydye.alo5
		m.stk6=stydye.stk6-stydye.alo6
		m.stk7=stydye.stk7-stydye.alo7
		m.stk8=stydye.stk8-stydye.alo8
		m.totstk=stydye.totstk-stydye.totalo
		m.sz1=SCALE.sz1
		m.sz2=SCALE.sz2
		m.sz3=SCALE.sz3
		m.sz4=SCALE.sz4
		m.sz5=SCALE.sz5
		m.sz6=SCALE.sz6
		m.sz7=SCALE.sz7
		m.sz8=SCALE.sz8	
	    m.colorco=gfCodDes(SUBSTR(RepStyle.STYLE,lcNonMajPos,lnColorLen),'COLOR')	
	    *: C201367,1 SAB 07/18/2011 Amend Inventory Declaration report to allow export it to excel.[Start]
	    * colorco contain Colore Description and cdiscrep contain Color Code
	    m.cdiscrep = SUBSTR(RepStyle.STYLE,lcNonMajPos,lnColorLen)
	    *: C201367,1 SAB 07/18/2011 Amend Inventory Declaration report to allow export it to excel.[End]
	    m.scale = RepStyle.scale  
	    m.desc1 = RepStyle.desc1
	  INSERT INTO (lcWorkFile) FROM MEMVAR
	ENDSCAN 
  ENDSCAN 
ELSE 
 SELECT RepStyle
  gfseek("")
  *: B608831,2 MMT 04/05/2009 fix bug of wrong values in inventory declaration report Subtotals[Start]
*!*    SCAN FOR RepStyle.cpurcode ='&lcRppurgrp.' AND RepStyle.cdefware = '&lcrpfact.' AND  ;
*!*     gfseek('S'+ RepStyle.scale,'SCALE','SCALE')and; 
*!*     gfseek(RepStyle.style,'stydye','stydye')
  SCAN FOR RepStyle.cpurcode =lcRppurgrp AND RepStyle.cdefware = lcrpfact AND  ;
   gfseek('S'+ RepStyle.scale,'SCALE','SCALE')and; 
   gfseek(RepStyle.style+ lcrpfact ,'stydye','stydye')
  *: B608831,2 MMT 04/05/2009 fix bug of wrong values in inventory declaration report Subtotals[End]   
   
   *: C201103,1 MMT Add Declare by Pack option [Start]
   IF llRpPack AND !gfSEEK('P'+RepStyle.Style,'RepSPCKLNST','SPCKLNST')
     LOOP 
   ENDIF 
   *: C201103,1 MMT Add Declare by Pack option [End]
   
   
    m.cstymajor=RepStyle.Cstymajor
		m.stk1=stydye.stk1-stydye.alo1
		m.stk2=stydye.stk2-stydye.alo2
		m.stk3=stydye.stk3-stydye.alo3
		m.stk4=stydye.stk4-stydye.alo4
		m.stk5=stydye.stk5-stydye.alo5
		m.stk6=stydye.stk6-stydye.alo6
		m.stk7=stydye.stk7-stydye.alo7
		m.stk8=stydye.stk8-stydye.alo8
		m.totstk=stydye.totstk-stydye.totalo
		m.sz1=SCALE.sz1
		m.sz2=SCALE.sz2
		m.sz3=SCALE.sz3
		m.sz4=SCALE.sz4
		m.sz5=SCALE.sz5
		m.sz6=SCALE.sz6
		m.sz7=SCALE.sz7
		m.sz8=SCALE.sz8	
	    m.colorco=gfCodDes(SUBSTR(RepStyle.STYLE,lcNonMajPos,lnColorLen),'COLOR')
	    *: C201367,1 SAB 07/18/2011 Amend Inventory Declaration report to allow export it to excel.[Start]
	    * colorco contain Colore Description and cdiscrep contain Color Code
	    m.cdiscrep = SUBSTR(RepStyle.STYLE,lcNonMajPos,lnColorLen)
	    *: C201367,1 SAB 07/18/2011 Amend Inventory Declaration report to allow export it to excel.[End]
        m.scale = RepStyle.scale 
	    m.desc1 = RepStyle.desc1
	  INSERT INTO (lcWorkFile) FROM MEMVAR
  ENDSCAN 
ENDIF 


SELECT (lcWorkFile)
LOCATE 
counter = 0
lnRunNo=SUBSTR(PADR(ALLTRIM(lcRpFact),2)+SUBSTR(lcFirstPlNo,LEN(lcFirstPlNo)-2),3,5)  
oAriaApplication.gcDevice =  'FILE'

*: C201367,1 SAB 07/18/2011 Amend Inventory Declaration report to allow export it to excel.[Start]
*loogscroll.cTextRepType = 'PDF'
*oAriaApplication.gcoutfile = lcRPpdf + lcattachfile + '.pdf'
*DO gfDispRe WITH EVALUATE('lcFormName')

llIsExcel = loogscroll.cTextRepType = "EXCEL"

IF llIsExcel
  *loogscroll.cTextRepType = "EXCEL"
  *SAB[Start]
  *oAriaApplication.gcoutfile = lcRPpdf + lcattachfile + '.XLS'
  oAriaApplication.gcoutfile = ADDBS(lcRPpdf) + lcattachfile + '.XLS'
  *SAB[End]
  loogscroll.lcOGLastForm = 'ICINVDCX'
  loogscroll.lcLogoPath = ""
  lfAdjustCRSettings()
  IF USED(lcWorkFile)
    USE IN (lcWorkFile)
  ENDIF
  loogscroll.mpreparecrystalreport()
  loogscroll.oReport.ExportOptions.DestinationType = 1 &&crEDTDiskFile
  loogscroll.oReport.ExportOptions.FormatType = 27 && crEFTExcel80	 
  lcStr = STRTRAN(oAriaApplication.gcOutFile,'"','')
  lcStr = STRTRAN(lcStr,'-','')
  lcStr = STRTRAN(lcStr,'*','')
  lcStr = STRTRAN(lcStr,'?','')
  lcStr = STRTRAN(lcStr,'|','')
  lcStr = STRTRAN(lcStr,'>','')
  lcStr = STRTRAN(lcStr,'<','')
  oAriaApplication.gcOutFile = lcStr  
  loogscroll.oReport.ExportOptions.DiskFileName = oAriaApplication.gcOutFile
  loogscroll.oReport.VerifyOnEveryPrint = .T.
  loogscroll.oReport.Export(.F.) 
ELSE 
  loogscroll.cTextRepType = 'PDF'
  *SAB[Start]
  *oAriaApplication.gcoutfile = lcRPpdf + lcattachfile + '.pdf'
  oAriaApplication.gcoutfile = ADDBS(lcRPpdf) + lcattachfile + '.pdf'
  *SAB[End]
  loogscroll.lcOGLastForm = 'ICINVDC'
  DO gfDispRe WITH EVALUATE('lcFormName')  
ENDIF
*: C201367,1 SAB 07/18/2011 Amend Inventory Declaration report to allow export it to excel.[End]

*!- MOS -ADD EMAIL OPTION AS PER -T20080504.0001-[START]
*: C201367,1 SAB 07/18/2011 Amend Inventory Declaration report to allow export it to excel.[Start]
*lcMessage2 = 'Process completed successfully, file '+lcRpFact+lcRppurgrp+lcRpfName+'.pdf'+' is saved on directory '+lcRPpdf+'.'
IF llIsExcel
  lcMessage2 = 'Process completed successfully, file '+lcRpFact+lcRppurgrp+lcRpfName+'.xls'+' is saved on directory '+lcRPpdf+'.'
ELSE 
  lcMessage2 = 'Process completed successfully, file '+lcRpFact+lcRppurgrp+lcRpfName+'.pdf'+' is saved on directory '+lcRPpdf+'.'
ENDIF
*: C201367,1 SAB 07/18/2011 Amend Inventory Declaration report to allow export it to excel.[End]
=gfModalGen('INM00000B00000','F','ALERT',' ',lcMessage2)
*!- MOS -ADD EMAIL OPTION AS PER -T20080504.0001-[END]
*-- End of lfProceed
*:*************************************************************
*: Name      : lfPrt1Part
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 07/15/2003
*: Purpose   : Funtion to get the hedaer part.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : = lfPrt1Part ()
*:*************************************************************
*:
FUNCTION lfPrt1Part
LPARAMETER cpWrCod

*ssh
DECLARE laVRltFld[1,2]
STORE '' TO lcAvilbil2 , lcSysTime , lcSysDate , lcAna

*-- R e c o r d   1 --*
laVRltFld[1,1] = 'CAVILABIL2'
laVRltFld[1,2] = 'lcAvilbil2'

*laVRltFld[2,1] = 'CANA'
*laVRltFld[2,2] = 'lcAna'
STORE '' TO lcAna , lcAvilbil2

=gfRltFld(lcRpPurGrp,@laVRltFld,'CPURCODE')

DECLARE laSetups[1,2]
*: B#200787,1 SSH  Change the supplier and insted print ETOS ANA
laSetups[1,1]  = 'M_SUPMLCD '
*laSetups[1,1]  = 'M_ETOSANA'
*: B#200787,1 SSH  Change the supplier and insted print ETOS ANA
=gfGetMemVar(@laSetups,oAriaApplication.ActiveCompanyID)
lcAna = laSetups[1,2]

lcSysTime = gfGetTime()
lcSysTime = LEFT(STRTRAN(lcSysTime,':',''),6)

lcSysDate = STRTRAN(DTOC(oAriaApplication.SystemDate),'/','')
lcSysDate = RIGHT(lcSysDate,2)+SUBSTR(lcSysDate,3,2)+LEFT(lcSysDate,2)
*lcGrpDesc = LEFT(ALLTRIM(gfCodDes(lcRpPurGrp,'CPURCODE')),4)
*: B#200787,1 SSH  remove that last space
*lcGrpDesc = "N"+LEFT(ALLTRIM(gfCodDes(lcRpPurGrp,'CPURCODE')),3)+""
lcGrpDesc = "N"+LEFT(ALLTRIM(gfCodDes(lcRpPurGrp,'CPURCODE')),3)+" "
*: B#200787,1 SSH  remove that last space
lcCmpEANLoc = PADR(gfGetMemVar('M_CMPEANLC' , oAriaApplication.ActiveCompanyId),13)
lcWareCode  = "0"+SUBSTR(ALLTRIM(lcRpFact),3,4)

*: B#200787,1 SSH  get mailBox from warehouse file
lnTrailerSeqNo=lnTrailerSeqNo+1
*lcWareCode  = lcWareCode &&+ lfGetWarHMailBox(cpWrCod)
*: B#200787,1 SSH  get mailBox from warehouse file

* First Row
*LCRPLCTEXT = 'X'+'5012068035505 '+'AVLHDR        '+'N'+LCGRPDESC+padr(lcana,5)+substr(LCFILESEQ,5,6)+'  '+LCSYSDATE+LCSYSTIME+SPACE(45)

LCRPLCTEXT = 'STX=ANA:1+'+lcCmpEANLoc+':'+ALLTRIM(PADR(oAriaApplication.activecompanyname,40))+"+"+ALLTRIM(lcAna)+"+"

*: B#200787,1 SSH  add lcRpPassWord and trailer seq no (lnTrailerSeqNo) to the STX line
*LCRPLCTEXT = LCRPLCTEXT +lcSysDate+[:]+lcSysTime+[+]+lcGrpDesc+lcWareCode+SUBSTR(lcFileSeq,5,6)+[+AVLHDR]+[+B']
LCRPLCTEXT = LCRPLCTEXT +lcSysDate+[:]+lcSysTime+[+]+lcGrpDesc+lcWareCode+SUBSTR(lcFileSeq,5,6)+"+"+lcRpPassWord+[+AVLHDR]+[+B']
*: B#200787,1 SSH  add lcRpPassWord and trailer seq no (lnTrailerSeqNo) to the STX line

*'AVLHDR        '+'N'+LCGRPDESC+padr(lcana,5)+substr(LCFILESEQ,5,6)+'  '+LCSYSDATE+LCSYSTIME+SPACE(45)
=FPUTS(lnFilHandl,lcRplctext)

*-- R e c o r d   2 --*
lcRplctext = 'MHD=1+'+'AVLHDR'+":3'"

=FPUTS(lnFilHandl,lcRplctext)

*-- R e c o r d   3 --*
lcRplctext = 'TYP=2000+' + "AVLHDR'"

=FPUTS(lnFilHandl,lcRplctext)

*-- R e c o r d   4 --*
lcGrpDesc = SUBSTR(ALLTRIM(gfCodDes(lcRpPurGrp,'CPURCODE')),1,3)+" "

lcRplctext = 'SDT=:'+lcGrpDesc+lcWareCode+"'"
=FPUTS(lnFilHandl,lcRplctext)

*-- R e c o r d   5 --*
lcRplctext = "CDT=:4151'"
=FPUTS(lnFilHandl,lcRplctext)


*-- R e c o r d   6 --*
lcRplctext = 'FIL=1+1+'+lcSysDate +"'"&& + 'XX' +'XXX   '+ lcSysDate + ' ' + lcSysDate
*lcChngtext = lcRplctext
=FPUTS(lnFilHandl,lcRplctext)

*-- R e c o r d   7 --*
lcRplctext = "MTR=6'"
*lcChngtext = lcRplctext
=FPUTS(lnFilHandl,lcRplctext)

*-- End OF lfPrt1Part
*:*************************************************************
*: Name      : lfPrt2Part
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 07/15/2003
*: Purpose   : Funtion to get the middle part.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : = lfPrt2Part ()
*:*************************************************************

FUNCTION lfPrt2Part
LPARAMETERS cpWrCode
PRIVATE lcSku_No , lCurStkVal

lcSysTime = gfGetTime()
lcSysTime = LEFT(STRTRAN(lcSysTime,':',''),6)

lcSysDate = STRTRAN(DTOC(oAriaApplication.SystemDate),'/','')
lcSysDate = RIGHT(lcSysDate,2)+SUBSTR(lcSysDate,3,2)+LEFT(lcSysDate,2)

STORE 0 TO  lnSequence
lnSequence = 0

lcExpr1 = lfGetFilterExp(.T.)

SELECT RepStyle
*SCAN FOR &lcExpr
lcRplctext = [MHD=2+]+[AVLDET:3']
=FPUTS(lnFilHandl,lcRplctext)
*SSH
*!*	ACTIVATE WINDOW trace
*!*	susp
*: B#608424,1 SSH  some changes as per T20071217.0001
*SCAN FOR CPURCODE $ lcRpPurGrp  &&&lcExpr1
lcFirstPlNo="000"
SCAN FOR CPURCODE $ lcRpPurGrp  
*!*	  IF cstymajor='7154'
*!*	    ACTIVATE wind trace
*!*	    susp
*!*	    susp
*!*	  ENDIF
  IF lfAddCheck(RepStyle.Style) &&&lcExpr1
*: B#608424,1 SSH  some changes as per T20071217.0001
*: B#608424,1 SSH  some changes as per T20071217.0001
    IF VAL(lcFirstPlNo)<VAL(RepStyle.CLASTPLDNO)
    *IF (VAL(lcFirstPlNo)<VAL(SUBSTR(ALLTRIM(lcFirstPlNo),LEN(ALLTRIM(lcFirstPlNo))-2)))
*: B#608424,1 SSH  some changes as per T20071217.0001
      lcFirstPlNo=ALLTRIM(RepStyle.CLASTPLDNO)
    ENDIF
    *lcFirstPlNo = IIF(EMPTY(lcFirstPlNo),ALLTRIM(RepStyle.CLASTPLDNO),lcFirstPlNo)
  ENDIF
ENDSCAN
lcFirstPlNo=SUBSTR(lcFirstPlNo,LEN(lcFirstPlNo)-2)
*: B#608424,1 SSH  some changes as per T20071217.0001
*lcRplctext = [ARF=] + PADR(cpWrCode,2)+PADR(lcFirstPlNo,3)+ [+] +lcSysDate+ [+] +lcSysDate+[']
lcRplctext = [ARF=] + PADR(ALLTRIM(lcRpFact),2)+SUBSTR(lcFirstPlNo,LEN(lcFirstPlNo)-2)+ [+] +lcSysDate+ [+] +lcSysDate+[']
*: B#608424,1 SSH  some changes as per T20071217.0001
=FPUTS(lnFilHandl,lcRplctext)



SCAN FOR &lcExpr AND lfVldStyWhs(RepStyle.Style)

  WAIT WINDOW 'Style # :' + Style NOWAIT
  *lnSumSkAlo = 0
  *-- Variable hold the last and Meximam No. of the PLD Number.
  lcLastPlNo = MAX(MAX(lcLastPlNo,VAL(RepStyle.CLASTPLDNO)),0)

  *-- Variable hold the First Default WeareHouse.
  IF EMPTY(lcDefWare)
    lcDefWare   = ALLTRIM(SUBSTR(RepStyle.cDefware,3,4))
  ENDIF

  *IF SEEK('S'+Scale,'SCALE')
  *: C201103,1 MMT Add Declare by Pack option [Start]
  IF llRpPack
    IF gfSEEK('P'+RepStyle.Style,'RepSPCKLNST','SPCKLNST')

      STORE 0 TO lnPck1,lnPck2,lnPck3,lnPck4,lnPck5,lnPck6,lnPck7,lnPck8
      lnPck1 = RepSPCKLNST.qtY1
      lnPck2 = RepSPCKLNST.Qty2
      lnPck3 = RepSPCKLNST.qty3
      lnPck4 = RepSPCKLNST.Qty4
      lnPck5 = RepSPCKLNST.Qty5
      lnPck6 = RepSPCKLNST.Qty6
      lnPck7 = RepSPCKLNST.Qty7
      lnPck8 = RepSPCKLNST.Qty8
      lnTotPack = 0
      
      STORE 0 TO lnStk1,lnStk2,lnStk8,lnstk3,lnstk4,lnstk5,lnstk6,lnstk7
      
      IF !EMPTY(cpWrCode)
        SELECT RepStyDye
        LOCATE FOR Style = RepStyle.Style AND cwareCode = cpWrCode
        IF FOUND()
          FOR lnA = 1 TO 8
            lcA = STR(lnA,1)
            lnStk&lcA  =  Stk&lcA. - Alo&lcA.
          ENDFOR 
        ENDIF
      ELSE
        SELECT RepStyle
        FOR lnA = 1 TO 8
            lcA = STR(lnA,1)
            lnStk&lcA  =  Stk&lcA. - Alo&lcA.
        ENDFOR 
      ENDIF   
  		 
  		DO WHILE (lnStk1 >= lnPck1 AND lnStk2 >= lnPck2 AND lnStk3 >= lnPck3 AND lnStk4 >= lnPck4 AND lnStk5 >= lnPck5 AND lnStk6 >= lnPck6 AND lnStk7 >= lnPck7 AND lnStk8 >= lnPck8 ) 
  		  FOR ln=1 to 8
  		    lc= STR(ln,1)
  		    lnStk&lc = lnStk&lc - lnPck&lc
  		  ENDFOR 
  		  lnTotPack = lnTotPack + 1
      ENDDO  
      IF lnTotPack > 0
        LNSEQUENCE = LNSEQUENCE+1
        lcRplctext = [AVD=]+ALLTRIM(STR(lnSequence,4))+[+0+:]+ALLTRIM(RepSPCKLNST.PAck_iD)+ [+++1+]+ALLTRIM(STR(lnTotPack ,10))+[']
        =FPUTS(lnFilHandl,lcRplctext)
      ENDIF  
    ELSE
      LOOP 
    ENDIF 
  ELSE
  *: C201103,1 MMT Add Declare by Pack option [End]
  IF gfSeek('S'+RepStyle.Scale,'RepScale')
    FOR lnStart = 1 TO RepScale.Cnt
      lcCurSize = STR(lnStart,1)
      *ash comment this line as per tony's email on 11/07/03
      *lnSequence = lnSequence + 1
      *-- type+style+account+pack_id
      lcSku_No   = lfGetSkuNo (lnStart)
      IF !EMPTY(cpWrCode)
        SELECT RepStyDye
        LOCATE FOR Style = RepStyle.Style AND cwareCode = cpWrCode
        lnStkAlo   = RepStyDye.Stk&lcCurSize - RepStyDye.Alo&lcCurSize
      ELSE
        lnStkAlo   = RepStyle.Stk&lcCurSize - RepStyle.Alo&lcCurSize
      ENDIF
      *ash Don't include the -ve stock size as per tony's email on 09/30/03
      IF lnStkAlo < 0
        LOOP
      ENDIF
      lnSumSkAlo = lnSumSkAlo + lnStkAlo
      *ash Add the if statement as per Tony's email on 11/07/03
      IF LCSKU_NO>' ' 
        LNSEQUENCE = LNSEQUENCE+1
        lcRplctext = [AVD=]+ALLTRIM(STR(lnSequence,4))+[+0+:]+PADR(lcSku_No,8)+ [+++1+]+ALLTRIM(STR(lnStkAlo,10))+[']
        =FPUTS(lnFilHandl,lcRplctext)
      ENDIF
    ENDFOR  
  ENDIF
  *: C201103,1 MMT Add Declare by Pack option [Start]
  ENDIF
  *: C201103,1 MMT Add Declare by Pack option [End]
ENDSCAN

lcRplctext = [ATR=]+ALLTRIM(STR(lnSequence,4))+[+]+ALLTRIM(STR(lnSumSkAlo,10))+[']
=FPUTS(lnFilHandl,lcRplctext)

lcRplctext = [MTR=]+ALLTRIM(STR(lnSequence+4,4))+[']
=FPUTS(lnFilHandl,lcRplctext)
WAIT CLEAR
*-- End OF lfPrt2Part
*:*************************************************************
*: Name      : lfPrt3Part
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 07/15/2003
*: Purpose   : Funtion to get the bottom part.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : = lfPrt3Part ()
*:*************************************************************
*:
FUNCTION lfPrt3Part

*-- R e c o r d   1 --*
lcRplctext = [MHD=3+AVLTLR:1']
=FPUTS(lnFilHandl,lcRplctext)

*-- R e c o r d   2 --*
lcRplctext = [AFT=1']
=FPUTS(lnFilHandl,lcRplctext)

lcRplctext = [MTR=3']
=FPUTS(lnFilHandl,lcRplctext)

lcRplctext = [END=3']
=FPUTS(lnFilHandl,lcRplctext)



*!- MOS -ADD EMAIL OPTION AS PER -T20080504.0001-[START]


*-- End OF lfPrt3Part
*:*************************************************************
*: Name      : lfvFpath
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 07/15/2003
*: Purpose   : Funtion to get the fil path.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfvFpath()
*:*************************************************************
*:
FUNCTION lfvFpath
PRIVATE lcoldPath
lcoldPath = FULLPATH('')
*!- MOS -ADD EMAIL OPTION AS PER -T20080504.0001-[START]
IF lnRepPath = '?' 
lnRepPath =  GETDIR("","","")
ENDIF 
*!- MOS -ADD EMAIL OPTION AS PER -T20080504.0001-[END]
SET DEFA TO &lcoldPath
*-- End of lfvFpath


*-- End OF lfPrt3Part
*:*************************************************************
*: Name      : lfvFpath2
*: Developer : Mostafa Eid - (Mos)
*: Date      : 08/26/2008
*: Purpose   : Funtion to get the fil path.
*:*************************************lcoldPath************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfvFpath2()
*:*************************************************************

FUNCTION lfvFpath2
PRIVATE lcoldPath2
lcoldPath = FULLPATH('')
*!- MOS -ADD EMAIL OPTION AS PER -T20080504.0001-[START]
IF lcRPpdf = '?' 
lcRPpdf =  GETDIR("","","")
ENDIF
*!- MOS -ADD EMAIL OPTION AS PER -T20080504.0001-[END]
SET DEFA TO &lcoldPath2
*-- End of lfvFpath2

*:*************************************************************
*: Name      : lfOpenFls
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 07/15/2003
*: Purpose   : Function to open needed files.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfOpenFls ()
*:*************************************************************
*:
FUNCTION lfOpenFls

*--- Array laOpenFile column 1 :- Hold the name of the file.
*--- Array laOpenFile column 2 :- Hold the name of the index file.
*--- Array laOpenFile column 3 :- Hold true in case open the file .

laOpenFile[1,1] = 'STYLE'
laOpenFile[1,2] = 'STYLE'
laOpenFile[1,4] = 'RepStyle'

laOpenFile[2,1] = 'SCALE'
laOpenFile[2,2] = 'SCALE'
laOpenFile[2,4] = 'RepScale'

laOpenFile[3,1] = 'SPCK_HDR'
laOpenFile[3,2] = 'SKU_STYLE'
laOpenFile[3,4] = 'RepSKU_Style'

laOpenFile[4,1] = 'SPCK_LIN'
laOpenFile[4,2] = 'SPCKLNST'
laOpenFile[4,4] = 'RepSPCKLNST'

laOpenFile[5,1] = 'StyDye'
laOpenFile[5,2] = 'StyDye'
laOpenFile[5,4] = 'RepStyDye'

*: B#608424,1 SSH  some changes as per T20071217.0001
laOpenFile[6,1] = 'OrdLine'
laOpenFile[6,2] = 'OrdLine'
laOpenFile[6,4] = 'RepOrdLine'

laOpenFile[7,1] = 'InvLine'
laOpenFile[7,2] = 'InvLine'
laOpenFile[7,4] = 'RepInvLine'

laOpenFile[8,1] = 'PikTkt'
laOpenFile[8,2] = 'PikTkt'
laOpenFile[8,4] = 'RepPikTkt'

*: B#608424,1 SSH  some changes as per T20071217.0001
FOR I = 1 To ALEN(laOpenFile,1)
  IF !USED(laOpenFile[I,4])
    laOpenFile[I,3] = gfOpenTable(gcDataDir+laOpenFile[I,1],laOpenFile[I,2], "SH",laOpenFile[I,4])
  ENDIF
ENDFOR

*-- End of lfOpenFls.

*:**************************************************************************
*: Name      : lfStySum
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 07/15/2003
*: Purpose   : sum a specific field for the current style in style file
*:**************************************************************************
*: Calls     : 
*:**************************************************************************
*: Called from : Option Grid,style browse calculated fields.
*:**************************************************************************
*: Passed Parameters  : lcSty,lccomp,lnAddToVar
*:**************************************************************************
*: Returns            : Calculated field value.
*:**************************************************************************
*: Example   : =lfStySum()
*:**************************************************************************
* 
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar
PRIVATE lnStyRec
lnTotcomp = 0

IF RECCOUNT('STYLE') != 0
  lnStyRec = RECNO('STYLE')
  SELECT Style_X
  *SUM &lcCOMP TO lnTotcomp WHILE Style = ALLTRIM(lcSty)  
  SUM &lcCOMP TO lnTotcomp WHILE ALLTRIM(cStyMajor) == ALLTRIM(lcSty)
  SELECT Style
  IF BETWEEN(lnStyRec,1,RECCOUNT())
    GO lnStyRec
  ENDIF  

  DO CASE
    CASE lnAddToVar = 1
  	  lnRpO_T_S = lnTotcomp
    CASE lnAddToVar = 2
      lnRpO_T_S = lnRpO_T_S + lnTotcomp
    CASE lnAddToVar = 3
      lnRpO_T_S = lnRpO_T_S - lnTotcomp
  ENDCASE
ENDIF  
RETURN INT(lnTotcomp)
*-- End of lfStySum.
*:**************************************************************************
*: Name      : lfsrvSty
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 07/15/2003
*: Purpose   : Rise change style flag, in range browse screen.
*:**************************************************************************
*: Calls     : 
*:**************************************************************************
*: Called from : Option Grid
*:**************************************************************************
*: Passed Parameters  : lcParm
*:**************************************************************************
*: Returns            : None
*:**************************************************************************
*: Example   : =lfsrvSty()
*:**************************************************************************
*: Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*:**************************************************************************
* 
FUNCTION lfSRVSty
PARAMETERS lcParm

DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to Style Major 
    *-- unique index.
    USE (gcDataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
    SELECT STYLE  
    IF !EMPTY(lcRpPurcod)
      SET FILTER TO &lcRpPurcod
    ENDIF
    SET ORDER TO TAG Cstyle
    SET RELATION TO STYLE.STYLE INTO STYLE_X
    GO TOP IN STYLE
  CASE lcParm = 'R'  && Reset code
    USE IN STYLE_X
    SELECT STYLE
    IF !EMPTY(lcRpPurcod)
      SET FILTER TO
    ENDIF
    SET ORDER TO TAG STYLE
ENDCASE

*-- End of lfsrvSty.
*:*************************************************************
*: Name      : lfFltr_Exp
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 07/15/2003
*: Purpose   : Create filter expration.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Called from : Report code
*:*************************************************************
*: Passed Parameters  : ...
*:*************************************************************
*: Returns            : None.
*:*************************************************************
*: Example   : = lfFltr_Exp()
*:*************************************************************

FUNCTION lfFltr_Exp
PRIVATE lnCurrPos
lnCurrPos  = lfItmPos('STYLE.CPURCODE')
lcRpPurGrp   = laOGFxFlt[lnCurrPos,6]
IF EMPTY(lcRpPurGrp)
  lcRpPurcod = ''
ELSE
  lcRpPurcod = "STYLE.CPURCODE $ lcRpPurGrp"
ENDIF
*!- MOS -ADD EMAIL OPTION AS PER -T20080504.0001-[START]
STORE '' TO lcRpMail
DECLARE laTermAry[1,2]
laTermAry[1,1] = 'CMAIL'
laTermAry[1,2] = 'lcRpMail'  
=gfRltFld(lcRpPurGrp,@laTermAry,'CPURCODE')
*!- MOS -ADD EMAIL OPTION AS PER -T20080504.0001-[END]

*-- End OF lfFltr_Exp
*:*************************************************************
*: Name      : lfItmPos
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 07/15/2003
*: Purpose   : Evaluate fixed filter position within array.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Called from : Report code
*:*************************************************************
*: Passed Parameters  : Option Grid field.
*:*************************************************************
*: Returns            : Position
*:*************************************************************
*: Example   : = lfItmPos()
*:*************************************************************
*
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos

*-- End of lfItmPos.
*:*************************************************************
*: Name      : lfGetSkuNo
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 07/15/2003
*: Purpose   : get the Sku no for current Style/Size
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Called from : Program code
*:*************************************************************
*: Passed Parameters  : lnSizePar ==> Style size number.
*:*************************************************************
*: Returns            : SKU#
*:*************************************************************
*: Example   : = lfGetSkuNo()
*:*************************************************************
*
FUNCTION lfGetSkuNo
PARAMETER lnSizePar
PRIVATE lnPrvAlis , lcRtnSkuNo , lnDime1 , lcStyle

lcRtnSkuNo = ''
lnPrvAlis  = SELECT(0)

*-- Check if the style has SKU# or Not.

IF gfSeek('S'+RepStyle.style,'RepSPCKLNST')
*IF SEEK('S'+Style.style,'RepSPCKLNST')
  lcStyle    = IIF(lnExtScPos = 0,RepStyle.style,LEFT(RepStyle.style,LEN(RepStyle.style)-lnExtScLen))
  *-- Check for this account and get the first record for only first account.
  *IF SEEK('S'+Spck_lin.Account+lcStyle,'SPCK_HDR')
  IF gfSeek('S'+RepSPCKLNST.Account+lcStyle,'RepSKU_Style')
    SELECT RepSPCKLNST
    lnDime1    = LEN(ALLTRIM(RepSKU_Style.pack_id))
    lcCurSkuNo = LEFT(ALLTRIM(RepSKU_Style.SKU),lnDime1)
    SCAN REST WHILE type+style+account+pack_id = 'S'+RepStyle.Style+RepSKU_Style.Account;
      FOR LEFT(Pack_Id,lnDime1) = lcCurSkuNo
      *-- if the current record is the curreent size assign it and then get out.
      IF EVALUATE('RepSPCKLNST.Qty'+ALLTRIM(STR(lnSizePar))) = 1
         lcRtnSkuNo = SUBSTR(ALLTRIM(Pack_id),lnDime1+1)
         EXIT
      ENDIF
    ENDSCAN
  ENDIF
ENDIF

SELECT(lnPrvAlis)
RETUR lcRtnSkuNo
*-- End OF lfGetSkuNo
*:*************************************************************
*: Name      : lfGetScPos
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 07/15/2003
*: Purpose   : Function to get rhe Scale Position.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Called from : Program code
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns            : None.
*:*************************************************************
*: Example   : = lfGetScPos()
*:*************************************************************
*
FUNCTION lfGetScPos

= gfItemMask(@laMajSegs)
FOR lnI = 1 TO ALEN(laMajSegs,1)
  IF laMajSegs[lnI,1] = 'S'
    lnExtScPos = IIF(lnExtScPos=0,laMajSegs[lnI,4],lnExtScPos)
    lnExtScSep = IIF(lnExtScSep=0,LEN(ALLTRIM(laMajSegs[lnI,6])),lnExtScLen)    
    lnExtScLen = IIF(lnExtScLen=0,LEN(laMajSegs[lnI,3]),lnExtScLen) + lnExtScSep
  ENDIF
ENDFOR

*-- End OF lfGetScPos
*:*************************************************************

*:*************************************************************
*: Name      : lfwRepWhen
*: Developer : Ahmed Salah -  (SSH)
*: Date      : 07/15/2003
*: Purpose   : Report When
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Called from : Program code
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns            : None.
*:*************************************************************
*: Example   : = lfwRepWhen()
*:*************************************************************

FUNCTION lfwRepWhen
*!- MOS -ADD EMAIL OPTION AS PER -T20080504.0001-[START]
IF !USED('SYUUSER')
 =gfOpenFile(gcsyshome + "SYUUSER" , "CUSER_ID" , 'SH')
ENDIF
*!- MOS -ADD EMAIL OPTION AS PER -T20080504.0001-[END]
PRIVATE lnOldAls
lnOldAls = SELECT(0)
USE (oAriaApplication.defaultpath+"Ftp_Info") SHARED AGAIN IN 0
SELECT Ftp_Info
GO TOP
lnRepPath = ALLTRIM(cOutgoing)
*!- MOS -ADD EMAIL OPTION AS PER -T20080504.0001-[START]
lcRPpdf = ALLTRIM(Chistory)
*!- MOS -ADD EMAIL OPTION AS PER -T20080504.0001-[END]

*: B#200787,1 SSH  Get the password from FTP_Info file
lcRpPassWord = PADL(cPassWord,8)
*: B#200787,1 SSH  Get the password from FTP_Info file

USE IN Ftp_Info

*!- MOS -ADD EMAIL OPTION AS PER -T20080504.0001-[START]
loOGScroll.EnableObject("LCRPMAIL",.F.)

*!- MOS -ADD EMAIL OPTION AS PER -T20080504.0001-[END]

SELECT (lnOldAls)
*:*************************************************************
*: Name      : RefreshTransaction
*: Developer : Ahmed Salah -  (SSH)
*: Date      : 07/15/2003
*: Purpose   : Called from PAth record in SYREPUVR
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Called from : Program code
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns            : None.
*:*************************************************************
*: Example   : = RefreshTransaction()
*:*************************************************************
*
FUNCTION RefreshTransaction
lnRepPath = IIF(TYPE("lnRepPath")="C",lnRepPath,"")
RETURN lnRepPath 

*!- MOS -ADD EMAIL OPTION AS PER -T20080504.0001-[START]
FUNCTION RefreshTransact2 
lcRPpdf = IIF(TYPE("lcRPpdf")="C",lcRPpdf,"")
RETURN lcRPpdf
*!- MOS -ADD EMAIL OPTION AS PER -T20080504.0001-[END]
*:*************************************************************
*: Name      : lfGetFilterExp
*: Developer : Ahmed Salah -  (SSH)
*: Date      : 07/15/2003
*: Purpose   : Return Filter EXP.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Called from : Program code
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns            : None.
*:*************************************************************
*
FUNCTION lfGetFilterExp
LPARAMETERS llDontIncludeWareHouse
PRIVATE lnOldAls,lc2Return
lc2Return = ".T."
lnCurrPos   = lfItmPos("STYLE.CSTYMAJOR ")
IF lnCurrPos>0
  cTempStyAls =  laOGFxFlt[lnCurrPos,6]
ELSE
  cTempStyAls =  ""
ENDIF

IF !EMPTY(cTempStyAls) AND USED(cTempStyAls) AND RECCOUNT(cTempStyAls)>0
  lc2Return = lc2Return+" .AND. "+"lfVldStyFlt(cStyMajor)"
ENDIF
IF !EMPTY(lcRpPurGrp)
  lc2Return = lc2Return+" .AND. "+"RepStyle.CPURCODE $ lcRpPurGrp"
ENDIF

IF !llDontIncludeWareHouse AND !EMPTY(cTempWrhAls) AND USED(cTempWrhAls) AND RECCOUNT(cTempWrhAls)>0 
  lc2Return = lc2Return+" .AND. "+"RepStyle.CwareCode = lcRpFact"
ENDIF

*: C201103,1 MMT Add Declare by Pack option [Start]
IF llRpPack
  lc2Return = lc2Return + " AND gfSEEK('P'+Style,'RepSPCKLNST','SPCKLNST')"
ENDIF 
*: C201103,1 MMT Add Declare by Pack option [End]


RETURN lc2Return

lnOldAls = SELECT(0)

*:*************************************************************
*: Name      : lfVldStyFlt
*: Developer : Ahmed Salah -  (SSH)
*: Date      : 07/15/2003
*: Purpose   : Valid Style in case report by style.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Called from : Program code
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns            : None.
*:*************************************************************
*: Example   : = lfVldStyFlt()
*:*************************************************************
*
FUNCTION lfVldStyFlt
LPARAMETERS cpSty
PRIVATE lnOldAls,ll2Ret
lnOldAls = SELECT(0)
SELECT (cTempStyAls)
LOCATE FOR cStyMajor=cpSty
ll2Ret = FOUND()
SELECT(lnOldAls)
RETURN(ll2Ret)

**************************************************************
FUNCTION lfClosCretedFils
**************************************************************

*-- this loop to close the file, the program should close this file under any condation.
IF !(lnFilHandl < 0)
  DO WHILE !FCLOSE(lnFilHandl)
      
  ENDDO
ENDIF
*-- Update the Global Variables.
CREATE CURSOR lcTmpCur (cTempMemo M(10))
APPEND BLANK
APPEND MEMO cTempMemo FROM  (lcFile)
lcTempMemo = cTempMemo
*-- this line to update the Style.cdefware and Maximam PLD Number.
lcLastPlNo = RIGHT(ALLTRIM(STR(lcLastPlNo)),3)
*ash changes as per Tony's email on 01/15/2004
lcLastPlNo = IIF(LEN(lcLastPlNo)<3,PADL(lcLastPlNo,3,'0'),lcLastPlNo )
lcSysDate  = STRTRAN(DTOC(oAriaApplication.SystemDate),'/','')
*ash changed to YYMMDD as per Tony email on 09/30/03
*lcSysDate  = LEFT(lcSysDate,4)+RIGHT(lcSysDate,2)
lcSysDate  = RIGHT(lcSysDate,2)+SUBSTR(lcSysDate,3,2)+LEFT(lcSysDate,2)
lcTextRepl = 'ARF ' + lcDefWare + lcLastPlNo + '   '+ lcSysDate + ' ' + lcSysDate
lcTempMemo = STRTRAN(lcTempMemo,lcChngtext,lcTextRepl)
REPLACE cTempMemo WITH lcTempMemo
COPY MEMO cTempMemo To (lcFile)
*- Message Text   :- Process complete successfully.
*- Message No.    :- 000000.
*- Buttom Message :- Ok
*- Buttom Number  :- 00000.
lcMessage = 'Process completed successfully, file '+lcRpfName +'.MA3 is saved on directory '+lnRepPath+'.'
=gfModalGen('INM00000B00000','F','ALERT',' ',lcMessage)
****************************************
FUNCTION lfVldStyWhs
LPARAMETERS cpStyle
****************************************

PRIVATE llFOUND,cOldAls
cOldAls = SELECT(0)
llFOUND = .F.
*!*	SELECT cSelWarHs
*!*	GO TOP
*!*	DO WHILE !EOF() AND !llFOUND

SELECT RepStyDye
LOCATE FOR Style = cpStyle AND CwareCode = lcRpFact
llFOUND = FOUND()

*!*	ENDDO
SELECT(cOldAls)
RETURN(llFOUND)

****************************
FUNCTION lfGetWarHMailBox
LPARAMETERS lcWareCode
****************************
PRIVATE lnOldAls

lnOldAls=SELECT(0)
IF gfSeek(lcWareCode,'WAREHOUS')
  lc2Return = WAREHOUS.cSupBrcd
ENDIF
SELECT(lnOldAls)
RETURN (lc2Return)

*******************
FUNCTION lfAddCheck
LPARAMETERS cStyle
*****************
PRIVATE lc2Return,lnOldAls,llcontinuo
lnOldAls=SELECT(0)
lc2Return=.F.
llcontinuo=.T.
SELECT RepStyDye
LOCATE FOR Style=cStyle AND cwarecode=ALLTRIM(lcRpFact)

IF FOUND()
  SELECT RepOrdLine
  
  SCAN FOR style+DTOS(complete)+cordtype+order+store+STR(lineno,6)= cStyle AND !EMPTY(PikTkt) AND llcontinuo AND ;
           cwarecode=lcRpFact
    SCATTER MEMVAR MEMO
    SELECT RepPikTkt
    LOCATE FOR PikTkt=m.PikTkt AND cwarecode=lcRpFact
    IF FOUND() AND RepPikTkt.Status="O"
      llcontinuo = .F.
      lc2Return=.T.
    ENDIF
  ENDSCAN
  IF llcontinuo
    SELECT RepInvLine
    LOCATE FOR style+invoice+STR(lineno,6)=cStyle AND cwarecode=lcRpFact
    IF FOUND()
      lc2Return=.T.
    ENDIF
  ENDIF
ELSE
  lc2Return=.F.
ENDIF
SELECT (lnOldAls)
RETURN(lc2Return)
*: B#608424,1 SSH  some changes as per T20071217.0001

FUNCTION lfVFactor
*: B#608424,1 SSH  some changes as per T20071217.0001
IF !gfSeek(PADR(ALLTRIM(lcRpFact),6),'WAREHOUS')
  lcRpFact = gfBrowWare(.T., .F., .F., lcRpFact, .F., 'S')
ENDIF

*!*************************************************************
*! Name      : lfCreateTemp
*! Developer : Mostafa Eid [MOS]
*! Date      : 05/29/2008
*! Purpose   : Function to create temp.file 
*!*************************************************************

FUNCTION lfCreateTemp
DIMENSION laStruArr[22, 4]

laStruArr[1  ,1] = 'Cstymajor'
laStruArr[1  ,2] = 'C'
laStruArr[1  ,3] = 19
laStruArr[1  ,4] = 0

laStruArr[2  ,1] = 'colorco'
laStruArr[2  ,2] = 'C'
laStruArr[2  ,3] = 20
laStruArr[2  ,4] = 0

laStruArr[3  ,1] = 'stk1'
laStruArr[3  ,2] = 'N'
laStruArr[3  ,3] = 7
laStruArr[3  ,4] = 0

laStruArr[4  ,1] = 'stk2'
laStruArr[4  ,2] = 'N'
laStruArr[4  ,3] = 7
laStruArr[4  ,4] = 0

laStruArr[5  ,1] = 'stk3'
laStruArr[5  ,2] = 'N'
laStruArr[5  ,3] = 7
laStruArr[5  ,4] = 0

laStruArr[6  ,1] = 'stk4'
laStruArr[6  ,2] = 'N'
laStruArr[6  ,3] = 7
laStruArr[6  ,4] = 0

laStruArr[7  ,1] = 'stk5'
laStruArr[7 , 2] = 'N'
laStruArr[7  ,3] = 7
laStruArr[7  ,4] = 0

laStruArr[8  ,1] = 'stk6'
laStruArr[8  ,2] = 'N'
laStruArr[8  ,3] = 7
laStruArr[8  ,4] = 0

laStruArr[9  ,1] = 'stk7'
laStruArr[9  ,2] = 'N'
laStruArr[9  ,3] = 7
laStruArr[9  ,4] = 0

laStruArr[10 ,1] = 'stk8'
laStruArr[10 ,2] = 'N'
laStruArr[10 ,3] = 7
laStruArr[10 ,4] = 0

laStruArr[11 ,1] = 'totstk'
laStruArr[11 ,2] = 'N'
laStruArr[11 ,3] = 7
laStruArr[11 ,4] = 0

laStruArr[12 ,1] = 'SZ1'
laStruArr[12 ,2] = 'C'
laStruArr[12 ,3] = 5
laStruArr[12 ,4] = 0

laStruArr[13 ,1] = 'SZ2'
laStruArr[13 ,2] = 'C'
laStruArr[13 ,3] = 5
laStruArr[13 ,4] = 0

laStruArr[14 ,1] = 'SZ3'
laStruArr[14 ,2] = 'C'
laStruArr[14 ,3] = 5
laStruArr[14 ,4] = 0

laStruArr[15 ,1] = 'SZ4'
laStruArr[15 ,2] = 'C'
laStruArr[15 ,3] = 5
laStruArr[15 ,4] = 0

laStruArr[16 ,1] = 'SZ5'
laStruArr[16 ,2] = 'C'
laStruArr[16 ,3] = 5
laStruArr[16 ,4] = 0

laStruArr[17 ,1] = 'SZ6'
laStruArr[17 ,2] = 'C'
laStruArr[17 ,3] = 5
laStruArr[17 ,4] = 0

laStruArr[18 ,1] = 'SZ7'
laStruArr[18 ,2] = 'C'
laStruArr[18 ,3] = 5
laStruArr[18 ,4] = 0

laStruArr[19 ,1] = 'SZ8'
laStruArr[19 ,2] = 'C'
laStruArr[19 ,3] = 5
laStruArr[19 ,4] = 0

laStruArr[20 ,1] = 'scale'
laStruArr[20 ,2] = 'C'
laStruArr[20 ,3] = 5
laStruArr[20 ,4] = 0

laStruArr[21 ,1] = 'desc1'
laStruArr[21 ,2] = 'C'
laStruArr[21 ,3] = 20
laStruArr[21 ,4] = 0

*: C201367,1 SAB 07/18/2011 Amend Inventory Declaration report to allow export it to excel.[Start]
laStruArr[22 ,1] = 'cdiscrep'
laStruArr[22 ,2] = 'C'
laStruArr[22 ,3] = 30
laStruArr[22 ,4] = 0

*MMT
*=gfCrtTmp(lcWorkFile,@laStruArr,'Cstymajor',lcWorkFile,.T.)
IF llIsExcel
  =gfCrtTmp(lcWorkFile,@laStruArr,'Cstymajor',lcWorkFile,.F.)
ELSE 
  =gfCrtTmp(lcWorkFile,@laStruArr,'Cstymajor',lcWorkFile,.T.)
ENDIF 
*MMT
*: C201367,1 SAB 07/18/2011 Amend Inventory Declaration report to allow export it to excel.[End]




FUNCTION lfcount() 
counter =0
FUNCTION lfinc()
counter =counter + 1

FUNCTION lfmail
LOCAL oEmail as Form, lcClassLibrary
lcClassLibrary = oAriaApplication.ClassDir + "ogmail.vcx"
oEmail = NEWOBJECT("sendmail",lcClassLibrary,"",loogscroll)
IF (VARTYPE(oEmail) != "O") OR ISNULL(oEmail)
  RETURN .F.
ENDIF 
*: C201367,1 SAB 07/18/2011 Amend Inventory Declaration report to allow export it to excel.[Start]
*MMT
IF llIsExcel
  * Set Default Output File
  oEmail.cboOutputFormat.Value = 2
  oEmail.cboOutputFormat.valid()
ENDIF
*MMT
*: C201367,1 SAB 07/18/2011 Amend Inventory Declaration report to allow export it to excel.[End]
oEmail.Show()
oEmail = .NULL.
RETURN .T.

FUNCTION flagchk       
llflag = .F.
RETURN llflag 


*: C201367,1 SAB 07/18/2011 Amend Inventory Declaration report to allow export it to excel.[Start]
*************************************************************
*! Name      : lfAdjustCRSettings
*! Developer : Saber Saber(SAB)
*! Date      : 07/20/2011
*! Purpose   : To set the report data files and parameters
*!*************************************************************
FUNCTION lfAdjustCRSettings

DIMENSION loOgScroll.laCRTables[1]

DIMENSION loOgScroll.laCRParams[3,2]

loOGScroll.cCROrientation='P'

loOgScroll.laCRTables[1] = oAriaApplication.WorkDir +  lcWorkFile + ".DBF"

loOgScroll.laCRParams[1,1] = 'SysDate'
loOgScroll.laCRParams[1,2] = oAriaApplication.SYSTEMDATE

loOgScroll.laCRParams[2,1] = 'FactoryCode'
loOgScroll.laCRParams[2,2] = lcRpFact

loOgScroll.laCRParams[3,1] = 'Dept'
loOgScroll.laCRParams[3,2] = LEFT(lcDept,4)

ENDFUNC 
*: C201367,1 SAB 07/18/2011 Amend Inventory Declaration report to allow export it to excel.[End]

