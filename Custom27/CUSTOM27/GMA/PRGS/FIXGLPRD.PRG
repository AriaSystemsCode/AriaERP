*:**************************************************************************
*: Program file  : FIXGLPRD.PRG
*: Program desc. : Fix Program
*: System        : Aria 2.7
*: Developer     : MOHAMED SHOKRY (MHM)
*: Date          : 12/03/2003
*: Reference     : B120267
*:**************************************************************************
PRIVATE lcPath , lcCurPath , lcInvNo , lcVendCode
lcPath = SET("DEFAULT")

lcCurPath = GETDIR("","Select the company directory")
IF ALLTRIM(lcCurPath) == "" 
  WAIT WINDOW "Invalid diectory! Program will be teminated"  NOWAIT 
  RETURN
ENDIF
SET DEFAULT TO lcCurPath+"\"

USE ApDist IN 0 ORDER TAG InvVend
USE GlDist IN 0 ORDER TAG Gldistac

USE GlBatch IN 0 ORDER TAG Batchno

USE GlTrnsdt IN 0 ORDER TAG Batchtrn
USE GlTrnshd IN 0 ORDER TAG Batchtrn

USE GlPtrnhd IN 0 ORDER TAG Batchtrn
USE GlPtrndt IN 0 ORDER TAG Batchtrn 

USE FISHD IN 0 ORDER TAG COMPFYEAR
USE FSPRD IN 0 ORDER TAG COMFYRPRDI
USE GLACBALS IN 0 ORDER TAG ACCYRPRD
USE GLACCHAR IN 0 ORDER TAG ACCTCODE 
USE APPAYMNT IN 0 
USE ARCUSHST IN 0 
USE APVENHST IN 0 
USE ICSTYHST IN 0 
*--use syccomp
lcCurPath1 = LEFT(lcCurPath,LEN(lcCurPath)-8)  +"sysfiles\SYCCOMP"
USE &lcCurPath1  IN 0

*--Update GLDist
SELECT GLDist
SCAN 
  *-- if not in the required date then loop
  IF Tran_Date < {01,01,2001}
    LOOP
  ENDIF

  llGLTRNSHD = .F.
  llGLTRNSDT = .F.
  llGLPTRNHD = .F.
  llGLPTRNDT = .F.
  llGLBATCH  = .F.
  
  WAIT WINDOW "Updating GLDist "  NOWAIT 

  *---Bucket 2000 From {01/01/2001 To 12/31/2001}
  IF BETWEEN(Tran_Date,{01,01,2001},{12,31,2001})
    lcYear = "2000"
  ENDIF

  *---Bucket 2001 From {01/01/2002 To 03/31/2002}
  *-- change period to the new period accourding to closing
  IF BETWEEN(Tran_Date,{01,01,2002},{03,31,2002})
    lcYear = "2001"
  ENDIF
    
  *---Bucket 2002 From {04/01/2002 To 03/31/2003}
  *-- change period to the new period accourding to closing
  IF BETWEEN(Tran_Date,{04,01,2002},{03,31,2003})
    lcYear = "2002"
  ENDIF
      
  *---Bucket 2003 From {04/01/2003 To 03/31/2004}
  *-- change period to the new period accourding to closing
  IF BETWEEN(Tran_Date,{04,01,2003},{03,31,2004})
    lcYear = "2003"
  ENDIF

  *---Bucket 2004 From {04/01/2004 To 03/31/2005}
  *-- change period to the new period accourding to closing
  IF BETWEEN(Tran_Date,{04,01,2004},{03,31,2005})
    lcYear = "2004"
  ENDIF
  
  IF posted ='X'
    IF SEEK(GLDist.glbatch,'GLTRNSHD')
      llGLTRNSHD = .T.
      IF SEEK(GLDist.glbatch,'GLTRNSDT')
        llGLTRNSDT = .T.
      ENDIF
    ENDIF
    
    IF SEEK(GLDist.glbatch,'GLPTRNHD')
      llGLPTRNHD = .T.
      IF SEEK(GLDist.glbatch,'GLPTRNDT')
        llGLPTRNDT = .T.
      ENDIF
    ENDIF

    IF SEEK(GLDist.glbatch,'GLBATCH')
      llGLBATCH = .T.
    ENDIF
  ENDIF
  
  Do CASE MONTH(GLDist.Tran_Date)
 
     CASE MONTH(GLDist.Tran_Date) = 1
		   IF GLDist.Tran_Date < {04,01,2002} 
		      lcPeriod = '01'
		   ELSE
		      lcPeriod = '10'
		   ENDIF

     CASE MONTH(GLDist.Tran_Date) = 2
		   IF GLDist.Tran_Date < {04,01,2002} 
		     lcPeriod = '02'
		   ELSE
		     lcPeriod = '11'
		   ENDIF  
		   		    
     CASE MONTH(GLDist.Tran_Date) = 3
		   IF GLDist.Tran_Date < {04,01,2002} 
		     lcPeriod = '03'
		   ELSE
		     lcPeriod = '12'
		   ENDIF  
		   		   
     CASE MONTH(GLDist.Tran_Date) = 4
		   IF GLDist.Tran_Date < {04,01,2002} 
		     lcPeriod = '04'
		   ELSE
		     lcPeriod = '01'
		   ENDIF  
		   		   
     CASE MONTH(GLDist.Tran_Date) = 5
		   IF GLDist.Tran_Date < {04,01,2002} 
		     lcPeriod = '05'
		   ELSE
		     lcPeriod = '02'
		   ENDIF  
		   		    
     CASE MONTH(GLDist.Tran_Date) = 6
		   IF GLDist.Tran_Date < {04,01,2002} 
		     lcPeriod = '06'
		   ELSE
		     lcPeriod = '03'
		   ENDIF  
		   		    
     CASE MONTH(GLDist.Tran_Date) = 7
		   IF GLDist.Tran_Date < {04,01,2002} 
		     lcPeriod = '07'
		   ELSE
		     lcPeriod = '04'
		   ENDIF
		   		    
     CASE MONTH(GLDist.Tran_Date) = 8
		   IF GLDist.Tran_Date < {04,01,2002} 
		     lcPeriod = '08'
		   ELSE
		     lcPeriod = '05'
		   ENDIF  
		   		    
     CASE MONTH(GLDist.Tran_Date) = 9
		   IF GLDist.Tran_Date < {04,01,2002} 
		     lcPeriod = '09'
		   ELSE
		     lcPeriod = '06'
		   ENDIF  
		   		    
     CASE MONTH(GLDist.Tran_Date) = 10
		   IF GLDist.Tran_Date < {04,01,2002} 
		     lcPeriod = '10' 
		   ELSE
		     lcPeriod = '07'
		   ENDIF  
		   		    
     CASE MONTH(GLDist.Tran_Date) = 11
		   IF GLDist.Tran_Date < {04,01,2002} 
		     lcPeriod = '11'
		   ELSE
		     lcPeriod = '08'
		   ENDIF  
		   		    
     CASE MONTH(GLDist.Tran_Date) = 12
		   IF GLDist.Tran_Date < {04,01,2002} 
		     lcPeriod = '12'
		   ELSE
		     lcPeriod = '09'
		   ENDIF  
    ENDCASE
    
    REPLACE GLDist.glperiod WITH lcPeriod;
	        GLDist.glfyear  WITH lcYear

    *--Update GLTRNSHD
	IF llGLTRNSHD
  	  REPLACE GLTRNSHD.ctrnpprd WITH lcPeriod;
              GLTRNSHD.ctrnpyr  WITH lcYear
	ENDIF

	*--Update GLTRNSDT 
    IF llGLTRNSDT 
  	  REPLACE GLTRNSDT.ctrnpprd WITH lcPeriod;
	          GLTRNSDT.ctrnpyr  WITH lcYear
    ENDIF
		   		   
	*--Update GLPTRNHD 
	IF llGLPTRNHD
  	   REPLACE GLPTRNHD.ctrnpprd WITH lcPeriod;
	           GLPTRNHD.ctrnpyr  WITH lcYear
	ENDIF
		   		   
	*--Update GLPTRNDT
	IF llGLPTRNDT
  	   REPLACE GLPTRNDT.ctrnpprd WITH lcPeriod;
	           GLPTRNDT.ctrnpyr  WITH lcYear
    ENDIF

	*--Update GLBATCH
	IF llGLBATCH
  	   REPLACE GLBATCH.cbatpyr  WITH lcYear
	ENDIF

  *---Bucket 2001 From {01/01/2002 To 03/31/2001}

ENDSCAN


*--Update APDist
SELECT APDist
SCAN 

  WAIT WINDOW "Updating APDist "  NOWAIT 
  *-- if not in the required date then loop
  IF dApDtrdat < {01,01,2001}
    LOOP
  ENDIF

  llGLTRNSHD = .F.
  llGlTRNSDT = .F.
  llGlPTRNHD = .F.
  llGlPTRNDT = .F.
  llGlBATCH  = .F.
  
  *---Bucket 2000 From {01/01/2001 To 12/31/2001}
  IF BETWEEN(dApDtrdat,{01,01,2001},{12,31,2001})
    lcYear = "2000"
  ENDIF

  *---Bucket 2001 From {01/01/2002 To 03/31/2001}
  *-- change period to the new period accourding to closing
  IF BETWEEN(dApDtrdat,{01,01,2002},{03,31,2001})
    lcYear = "2001"
  ENDIF
    
  *---Bucket 2002 From {04/01/2002 To 03/31/2003}
  *-- change period to the new period accourding to closing
  IF BETWEEN(dApDtrdat,{04,01,2002},{03,31,2003})
    lcYear = "2002"
  ENDIF
      
  *---Bucket 2003 From {04/01/2003 To 03/31/2004}
  *-- change period to the new period accourding to closing
  IF BETWEEN(dApDtrdat,{04,01,2003},{03,31,2004})
    lcYear = "2003"
  ENDIF

  *---Bucket 2004 From {04/01/2004 To 03/31/2005}
  *-- change period to the new period accourding to closing
  IF BETWEEN(dApDtrdat,{04,01,2004},{03,31,2005})
    lcYear = "2004"
  ENDIF
          
    IF lapdpost

      IF SEEK(APDist.cbatchno,'GLTRNSHD')
        llGLTRNSHD = .T.
        IF SEEK(APDist.cbatchno,'GLTRNSDT')
          llGLTRNSDT = .T.
        ENDIF
      ENDIF

      IF SEEK(APDist.cbatchno,'GLPTRNHD')
        llGLPTRNHD = .T.
        IF SEEK(APDist.cbatchno,'GLPTRNDT')
          llGLPTRNDT = .T.
        ENDIF
      ENDIF

      IF SEEK(APDist.cbatchno,'GLBATCH')
        llGLBATCH = .T.
      ENDIF
    ENDIF
  
    Do CASE MONTH(APDist.dApDtrdat)
    
       CASE MONTH(APDist.dApDtrdat) = 1
		   IF APDist.dApDtrdat < {04,01,2002} 
		      lcPeriod = '01'
		   ELSE
		      lcPeriod = '10'
		   ENDIF

       CASE MONTH(APDist.dApDtrdat) = 2
		   IF APDist.dApDtrdat < {04,01,2002} 
		      lcPeriod = '02'
		   ELSE
		      lcPeriod = '11'
		   ENDIF
		   		    
       CASE MONTH(APDist.dApDtrdat) = 3
		   IF APDist.dApDtrdat < {04,01,2002} 
		      lcPeriod = '03'
		   ELSE
		      lcPeriod = '12'
		   ENDIF
		   		   
       CASE MONTH(APDist.dApDtrdat) = 4
		   IF APDist.dApDtrdat < {04,01,2002} 
		      lcPeriod = '04'
		   ELSE
		      lcPeriod = '01'
		   ENDIF
		   		   
       CASE MONTH(APDist.dApDtrdat) = 5
		   IF APDist.dApDtrdat < {04,01,2002} 
		      lcPeriod = '05'
		   ELSE
		      lcPeriod = '02'
		   ENDIF
		   		    
       CASE MONTH(APDist.dApDtrdat) = 6
		   IF APDist.dApDtrdat < {04,01,2002} 
		      lcPeriod = '06'
		   ELSE
		      lcPeriod = '03'
		   ENDIF
		   		    
       CASE MONTH(APDist.dApDtrdat) = 7
		   IF APDist.dApDtrdat < {04,01,2002} 
		      lcPeriod = '07'
		   ELSE
		      lcPeriod = '04'
		   ENDIF
		   		    
       CASE MONTH(APDist.dApDtrdat) = 8
		   IF APDist.dApDtrdat < {04,01,2002} 
		      lcPeriod = '08'
		   ELSE
		      lcPeriod = '05'
		   ENDIF
		   		    
       CASE MONTH(APDist.dApDtrdat) = 9
		   IF APDist.dApDtrdat < {04,01,2002} 
		      lcPeriod = '09'
		   ELSE
		      lcPeriod = '06'
		   ENDIF
		   		    
       CASE MONTH(APDist.dApDtrdat) = 10
		   IF APDist.dApDtrdat < {04,01,2002} 
		      lcPeriod = '10'
		   ELSE
		      lcPeriod = '07'
		   ENDIF
		   		    
       CASE MONTH(APDist.dApDtrdat) = 11
		   IF APDist.dApDtrdat < {04,01,2002} 
		      lcPeriod = '11'
		   ELSE
		      lcPeriod = '08'
		   ENDIF
		   		    
       CASE MONTH(APDist.dApDtrdat) = 12
		   IF APDist.dApDtrdat < {04,01,2002} 
		      lcPeriod = '12'
		   ELSE
		      lcPeriod = '09'
		   ENDIF
    ENDCASE
    
    REPLACE ApDist.cfspprdid WITH lcPeriod;
	        ApDist.cfisfyear WITH lcYear

    *--Update GLTRNSHD
	IF llGLTRNSHD
  	  REPLACE GLTRNSHD.ctrnpprd WITH lcPeriod;
              GLTRNSHD.ctrnpyr  WITH lcYear
	ENDIF

	*--Update GLTRNSDT 
    IF llGLTRNSDT 
  	  REPLACE GLTRNSDT.ctrnpprd WITH lcPeriod;
	          GLTRNSDT.ctrnpyr  WITH lcYear
    ENDIF
		   		   
	*--Update GLPTRNHD 
	IF llGLPTRNHD
  	   REPLACE GLPTRNHD.ctrnpprd WITH lcPeriod;
	           GLPTRNHD.ctrnpyr  WITH lcYear
	ENDIF
		   		   
	*--Update GLPTRNDT
	IF llGLPTRNDT
  	   REPLACE GLPTRNDT.ctrnpprd WITH lcPeriod;
	           GLPTRNDT.ctrnpyr  WITH lcYear
    ENDIF

	*--Update GLBATCH
	IF llGLBATCH
  	   REPLACE GLBATCH.cbatpyr  WITH lcYear
	ENDIF

ENDSCAN

*-- change period to the new period accourding to closing in APPAYMNT
SELECT APPAYMNT
SCAN
  *-- if not in the required date then loop
  IF dpaydate < {01,01,2001}
    LOOP
  ENDIF

  
  WAIT WINDOW "Updating APPAYMNT "   NOWAIT

  *---Bucket 2000 From {01/01/2001 To 12/31/2001}
  IF BETWEEN(dpaydate,{01,01,2001},{12,31,2001})
    lcYear = "2000"
  ENDIF

  *---Bucket 2001 From {01/01/2002 To 03/31/2001}
  *-- change period to the new period accourding to closing
  IF BETWEEN(dpaydate,{01,01,2002},{03,31,2001})
    lcYear = "2001"
  ENDIF
    
  *---Bucket 2002 From {04/01/2002 To 03/31/2003}
  *-- change period to the new period accourding to closing
  IF BETWEEN(dpaydate,{04,01,2002},{03,31,2003})
    lcYear = "2002"
  ENDIF
      
  *---Bucket 2003 From {04/01/2003 To 03/31/2004}
  *-- change period to the new period accourding to closing
  IF BETWEEN(dpaydate,{04,01,2003},{03,31,2004})
    lcYear = "2003"
  ENDIF

  *---Bucket 2004 From {04/01/2004 To 03/31/2005}
  *-- change period to the new period accourding to closing
  IF BETWEEN(dpaydate,{04,01,2004},{03,31,2005})
    lcYear = "2004"
  ENDIF
  Do CASE MONTH(APPAYMNT.dpaydate) 
    
       CASE MONTH(APPAYMNT.dpaydate) = 1
		   IF APPAYMNT.dpaydate < {04,01,2002} 
		      lcPeriod = '01'
		   ELSE
		      lcPeriod = '10'
		   ENDIF

       CASE MONTH(APPAYMNT.dpaydate) = 2
		   IF APPAYMNT.dpaydate < {04,01,2002} 
		      lcPeriod = '02'
		   ELSE
		      lcPeriod = '11'
		   ENDIF
		   		    
       CASE MONTH(APPAYMNT.dpaydate) = 3
		   IF APPAYMNT.dpaydate < {04,01,2002} 
		      lcPeriod = '03'
		   ELSE
		      lcPeriod = '12'
		   ENDIF
		   		   
       CASE MONTH(APPAYMNT.dpaydate) = 4
		   IF APPAYMNT.dpaydate < {04,01,2002} 
		      lcPeriod = '04'
		   ELSE
		      lcPeriod = '01'
		   ENDIF
		   		   
       CASE MONTH(APPAYMNT.dpaydate) = 5
		   IF APPAYMNT.dpaydate < {04,01,2002} 
		      lcPeriod = '05'
		   ELSE
		      lcPeriod = '02'
		   ENDIF
		   		    
       CASE MONTH(APPAYMNT.dpaydate) = 6
		   IF APPAYMNT.dpaydate < {04,01,2002} 
		      lcPeriod = '06'
		   ELSE
		      lcPeriod = '03'
		   ENDIF
		   		    
       CASE MONTH(APPAYMNT.dpaydate) = 7
		   IF APPAYMNT.dpaydate < {04,01,2002} 
		      lcPeriod = '07'
		   ELSE
		      lcPeriod = '04'
		   ENDIF
		   		    
       CASE MONTH(APPAYMNT.dpaydate) = 8
		   IF APPAYMNT.dpaydate < {04,01,2002} 
		      lcPeriod = '08'
		   ELSE
		      lcPeriod = '05'
		   ENDIF
		   		    
       CASE MONTH(APPAYMNT.dpaydate) = 9
		   IF APPAYMNT.dpaydate < {04,01,2002} 
		      lcPeriod = '09'
		   ELSE
		      lcPeriod = '06'
		   ENDIF
		   		    
       CASE MONTH(APPAYMNT.dpaydate) = 10
		   IF APPAYMNT.dpaydate < {04,01,2002} 
		      lcPeriod = '10'
		   ELSE
		      lcPeriod = '07'
		   ENDIF
		   		    
       CASE MONTH(APPAYMNT.dpaydate) = 11
		   IF APPAYMNT.dpaydate < {04,01,2002} 
		      lcPeriod = '11'
		   ELSE
		      lcPeriod = '08'
		   ENDIF
		   		    
       CASE MONTH(APPAYMNT.dpaydate) = 12
		   IF APPAYMNT.dpaydate < {04,01,2002} 
		      lcPeriod = '12'
		   ELSE
		      lcPeriod = '09'
		   ENDIF
    ENDCASE
    
    REPLACE APPAYMNT.cfspprdid WITH lcPeriod;
	        APPAYMNT.cfisfyear WITH lcYear
ENDSCAN

*--Glptrnshd


*-- change period to the new period accourding to closing in APPAYMNT
SELECT GLTRNSHD
SCAN
  *-- if not in the required date then loop
  IF dtrnpdate < {01,01,2001}
    LOOP
  ENDIF

  
  WAIT WINDOW "Updating APPAYMNT "   NOWAIT

  *---Bucket 2000 From {01/01/2001 To 12/31/2001}
  IF BETWEEN(dtrnpdate,{01,01,2001},{12,31,2001})
    lcYear = "2000"
  ENDIF

  *---Bucket 2001 From {01/01/2002 To 03/31/2001}
  *-- change period to the new period accourding to closing
  IF BETWEEN(dtrnpdate,{01,01,2002},{03,31,2001})
    lcYear = "2001"
  ENDIF
    
  *---Bucket 2002 From {04/01/2002 To 03/31/2003}
  *-- change period to the new period accourding to closing
  IF BETWEEN(dtrnpdate,{04,01,2002},{03,31,2003})
    lcYear = "2002"
  ENDIF
      
  *---Bucket 2003 From {04/01/2003 To 03/31/2004}
  *-- change period to the new period accourding to closing
  IF BETWEEN(dtrnpdate,{04,01,2003},{03,31,2004})
    lcYear = "2003"
  ENDIF

  *---Bucket 2004 From {04/01/2004 To 03/31/2005}
  *-- change period to the new period accourding to closing
  IF BETWEEN(dtrnpdate,{04,01,2004},{03,31,2005})
    lcYear = "2004"
  ENDIF
  
  Do CASE MONTH(GLTRNSHD.dtrnpdate)
    
       CASE MONTH(GLTRNSHD.dtrnpdate) = 1
		   IF GLTRNSHD.dtrnpdate < {04,01,2002} 
		      lcPeriod = '01'
		   ELSE
		      lcPeriod = '10'
		   ENDIF

       CASE MONTH(GLTRNSHD.dtrnpdate)=2
		   IF GLTRNSHD.dtrnpdate < {04,01,2002} 
		      lcPeriod = '02'
		   ELSE
		      lcPeriod = '11'
		   ENDIF
		   		    
       CASE MONTH(GLTRNSHD.dtrnpdate)=3
		   IF GLTRNSHD.dtrnpdate < {04,01,2002} 
		      lcPeriod = '03'
		   ELSE
		      lcPeriod = '12'
		   ENDIF
		   		   
       CASE MONTH(GLTRNSHD.dtrnpdate)=4
		   IF GLTRNSHD.dtrnpdate < {04,01,2002} 
		      lcPeriod = '04'
		   ELSE
		      lcPeriod = '01'
		   ENDIF
		   		   
       CASE MONTH(GLTRNSHD.dtrnpdate)=5
		   IF GLTRNSHD.dtrnpdate < {04,01,2002} 
		      lcPeriod = '05'
		   ELSE
		      lcPeriod = '02'
		   ENDIF
		   		    
       CASE MONTH(GLTRNSHD.dtrnpdate)=6
		   IF GLTRNSHD.dtrnpdate < {04,01,2002} 
		      lcPeriod = '06'
		   ELSE
		      lcPeriod = '03'
		   ENDIF
		   		    
       CASE MONTH(GLTRNSHD.dtrnpdate)=7
		   IF GLTRNSHD.dtrnpdate < {04,01,2002} 
		      lcPeriod = '07'
		   ELSE
		      lcPeriod = '04'
		   ENDIF
		   		    
       CASE MONTH(GLTRNSHD.dtrnpdate)=8
		   IF GLTRNSHD.dtrnpdate < {04,01,2002} 
		      lcPeriod = '08'
		   ELSE
		      lcPeriod = '05'
		   ENDIF
		   		    
       CASE MONTH(GLTRNSHD.dtrnpdate)=9
		   IF GLTRNSHD.dtrnpdate < {04,01,2002} 
		      lcPeriod = '09'
		   ELSE
		      lcPeriod = '06'
		   ENDIF
		   		    
       CASE MONTH(GLTRNSHD.dtrnpdate)=10
		   IF GLTRNSHD.dtrnpdate < {04,01,2002} 
		      lcPeriod = '10'
		   ELSE
		      lcPeriod = '07'
		   ENDIF
		   		    
       CASE MONTH(GLTRNSHD.dtrnpdate)=11
		   IF GLTRNSHD.dtrnpdate < {04,01,2002} 
		      lcPeriod = '11'
		   ELSE
		      lcPeriod = '08'
		   ENDIF
		   		    
       CASE MONTH(GLTRNSHD.dtrnpdate)=12
		   IF GLTRNSHD.dtrnpdate < {04,01,2002} 
		      lcPeriod = '12'
		   ELSE
		      lcPeriod = '09'
		   ENDIF
    ENDCASE
    
    REPLACE GLTRNSHD.ctrnpprd  WITH lcPeriod;
	        GLTRNSHD.ctrnpyr   WITH lcYear
	IF !EMPTY(dtrnrevdt)          
	  lcMperid = ALLTRIM(STR(MONTH(GLTRNSHD.dtrnrevdt)-3))
	  lcper = IIF(VAL(lcMperid) > 9 ,LEFT(lcMperid,2),"0"+LEFT(lcMperid,1))
	  REPLACE GLTRNSHD.ctrnrevyr WITH lcYear;
	          GLTRNSHD.ctrnrevpr WITH lcper
	ENDIF        
ENDSCAN


*--detail
*-- change period to the new period accourding to closing in APPAYMNT
SELECT GLTRNSDT
SCAN
  *-- if not in the required date then loop
  IF dtrnpdate < {01,01,2001}
    LOOP
  ENDIF

  
  WAIT WINDOW "Updating APPAYMNT "   NOWAIT

  *---Bucket 2000 From {01/01/2001 To 12/31/2001}
  IF BETWEEN(dtrnpdate,{01,01,2001},{12,31,2001})
    lcYear = "2000"
  ENDIF

  *---Bucket 2001 From {01/01/2002 To 03/31/2001}
  *-- change period to the new period accourding to closing
  IF BETWEEN(dtrnpdate,{01,01,2002},{03,31,2001})
    lcYear = "2001"
  ENDIF
    
  *---Bucket 2002 From {04/01/2002 To 03/31/2003}
  *-- change period to the new period accourding to closing
  IF BETWEEN(dtrnpdate,{04,01,2002},{03,31,2003})
    lcYear = "2002"
  ENDIF
      
  *---Bucket 2003 From {04/01/2003 To 03/31/2004}
  *-- change period to the new period accourding to closing
  IF BETWEEN(dtrnpdate,{04,01,2003},{03,31,2004})
    lcYear = "2003"
  ENDIF

  *---Bucket 2004 From {04/01/2004 To 03/31/2005}
  *-- change period to the new period accourding to closing
  IF BETWEEN(dtrnpdate,{04,01,2004},{03,31,2005})
    lcYear = "2004"
  ENDIF
  Do CASE MONTH(GLTRNSDT.dtrnpdate)

       CASE MONTH(GLTRNSDT.dtrnpdate)=1
		   IF GLTRNSDT.dtrnpdate < {04,01,2002} 
		      lcPeriod = '01'
		   ELSE
		      lcPeriod = '10'
		   ENDIF

       CASE MONTH(GLTRNSDT.dtrnpdate)=2
		   IF GLTRNSDT.dtrnpdate < {04,01,2002} 
		      lcPeriod = '02'
		   ELSE
		      lcPeriod = '11'
		   ENDIF
		   		    
       CASE MONTH(GLTRNSDT.dtrnpdate)=3
		   IF GLTRNSDT.dtrnpdate < {04,01,2002} 
		      lcPeriod = '03'
		   ELSE
		      lcPeriod = '12'
		   ENDIF
		   		   
       CASE MONTH(GLTRNSDT.dtrnpdate)=4
		   IF GLTRNSDT.dtrnpdate < {04,01,2002} 
		      lcPeriod = '04'
		   ELSE
		      lcPeriod = '01'
		   ENDIF
		   		   
       CASE MONTH(GLTRNSDT.dtrnpdate)=5
		   IF GLTRNSDT.dtrnpdate < {04,01,2002} 
		      lcPeriod = '05'
		   ELSE
		      lcPeriod = '02'
		   ENDIF
		   		    
       CASE MONTH(GLTRNSDT.dtrnpdate)=6
		   IF GLTRNSDT.dtrnpdate < {04,01,2002} 
		      lcPeriod = '06'
		   ELSE
		      lcPeriod = '03'
		   ENDIF
		   		    
       CASE MONTH(GLTRNSDT.dtrnpdate)=7
		   IF GLTRNSDT.dtrnpdate < {04,01,2002} 
		      lcPeriod = '07'
		   ELSE
		      lcPeriod = '04'
		   ENDIF
		   		    
       CASE MONTH(GLTRNSDT.dtrnpdate)=8
		   IF GLTRNSDT.dtrnpdate < {04,01,2002} 
		      lcPeriod = '08'
		   ELSE
		      lcPeriod = '05'
		   ENDIF
		   		    
       CASE MONTH(GLTRNSDT.dtrnpdate)=9
		   IF GLTRNSDT.dtrnpdate < {04,01,2002} 
		      lcPeriod = '09'
		   ELSE
		      lcPeriod = '06'
		   ENDIF
		   		    
       CASE MONTH(GLTRNSDT.dtrnpdate)=10
		   IF GLTRNSDT.dtrnpdate < {04,01,2002} 
		      lcPeriod = '10'
		   ELSE
		      lcPeriod = '07'
		   ENDIF
		   		    
       CASE MONTH(GLTRNSDT.dtrnpdate)=11
		   IF GLTRNSDT.dtrnpdate < {04,01,2002} 
		      lcPeriod = '11'
		   ELSE
		      lcPeriod = '08'
		   ENDIF
		   		    
       CASE MONTH(GLTRNSDT.dtrnpdate)=12
		   IF GLTRNSDT.dtrnpdate < {04,01,2002} 
		      lcPeriod = '12'
		   ELSE
		      lcPeriod = '09'
		   ENDIF
    ENDCASE
    
    REPLACE GLTRNSDT.ctrnpprd WITH lcPeriod;
	        GLTRNSDT.ctrnpyr WITH lcYear
ENDSCAN

*-- change period to the new period accourding to closing in APPAYMNT
SELECT GLPTRNHD
SCAN
  *-- if not in the required date then loop
  IF dtrnpdate < {01,01,2001}
    LOOP
  ENDIF

  
  WAIT WINDOW "Updating APPAYMNT "   NOWAIT

  *---Bucket 2000 From {01/01/2001 To 12/31/2001}
  IF BETWEEN(dtrnpdate,{01,01,2001},{12,31,2001})
    lcYear = "2000"
  ENDIF

  *---Bucket 2001 From {01/01/2002 To 03/31/2001}
  *-- change period to the new period accourding to closing
  IF BETWEEN(dtrnpdate,{01,01,2002},{03,31,2001})
    lcYear = "2001"
  ENDIF
    
  *---Bucket 2002 From {04/01/2002 To 03/31/2003}
  *-- change period to the new period accourding to closing
  IF BETWEEN(dtrnpdate,{04,01,2002},{03,31,2003})
    lcYear = "2002"
  ENDIF
      
  *---Bucket 2003 From {04/01/2003 To 03/31/2004}
  *-- change period to the new period accourding to closing
  IF BETWEEN(dtrnpdate,{04,01,2003},{03,31,2004})
    lcYear = "2003"
  ENDIF

  *---Bucket 2004 From {04/01/2004 To 03/31/2005}
  *-- change period to the new period accourding to closing
  IF BETWEEN(dtrnpdate,{04,01,2004},{03,31,2005})
    lcYear = "2004"
  ENDIF
  Do CASE MONTH(GLPTRNHD.dtrnpdate)
        
       CASE MONTH(GLPTRNHD.dtrnpdate)=1
		   IF GLPTRNHD.dtrnpdate < {04,01,2002} 
		      lcPeriod = '01'
		   ELSE
		      lcPeriod = '10'
		   ENDIF

       CASE MONTH(GLPTRNHD.dtrnpdate)=2
		   IF GLPTRNHD.dtrnpdate < {04,01,2002} 
		      lcPeriod = '02'
		   ELSE
		      lcPeriod = '11'
		   ENDIF
		   		    
       CASE MONTH(GLPTRNHD.dtrnpdate)=3
		   IF GLPTRNHD.dtrnpdate < {04,01,2002} 
		      lcPeriod = '03'
		   ELSE
		      lcPeriod = '12'
		   ENDIF
		   		   
       CASE MONTH(GLPTRNHD.dtrnpdate)=4
		   IF GLPTRNHD.dtrnpdate < {04,01,2002} 
		      lcPeriod = '04'
		   ELSE
		      lcPeriod = '01'
		   ENDIF
		   		   
       CASE MONTH(GLPTRNHD.dtrnpdate)=5
		   IF GLPTRNHD.dtrnpdate < {04,01,2002} 
		      lcPeriod = '05'
		   ELSE
		      lcPeriod = '02'
		   ENDIF
		   		    
       CASE MONTH(GLPTRNHD.dtrnpdate)=6
		   IF GLPTRNHD.dtrnpdate < {04,01,2002} 
		      lcPeriod = '06'
		   ELSE
		      lcPeriod = '03'
		   ENDIF
		   		    
       CASE MONTH(GLPTRNHD.dtrnpdate)=7
		   IF GLPTRNHD.dtrnpdate < {04,01,2002} 
		      lcPeriod = '07'
		   ELSE
		      lcPeriod = '04'
		   ENDIF
		   		    
       CASE MONTH(GLPTRNHD.dtrnpdate)=8
		   IF GLPTRNHD.dtrnpdate < {04,01,2002} 
		      lcPeriod = '08'
		   ELSE
		      lcPeriod = '05'
		   ENDIF
		   		    
       CASE MONTH(GLPTRNHD.dtrnpdate)=9
		   IF GLPTRNHD.dtrnpdate < {04,01,2002} 
		      lcPeriod = '09'
		   ELSE
		      lcPeriod = '06'
		   ENDIF
		   		    
       CASE MONTH(GLPTRNHD.dtrnpdate)=10
		   IF GLPTRNHD.dtrnpdate < {04,01,2002} 
		      lcPeriod = '10'
		   ELSE
		      lcPeriod = '07'
		   ENDIF
		   		    
       CASE MONTH(GLPTRNHD.dtrnpdate)=11
		   IF GLPTRNHD.dtrnpdate < {04,01,2002} 
		      lcPeriod = '11'
		   ELSE
		      lcPeriod = '08'
		   ENDIF
		   		    
       CASE MONTH(GLPTRNHD.dtrnpdate)=12
		   IF GLPTRNHD.dtrnpdate < {04,01,2002} 
		      lcPeriod = '12'
		   ELSE
		      lcPeriod = '09'
		   ENDIF
    ENDCASE
    
    REPLACE GLPTRNHD.ctrnpprd WITH lcPeriod;
	        GLPTRNHD.ctrnpyr WITH lcYear
ENDSCAN


*-- change period to the new period accourding to closing in APPAYMNT
SELECT GLPTRNDT
SCAN
  *-- if not in the required date then loop
  IF dtrnpdate < {01,01,2001}
    LOOP
  ENDIF

  
  WAIT WINDOW "Updating APPAYMNT "   NOWAIT

  *---Bucket 2000 From {01/01/2001 To 12/31/2001}
  IF BETWEEN(dtrnpdate,{01,01,2001},{12,31,2001})
    lcYear = "2000"
  ENDIF

  *---Bucket 2001 From {01/01/2002 To 03/31/2001}
  *-- change period to the new period accourding to closing
  IF BETWEEN(dtrnpdate,{01,01,2002},{03,31,2001})
    lcYear = "2001"
  ENDIF
    
  *---Bucket 2002 From {04/01/2002 To 03/31/2003}
  *-- change period to the new period accourding to closing
  IF BETWEEN(dtrnpdate,{04,01,2002},{03,31,2003})
    lcYear = "2002"
  ENDIF
      
  *---Bucket 2003 From {04/01/2003 To 03/31/2004}
  *-- change period to the new period accourding to closing
  IF BETWEEN(dtrnpdate,{04,01,2003},{03,31,2004})
    lcYear = "2003"
  ENDIF

  *---Bucket 2004 From {04/01/2004 To 03/31/2005}
  *-- change period to the new period accourding to closing
  IF BETWEEN(dtrnpdate,{04,01,2004},{03,31,2005})
    lcYear = "2004"
  ENDIF
  Do CASE MONTH(GLPTRNDT.dtrnpdate)
    
       CASE MONTH(GLPTRNDT.dtrnpdate)=1
		   IF GLPTRNDT.dtrnpdate < {04,01,2002} 
		      lcPeriod = '01'
		   ELSE
		      lcPeriod = '10'
		   ENDIF

       CASE MONTH(GLPTRNDT.dtrnpdate)=2
		   IF GLPTRNDT.dtrnpdate < {04,01,2002} 
		      lcPeriod = '02'
		   ELSE
		      lcPeriod = '11'
		   ENDIF
		   		    
       CASE MONTH(GLPTRNDT.dtrnpdate)=3
		   IF GLPTRNDT.dtrnpdate < {04,01,2002} 
		      lcPeriod = '03'
		   ELSE
		      lcPeriod = '12'
		   ENDIF
		   		   
       CASE MONTH(GLPTRNDT.dtrnpdate)=4
		   IF GLPTRNDT.dtrnpdate < {04,01,2002} 
		      lcPeriod = '04'
		   ELSE
		      lcPeriod = '01'
		   ENDIF
		   		   
       CASE MONTH(GLPTRNDT.dtrnpdate)=5
		   IF GLPTRNDT.dtrnpdate < {04,01,2002} 
		      lcPeriod = '05'
		   ELSE
		      lcPeriod = '02'
		   ENDIF
		   		    
       CASE MONTH(GLPTRNDT.dtrnpdate)=6
		   IF GLPTRNDT.dtrnpdate < {04,01,2002} 
		      lcPeriod = '06'
		   ELSE
		      lcPeriod = '03'
		   ENDIF
		   		    
       CASE MONTH(GLPTRNDT.dtrnpdate)=7
		   IF GLPTRNDT.dtrnpdate < {04,01,2002} 
		      lcPeriod = '07'
		   ELSE
		      lcPeriod = '04'
		   ENDIF
		   		    
       CASE MONTH(GLPTRNDT.dtrnpdate)=8
		   IF GLPTRNDT.dtrnpdate < {04,01,2002} 
		      lcPeriod = '08'
		   ELSE
		      lcPeriod = '05'
		   ENDIF
		   		    
       CASE MONTH(GLPTRNDT.dtrnpdate)=9
		   IF GLPTRNDT.dtrnpdate < {04,01,2002} 
		      lcPeriod = '09'
		   ELSE
		      lcPeriod = '06'
		   ENDIF
		   		    
       CASE MONTH(GLPTRNDT.dtrnpdate)=10
		   IF GLPTRNDT.dtrnpdate < {04,01,2002} 
		      lcPeriod = '10'
		   ELSE
		      lcPeriod = '07'
		   ENDIF
		   		    
       CASE MONTH(GLPTRNDT.dtrnpdate)=11
		   IF GLPTRNDT.dtrnpdate < {04,01,2002} 
		      lcPeriod = '11'
		   ELSE
		      lcPeriod = '08'
		   ENDIF
		   		    
       CASE MONTH(GLPTRNDT.dtrnpdate)=12
		   IF GLPTRNDT.dtrnpdate < {04,01,2002} 
		      lcPeriod = '12'
		   ELSE
		      lcPeriod = '09'
		   ENDIF
    ENDCASE
    
    REPLACE GLPTRNDT.ctrnpprd WITH lcPeriod;
	        GLPTRNDT.ctrnpyr WITH lcYear
ENDSCAN



*-- change period to the new period accourding to closing in APPAYMNT
*SELECT ARCUSHST
*SCAN
*  WAIT WINDOW "Updating ARCUSHST "   NOWAIT
*  REPLACE cfisfyear WITH "2002"
*  SKIP
*  IF EOF()
*    EXIT
*  ENDIF
*  REPLACE cfisfyear WITH "2003"
*  SKIP
*  IF EOF()
*    EXIT
*  ENDIF
*  REPLACE cfisfyear WITH "2004"
*ENDSCAN


*-- change period to the new period accourding to closing in ICSTYHST
*SELECT ICSTYHST
*SCAN
*    WAIT WINDOW "Updating APVENHST "   NOWAIT
*	REPLACE cfisfyear WITH "2002"
*	SKIP
*    IF EOF()
*      EXIT
*    ENDIF
*	REPLACE cfisfyear WITH "2003"
*	SKIP
*    IF EOF()
*      EXIT
*    ENDIF
*	REPLACE cfisfyear WITH "2004"
*ENDSCAN

*-- change period to the new period accourding to closing in APVENHST
*SELECT APVENHST
*SCAN
*    WAIT WINDOW "Updating APVENHST "   NOWAIT
*	REPLACE cfisfyear WITH "2002"
*	SKIP
*    IF EOF()
*      EXIT
*    ENDIF
*	REPLACE cfisfyear WITH "2003"
*	SKIP
*    IF EOF()
*      EXIT
*    ENDIF
*	REPLACE cfisfyear WITH "2004"
*ENDSCAN

*-- change period to the new period accourding to closing in FISHD
SELECT FISHD
LOCATE
REPLACE cfisfyear WITH "2000",;
        Cfisystat WITH "H"   ,;
        cFislhead With "Fiscal year 2000 (01/01/2001-12/31/2001)" ,;
        cfisshead With "Fis. year 2000 " 
SKIP        

REPLACE cfisfyear WITH "2001",;
        Cfisystat WITH "P"   ,;
        cFislhead With "Fiscal year 2001 (01/01/2002-03/31/2002)" ,;
        cfisshead With "Fis. year 2001 " 
SKIP
REPLACE cfisfyear WITH "2002",;
        Cfisystat WITH "C"   ,;
        cFislhead With "Fiscal year 2002 (04/01/2002-03/31/2003)" ,;
        cfisshead With "Fis. year 2002 " 

SKIP
REPLACE cfisfyear WITH "2003",;
        Cfisystat WITH "N"   ,;
        cFislhead With "Fiscal year 2003 (04/01/2003-03/31/2004)" ,;
        cfisshead With "Fis. year 2003 " 

*SKIP
*IF EOF()
*  APPEND BLANK
*  REPLACE cfisfyear WITH "2004",;
        Cfisystat WITH "N"   ,;
        cFislhead With "Fiscal year 2004 " ,;
        cfisnoprd With "12",;
        cFisnonwd WITH "67",;
        DfisBgDat WITH {04/01/2004},;
        DfisEnDat WITH {03/31/2005},;
        cfisshead With "Fis. year 2004 " 
*ENDIF
*--Update FSPRD and add new year 
SELECT FSPRD
SCAN
  WAIT WINDOW "Updating FSPRD "   NOWAIT
  REPLACE cfisfyear WITH ALLTRIM(STR(VAL(cfisfyear)-1))
  IF cfisfyear < "2003"
    IF cfisfyear = "2002" AND cfspprdid = "12"
      REPLACE lfspclsds WITH .F.
    ELSE
      REPLACE lfspclsds WITH .T.
    ENDIF
  ENDIF
  
ENDSCAN

*=SEEK("2003"+"01")
*SCATTER MEMVAR MEMO
*m.cfisfyear = "2004"
*m.dfsppbgdt = {04/01/2004}
*m.dfsppendt = {04/30/2004}
*m.Nfsppartn = 1
*APPEND BLANK 
*GATHER MEMVAR MEMO

*=SEEK("2003"+"02")
*SCATTER MEMVAR MEMO
*m.cfisfyear = "2004"
*m.dfsppbgdt = {05/01/2004}
*m.dfsppendt = {05/31/2004}
*m.Nfsppartn = 1
*APPEND BLANK 
*GATHER MEMVAR MEMO

*=SEEK("2003"+"03")
*SCATTER MEMVAR MEMO
*m.cfisfyear = "2004"
*m.dfsppbgdt = {06/01/2004}
*m.dfsppendt = {06/30/2004}
*m.Nfsppartn = 1
*APPEND BLANK 
*GATHER MEMVAR MEMO

*=SEEK("2003"+"04")
*SCATTER MEMVAR MEMO
*m.cfisfyear = "2004"
*m.dfsppbgdt = {07/01/2004}
*m.dfsppendt = {07/31/2004}
*m.Nfsppartn = 2
*APPEND BLANK 
*GATHER MEMVAR MEMO

*=SEEK("2003"+"05")
*SCATTER MEMVAR MEMO
*m.cfisfyear = "2004"
*m.dfsppbgdt = {08/01/2004}
*m.dfsppendt = {08/31/2004}
*m.Nfsppartn = 2
*APPEND BLANK 
*GATHER MEMVAR MEMO

*=SEEK("2003"+"06")
*SCATTER MEMVAR MEMO
*m.cfisfyear = "2004"
*m.dfsppbgdt = {09/01/2004}
*m.dfsppendt = {09/30/2004}
*m.Nfsppartn = 2
*APPEND BLANK 
*GATHER MEMVAR MEMO

*=SEEK("2003"+"07")
*SCATTER MEMVAR MEMO
*m.cfisfyear = "2004"
*m.dfsppbgdt = {10/01/2004}
*m.dfsppendt = {10/31/2004}
*m.Nfsppartn = 3
*APPEND BLANK 
*GATHER MEMVAR MEMO

*=SEEK("2003"+"08")
*SCATTER MEMVAR MEMO
*m.cfisfyear = "2004"
*m.dfsppbgdt = {11/01/2004}
*m.dfsppendt = {11/30/2004}
*m.Nfsppartn = 3
*APPEND BLANK 
*GATHER MEMVAR MEMO

*=SEEK("2003"+"09")
*SCATTER MEMVAR MEMO
*m.cfisfyear = "2004"
*m.dfsppbgdt = {12/01/2004}
*m.dfsppendt = {12/31/2004}
*m.Nfsppartn = 3
*APPEND BLANK 
*GATHER MEMVAR MEMO

*=SEEK("2003"+"10")
*SCATTER MEMVAR MEMO
*m.cfisfyear = "2004"
*m.dfsppbgdt = {01/01/2005}
*m.dfsppendt = {01/30/2005}
*m.Nfsppartn = 4
*APPEND BLANK 
*GATHER MEMVAR MEMO

*=SEEK("2003"+"11")
*SCATTER MEMVAR MEMO
*m.cfisfyear = "2004"
*m.dfsppbgdt = {02/01/2005}
*m.dfsppendt = {02/28/2005}
*m.Nfsppartn = 4
*APPEND BLANK 
*GATHER MEMVAR MEMO

*=SEEK("2003"+"12")
*SCATTER MEMVAR MEMO
*m.cfisfyear = "2004"
*m.dfsppbgdt = {03/01/2005}
*m.dfsppendt = {03/31/2005}
*m.Nfsppartn = 4
*APPEND BLANK 
*GATHER MEMVAR MEMO


SELECT GLACBALS
SCAN
  WAIT WINDOW "Updating GLACBALS "   NOWAIT
  REPLACE cfisfyear WITH ALLTRIM(STR(VAL(cfisfyear)-1))
ENDSCAN

SELECT GLACCHAR 
SCAN
  WAIT WINDOW "Updating GLACBALS "   NOWAIT
  FOR lnCount =1 TO 12
    SELECT GLACBALS
    lcCount = ALLTRIM(STR(lnCount))
    lcPeriod = IIF(lnCount > 9 ,LEFT(lcCount,2),"0"+LEFT(lcCount,1))

    APPEND BLANK
    REPLACE cAcctcode   WITH GLACCHAR.cAcctcode ,;
            cfisfyear  WITH "2004" ,;
            cfspprdid  WITH lcPeriod
  ENDFOR
ENDSCAN

SELECT SYCCOMP
SET ORDER TO TAG Ccomp_id
=SEEK("05")
REPLACE ccurr_yer WITH "2002",;
        ccurr_prd WITH "12"

WAIT CLEAR


IF USED('ApDist')
  USE IN ApDist
ENDIF

IF USED('GlDist')
  USE IN GlDist
ENDIF

IF USED('GlBatch')
  USE IN GlBatch
ENDIF

IF USED('GlTrnsdt')
  USE IN GlTrnsdt
ENDIF

IF USED('GlTrnshd')
  USE IN GlTrnshd
ENDIF

IF USED('GlPtrnhd')
  USE IN GlPtrnhd
ENDIF

IF USED('GlPtrndt')
  USE IN GlPtrndt
ENDIF

IF USED('FISHD')
  USE IN FISHD
ENDIF

IF USED('FSPRD')
  USE IN FSPRD
ENDIF

IF USED('APPAYMNT')
  USE IN APPAYMNT
ENDIF

IF USED('ARCUSHST')
  USE IN ARCUSHST
ENDIF

IF USED('APVENHST')
  USE IN APVENHST
ENDIF

IF USED('ICSTYHST')
  USE IN ICSTYHST
ENDIF

IF USED('GLACBALS')
  USE IN GLACBALS
ENDIF

IF USED('GLACCHAR')
  USE IN GLACCHAR
ENDIF

IF USED('SYCCOMP')
  USE IN SYCCOMP
ENDIF

WAIT WINDOW  'Finished updating correct periods.'   nowait


SET DEFAULT TO &lcPath