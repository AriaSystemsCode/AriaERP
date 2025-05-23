&& this fix, for gou10 only, Company 01
&& this fix, will re-calculate booked qty in bulk orders header and lines
&& T20121226.0001  - ARIA EDI : Received some edi release po�s for some edi bulk po�s and they are wrong in aria in the s/o .

CLOSE ALL
SET SAFETY OFF
SET TALK OFF

*------------------------------------------------------------------------------------*
lcSysFilPath = GETDIR("","Select system files folder")
IF !EMPTY(ALLTRIM(lcSysFilPath)) AND FILE(ADDBS(lcSysFilPath)+"SYCCOMP.DBF")

  lcSysFilPath = ADDBS(lcSysFilPath)
  CD (lcSysFilPath)

  IF USED('SYCCOMP')
    USE IN SYCCOMP
  ENDIF
  USE lcSysFilPath+'SycComp' IN 0 SHARED

  SELECT SYCCOMP
  *SCAN FOR ALLTRIM(SYCCOMP.cComp_Id) = "01" OR ALLTRIM(SYCCOMP.cComp_Id) = "02"
  SCAN 
    MESSAGEBOX( "Processing Bulk Orders of company: "+ ALLTRIM(SYCCOMP.cComp_Id))
    lcCompPath = ADDBS(ALLTRIM(SycComp.cCom_DDir))

    IF USED('OrdHdr')
      USE IN OrdHdr
    ENDIF

    IF USED('Ordline')
      USE IN Ordline
    ENDIF

    IF USED('Ordline1')
      USE IN Ordline1
    ENDIF

    IF !FILE(lcCompPath + 'OrdHdr_1.dbf')
      COPY FILE (lcCompPath + 'OrdHdr.*') TO (lcCompPath + 'OrdHdr_1.*')
    ENDIF

    IF !FILE(lcCompPath + 'Ordline_1.dbf')
      COPY FILE (lcCompPath + 'Ordline.*') TO (lcCompPath + 'Ordline_1.*')
    ENDIF

    USE lcCompPath + 'OrdHdr' IN 0 SHARED
    SELECT OrdHdr
    SET ORDER TO ORDHDR   && CORDTYPE+ORDER

    USE lcCompPath + 'Ordline' IN 0 SHARED
    SELECT Ordline
    SET ORDER TO ORDLINE   && CORDTYPE+ORDER+STR(LINENO,6)

    USE lcCompPath + 'Ordline' IN 0 again SHARED ALIAS Ordline1
    SELECT Ordline1
    SET ORDER TO ORDBULK   && CORDTYPE+CFROMORDER+STR(BULKLINENO,6)

    SELECT ORDHDR
    SCAN
      WAIT WINDOW "Processing Bulk Order : "+ ALLTRIM(OrdHdr.ORDER) NOWAIT
      && 1- Need to create fix program to loop on BULK order that has status 'X' and has Book qty > 0,
      IF Ordhdr.book >0 AND Ordhdr.STATUS = 'X' AND Ordhdr.bulk='Y'
        && 2- will check the Release order created from each bulk order and sum the Qty of each size of the 8 sizes from ordline
        SELECT CFROMORDER,BULKLINENO,SUM(Qty1) AS qty1 ,SUM(Qty2) AS qty2,SUM(Qty3) AS qty3 ,SUM(Qty4) AS qty4,SUM(Qty5) AS qty5 ,;
                SUM(Qty6) AS qty6,SUM(Qty7) AS qty7 ,SUM(Qty8) AS qty8, SUM(totQty) AS totQty ;
                 FROM Ordline1 WHERE CORDTYPE+CFROMORDER+STR(BULKLINENO,6) = 'O'+OrdHdr.ORDER ;
                 GROUP BY CFROMORDER,BULKLINENO ;
                 ORDER BY CFROMORDER,BULKLINENO INTO CURSOR Rel_1

        SELECT Rel_1
        INDEX ON BULKLINENO TAG BULKLINENO
        lnTotBook = 0
        SCAN
          lnTotBook = lnTotBook + totqty
        ENDSCAN
        
        IF lnTotBook >= OrdHdr.Book
          SELECT Ordline
          =SEEK(Ordhdr.CORDTYPE+Ordhdr.ORDER)
          lnTotBook = 0

          SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = Ordhdr.CORDTYPE+Ordhdr.ORDER
            SELECT Rel_1
            =SEEK(Ordline.LINENO)
            FOR i = 1 TO 8
              lcStr = ALLTRIM(STR(i))
              IF Rel_1.Qty&lcStr. >= Ordline.Book&lcStr.
                REPLACE Ordline.Book&lcStr. WITH 0
              ENDIF
              lnTotBook = lnTotBook + Ordline.Book&lcStr.
            ENDFOR
          ENDSCAN
          SELECT OrdHdr
          REPLACE Book WITH 0
          REPLACE bookamt WITH 0
        ENDIF
      ENDIF
    ENDSCAN
    USE IN OrdHdr
    USE IN OrdLine
    USE IN OrdLine1
    USE IN Rel_1
  ENDSCAN
  MESSAGEBOX("Bulk Orders Processing is Completed",0,"Aria systems Inc.")
ELSE
  MESSAGEBOX("Can not find System files folder")
ENDIF

