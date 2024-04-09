select testcst
SCAN FOR bocstdif>.9 and ABS(bomcost-posln)<1
  lcActNo = type
  REPLACE CUTTKTH.NACT_COST&lcActNo WITH bomcost
ENDSCAN