*E300683,4 AHMED 06/04/97 Add prgs directory path to the calling of programs
*E300683,4 Call programs from PRGS directory
*DO SMCLOSE WITH 'P' 
DO (gcAppHome + gcWinAppl + '\SMCLOSE ') WITH 'P' 
*E300683,4 end