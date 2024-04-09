*E300683,4 AHMED 06/04/97 Add prgs directory path to the calling of programs
*E300683,4 Call programs from PRGS directory
*DO SMCLOSE WITH 'Y'
DO (gcAppHome + gcWinAppl + '\SMCLOSE ') WITH 'Y'
*E300683,4 end