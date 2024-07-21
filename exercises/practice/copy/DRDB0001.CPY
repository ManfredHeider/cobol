      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  29.03.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRDB0001.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRDB0001.CPv  $
      *
      *   Rev 2.1   Mar 29 2004 15:19:16   p13sp14
      *AKI00404 Erstübergabe
      *
      *   Rev 2.0   Mar 29 2004 14:56:04   p13sp14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
000273*********************************************************
000274* Declare the SQLSTATE that can be set by the stor. proc*
000275*********************************************************
000276 01  P-SQLSTATE PIC X(5).
000277*********************************************************
000278* Declare the qualified procedure name                  *
000279*********************************************************
000280 01  P-PROC.
000281     49 P-PROC-LEN  PIC 9(4) USAGE BINARY.
000282     49 P-PROC-TEXT PIC X(27).
000283*********************************************************
000284* Declare the specific procedure name                   *
000285*********************************************************
000286 01  P-SPEC.
000287     49 P-SPEC-LEN  PIC 9(4) USAGE BINARY.
000288     49 P-SPEC-TEXT PIC X(18).
000289*********************************************************
000290* Declare SQL diagnostic message token                  *
000291*********************************************************
000292 01  P-DIAG.
000293     49 P-DIAG-LEN  PIC 9(4) USAGE BINARY.
000294     49 P-DIAG-TEXT PIC X(70).
000295*********************************************************
000296* Declare the DBINFO structure                          *
000297*********************************************************
000298 01  SP-DBINFO.
000299*       Location length and name
000300     02 UDF-DBINFO-LOCATION.
000301        49 UDF-DBINFO-LLEN PIC 9(4) USAGE BINARY.
000302        49 UDF-DBINFO-LOC  PIC X(128).
000303*       Authorization ID length and name
000304     02 UDF-DBINFO-AUTHORIZATION.
000305        49 UDF-DBINFO-ALEN PIC 9(4) USAGE BINARY.
000306        49 UDF-DBINFO-AUTH PIC X(128).
000307*       CCSIDs for DB2 for OS/390
000308     02 UDF-DBINFO-CCSID PIC X(48).
000309     02 UDF-DBINFO-CCSID-REDEFINE REDEFINES UDF-DBINFO-CCSID.
000310        03 UDF-DBINFO-ASBCS   PIC 9(9) USAGE BINARY.
000311        03 UDF-DBINFO-ADBCS   PIC 9(9) USAGE BINARY.
000312        03 UDF-DBINFO-AMIXED  PIC 9(9) USAGE BINARY.
000313        03 UDF-DBINFO-ESBCS   PIC 9(9) USAGE BINARY.
000314        03 UDF-DBINFO-EDBCS   PIC 9(9) USAGE BINARY.
000315        03 UDF-DBINFO-EMIXED  PIC 9(9) USAGE BINARY.
000316        03 UDF-DBINFO-ENCODE  PIC 9(9) USAGE BINARY.
000317        03 UDF-DBINFO-USBCS   PIC 9(9) USAGE BINARY.
000318        03 UDF-DBINFO-UDBCS   PIC 9(9) USAGE BINARY.
000319        03 UDF-DBINFO-UMIXED  PIC 9(9) USAGE BINARY.
000320        03 UDF-DBINFO-RESERV0 PIC X(8).
000321*       Schema length and name
000322     02 UDF-DBINFO-SCHEMA0.
000323        49 UDF-DBINFO-SLEN PIC 9(4) USAGE BINARY.
000324        49 UDF-DBINFO-SCHEMA PIC X(128).
000325*       Table length and name
000326     02 UDF-DBINFO-TABLE0.
000327        49 UDF-DBINFO-TLEN PIC 9(4) USAGE BINARY.
000328        49 UDF-DBINFO-TABLE  PIC X(128).
000329*       Column length and name
000330     02 UDF-DBINFO-COLUMN0.
000331        49 UDF-DBINFO-CLEN PIC 9(4) USAGE BINARY.
000332        49 UDF-DBINFO-COLUMN  PIC X(128).
000333*       DB2 release level
000334     02 UDF-DBINFO-VERREL PIC X(8).
000335*       Unused
000336     02 FILLER            PIC X(2).
000337*       Database Platform
000338     02 UDF-DBINFO-PLATFORM PIC 9(9) USAGE BINARY.
000339*       # of entries in Table Function column list
000340     02 UDF-DBINFO-NUMTFCOL PIC 9(4) USAGE BINARY.
000341*       reserved
000342     02 UDF-DBINFO-RESERV1 PIC X(24).
000343*       Unused
000344     02 FILLER            PIC X(2).
000345*       Pointer to Table Function column list
000346     02 UDF-DBINFO-TFCOLUMN PIC 9(9) USAGE BINARY.
000347*       Pointer to Application ID
000348     02 UDF-DBINFO-APPLID  PIC 9(9) USAGE BINARY.
000349*       reserved
000350     02 UDF-DBINFO-RESERV2 PIC X(20).

