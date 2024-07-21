      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  23.11.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRDB0002.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRDB0002.CPv  $
      *
      *   Rev 2.1   Nov 24 2004 14:44:04   p13sp14
      *ADB00022: Groﬂ/KLeinschreibung
      *
      *   Rev 2.0   Nov 23 2004 16:40:44   p13sp14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
036516*********************************************************
036517* Declare the SQLSTATE that can be set by the           *
036518*********************************************************
036519 01  UDF-SQLSTATE PIC X(5).
036520*********************************************************
036521* Declare the qualified function name                   *
036522*********************************************************
036523 01  UDF-FUNC.
036524     49 UDF-FUNC-LEN PIC 9(4) USAGE BINARY.
036525     49 UDF-FUNC-TEXT PIC X(137).
036526*********************************************************
036527* Declare the specific function name                    *
036528*********************************************************
036529 01  UDF-SPEC.
036530     49 UDF-SPEC-LEN PIC 9(4) USAGE BINARY.
036531     49 UDF-SPEC-TEXT PIC X(128).
036532*********************************************************
036533* Declare SQL diagnostic message token                  *
036534*********************************************************
036535 01  UDF-DIAG.
036536     49 UDF-DIAG-LEN PIC 9(4) USAGE BINARY.
036537     49 UDF-DIAG-TEXT PIC X(70).
036538*********************************************************
036539* Declare the scratchpad                                *
036540*********************************************************
036541 01  UDF-SCRATCHPAD.
036542     49 UDF-SPAD-LEN PIC 9(4) USAGE BINARY.
036543     49 UDF-SPAD-TEXT PIC X(1000).
036549*********************************************************
036550* Declare the call type                                 *
036551*********************************************************
036552*01  UDF-CALL-TYPE PIC 9(9) USAGE BINARY.
036553 01  UDF-CALL-TYPE PIC S9(9) USAGE BINARY.
036554*****************************************************
036555* CONSTANTS FOR DB2-EBCODING-SCHEME.                *
036556*****************************************************
036557 77 SQLUDF-ASCII PIC 9(9) VALUE 1.
036558 77 SQLUDF-EBCDIC PIC 9(9) VALUE 2.
036559 77 SQLUDF-UNICODE PIC 9(9) VALUE 3.
036560*********************************************************
036561* Declare the DBINFO structure                          *
036562*********************************************************
036563 01  UDF-DBINFO.
036564*       Location length and name
036565     02 UDF-DBINFO-LOCATION.
036566        49 UDF-DBINFO-LLEN PIC 9(4) USAGE BINARY.
036567        49 UDF-DBINFO-LOC  PIC X(128).
036568*       Authorization ID length and name
036569     02 UDF-DBINFO-AUTHORIZATION.
036570        49 UDF-DBINFO-ALEN PIC 9(4) USAGE BINARY.
036571        49 UDF-DBINFO-AUTH PIC X(128).
036572*       CCSIDs for DB2 for OS/390
036573     02 UDF-DBINFO-CCSID PIC X(48).
036574     02 UDF-DBINFO-CDPG REDEFINES UDF-DBINFO-CCSID.
036575        03 DB2-CCSIDS OCCURS 3 TIMES.
036576           04 DB2-SBCS   PIC 9(9) USAGE BINARY.
036577           04 DB2-DBCS   PIC 9(9) USAGE BINARY.
036578           04 DB2-MIXED  PIC 9(9) USAGE BINARY.
036579        03 DB2-ENCODING-SCHEME   PIC 9(9) USAGE BINARY.
036580        03 DB2-CCSID-RESERVED    PIC 9(9) USAGE BINARY.
036581        03 DB2-CCSID-RESERVED-1  PIC 9(9) USAGE BINARY.
036582*       Schema length and name
036583     02 UDF-DBINFO-SCHEMA0.
036584        49 UDF-DBINFO-SLEN PIC 9(4) USAGE BINARY.
036585        49 UDF-DBINFO-SCHEMA PIC X(128).
036586*       Table length and name
036587     02 UDF-DBINFO-TABLE0.
036588        49 UDF-DBINFO-TLEN PIC 9(4) USAGE BINARY.
036589        49 UDF-DBINFO-TABLE  PIC X(128).
036590*       Column length and name
036591     02 UDF-DBINFO-COLUMN0.
036592        49 UDF-DBINFO-CLEN PIC 9(4) USAGE BINARY.
036593        49 UDF-DBINFO-COLUMN  PIC X(128).
036594*       DB2 release level
036595     02 UDF-DBINFO-VERREL PIC X(8).
036596*       Unused
036597     02 FILLER            PIC X(2).
036598*       Database Platform
036599     02 UDF-DBINFO-PLATFORM PIC 9(9) USAGE BINARY.
036600*       # of entries in Table Function column list
036601     02 UDF-DBINFO-NUMTFCOL PIC 9(4) USAGE BINARY.
036602*       reserved
036603     02 UDF-DBINFO-RESERV1 PIC X(24).
036604*       Unused
036605     02 FILLER            PIC X(2).
036606*       Pointer to Table Function column list
036607     02 UDF-DBINFO-TFCOLUMN PIC 9(9) USAGE BINARY.
036608*       Pointer to Application ID
036609     02 UDF-DBINFO-APPLID  PIC 9(9) USAGE BINARY.
036610*       reserved
036611     02 UDF-DBINFO-RESERV2 PIC X(20).
