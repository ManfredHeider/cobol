      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.10.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0402.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAT0402.CPv  $
      *
      *   Rev 2.1   Oct 28 2004 09:28:02   t13ex22
      *ASR00001: Elektronische Archivierung - PGM für UDF
      *
      *   Rev 2.0   Oct 28 2004 08:16:06   t13ex22
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0402                                      *
      *    --------------------------------------------------------   *
      *    BATCHRAHMEN: FEHLERAUFBEREITUNG UND -AUSGABE (DISPLAYS)    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: MUND                                             *
      *    DATUM:    28.10.2004                                       *
      *****************************************************************
      *    VERSION   1    28.10.04 16:35:20
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASR00001!28.10.04!    ! NEU                       ! MUND     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  SQLUDF-SQLSTATE        PIC X(5).
             88 OK  VALUE '00000'.
             88 EOF VALUE '02000'.
             88 NUL VALUE '38NUL'.
             88 LEN VALUE '38LEN'.
             88 VAL VALUE '38VAL'.
             88 RNG VALUE '38RNG'.
             88 EWM VALUE '38EWM'.
             88 DB2 VALUE '38SQL'.
             88 ERR VALUE '38ERR'.
             88 NIX VALUE '38NIX'.
             88 NA  VALUE '01HNA'.
             88 TRU VALUE '01TRU'.

       01  SQLUDF-FNAME.
           49 SQLUDF-FNAME-LEN      PIC 9(4) USAGE BINARY.
           49 SQLUDF-FNAME-TEXT     PIC X(137).

       01  SQLUDF-FSPECNAME.
           49 SQLUDF-FSPECNAME-LEN  PIC 9(4) USAGE BINARY.
           49 SQLUDF-FSPECNAME-TEXT PIC X(128).

       01  SQLUDF-MSGTEXT.
           49 SQLUDF-MSGTEXT-LEN    PIC 9(4) USAGE BINARY.
           49 SQLUDF-MSGTEXT-TEXT   PIC X(70).

      *  -- OPTIONAL: SCRATCHPAD
      *01  UDF-SCRATCHPAD.
      *    49 UDF-SPAD-LEN PIC 9(9) USAGE BINARY.
      *    49 UDF-SPAD-TEXT PIC X(1).

      *  -- OPTIONAL: SQLUDF-CALL-TYPE
       01  UDF-CALL-TYPE PIC S9(9) USAGE BINARY.
           88 FIRST-CALL VALUE -2.
           88 OPEN-CALL  VALUE -1.
           88 FETCH-CALL VALUE  0.
           88 CLOSE-CALL VALUE +1.
           88 FINAL-CALL VALUE +2.

      *  -- OPTIONAL: DBINFO
       01  UDF-DBINFO.
      *       LOCATION LENGTH AND NAME
           02 UDF-DBINFO-LOCATION.
              49 UDF-DBINFO-LLEN PIC 9(4) USAGE BINARY.
              49 UDF-DBINFO-LOC  PIC X(128).
      *       AUTHORIZATION ID LENGTH AND NAME
           02 UDF-DBINFO-AUTHORIZATION.
              49 UDF-DBINFO-ALEN PIC 9(4) USAGE BINARY.
              49 UDF-DBINFO-AUTH PIC X(128).
      *       CCSIDS FOR DB2 FOR OS/390
           02 UDF-DBINFO-CCSID PIC X(48).
           02 UDF-DBINFO-CDPG REDEFINES UDF-DBINFO-CCSID.
              03 DB2-CCSIDS OCCURS 3 TIMES.
                 04 DB2-SBCS   PIC 9(9) USAGE BINARY.
                 04 DB2-DBCS   PIC 9(9) USAGE BINARY.
                 04 DB2-MIXED  PIC 9(9) USAGE BINARY.
              03 DB2-ENCODING-SCHEME   PIC 9(9) USAGE BINARY.
              03 DB2-CCSID-RESERVED    PIC 9(9) USAGE BINARY.
              03 DB2-CCSID-RESERVED-1  PIC 9(9) USAGE BINARY.
      *       SCHEMA LENGTH AND NAME
           02 UDF-DBINFO-SCHEMA0.
              49 UDF-DBINFO-SLEN PIC 9(4) USAGE BINARY.
              49 UDF-DBINFO-SCHEMA PIC X(128).
      *       TABLE LENGTH AND NAME
           02 UDF-DBINFO-TABLE0.
              49 UDF-DBINFO-TLEN PIC 9(4) USAGE BINARY.
              49 UDF-DBINFO-TABLE  PIC X(128).
      *       COLUMN LENGTH AND NAME
           02 UDF-DBINFO-COLUMN0.
              49 UDF-DBINFO-CLEN PIC 9(4) USAGE BINARY.
              49 UDF-DBINFO-COLUMN  PIC X(128).
      *       DB2 RELEASE LEVEL
           02 UDF-DBINFO-VERREL PIC X(8).
      *       UNUSED
           02 FILLER            PIC X(2).
      *       DATABASE PLATFORM
           02 UDF-DBINFO-PLATFORM PIC 9(9) USAGE BINARY.
      *       # OF ENTRIES IN TABLE FUNCTION COLUMN LIST
           02 UDF-DBINFO-NUMTFCOL PIC 9(4) USAGE BINARY.
      *       RESERVED
           02 UDF-DBINFO-RESERV1 PIC X(24).
      *       UNUSED
           02 FILLER            PIC X(2).
      *       POINTER TO TABLE FUNCTION COLUMN LIST
           02 UDF-DBINFO-TFCOLUMN PIC 9(9) USAGE BINARY.
      *       POINTER TO APPLICATION ID
           02 UDF-DBINFO-APPLID  PIC 9(9) USAGE BINARY.
      *       RESERVED
           02 UDF-DBINFO-RESERV2 PIC X(20).

