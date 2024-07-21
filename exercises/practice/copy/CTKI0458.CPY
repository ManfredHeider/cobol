      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27/02/13
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0458.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CTKI0458.CPv  $
      *
      *   Rev 2.2   Aug 21 2013 10:47:48   t13ex19
      *AKI01204: SEPA - DB-Online - OZPs für TCKI0458 / 0459
      *
      *   Rev 2.1   Feb 27 2013 14:00:04   t13ex14
      *AKI01204: SEPA - DB-Online - OZPs für SEPA-Tabellen
      *
      *   Rev 2.0   Feb 27 2013 11:45:08   T13EX14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE BTGZAHLERDEPOT TABLE
CCC004*    ( BZDVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      BZDGHPLFDNR                    SMALLINT NOT NULL,
CCC004*      BZDLFDNR                       SMALLINT NOT NULL,
CCC004*      BZDKNDPERSONENID               CHAR(13),
CCC004*      BZDNACHNAME                    CHAR(30),
CCC004*      BZDVORNAME                     CHAR(30),
CCC004*      BZDGEBDATUM                    DATE,
CCC004*      BZDADRESSEID                   CHAR(13),
CCC004*      BZDANREDE_KTY                  SMALLINT,
CCC004*      BZDANREDE_KNR                  SMALLINT,
CCC004*      BZDIBAN                        CHAR(34),
CCC004*      BZDBIC                         CHAR(11),
CCC004*      BZDABWBTGZAHLER                CHAR(1),
CCC004*      BZDMANDATREFNR                 VARCHAR(35),
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE BTGZAHLERDEPOT              *
      ******************************************************************
       01  BTGZAHLERDEPOT-GESAMT.
        05 NI-BTGZAHLERDEPOT-TAB.
           10 NI-BTGZAHLERDEPOT-ELE PIC S9(04) COMP OCCURS 15 TIMES.
        05 NI-BTGZAHLERDEPOT REDEFINES NI-BTGZAHLERDEPOT-TAB.
           10 NI-BZDVERTRAGSID      PIC S9(4) COMP.
           10 NI-BZDGHPLFDNR        PIC S9(4) COMP.
           10 NI-BZDLFDNR           PIC S9(4) COMP.
           10 NI-BZDKNDPERSONENID   PIC S9(4) COMP.
           10 NI-BZDNACHNAME        PIC S9(4) COMP.
           10 NI-BZDVORNAME         PIC S9(4) COMP.
           10 NI-BZDGEBDATUM        PIC S9(4) COMP.
           10 NI-BZDADRESSEID       PIC S9(4) COMP.
           10 NI-BZDANREDE-KTY      PIC S9(4) COMP.
           10 NI-BZDANREDE-KNR      PIC S9(4) COMP.
           10 NI-BZDIBAN            PIC S9(4) COMP.
           10 NI-BZDBIC             PIC S9(4) COMP.
           10 NI-BZDABWBTGZAHLER    PIC S9(4) COMP.
           10 NI-BZDMANDATREFNR     PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.
        05 DCL-BTGZAHLERDEPOT.
           10 BZDVERTRAGSID        PIC X(13).
           10 BZDGHPLFDNR          PIC S9(4) USAGE COMP.
           10 BZDLFDNR             PIC S9(4) USAGE COMP.
           10 BZDKNDPERSONENID     PIC X(13).
           10 BZDNACHNAME          PIC X(30).
           10 BZDVORNAME           PIC X(30).
           10 BZDGEBDATUM          PIC X(10).
           10 BZDADRESSEID         PIC X(13).
           10 BZDANREDE-KTY        PIC S9(4) USAGE COMP.
           10 BZDANREDE-KNR        PIC S9(4) USAGE COMP.
           10 BZDIBAN              PIC X(34).
           10 BZDBIC               PIC X(11).
           10 BZDABWBTGZAHLER      PIC X(1).
           10 BZDMANDATREFNR.
              49 BZDMANDATREFNR-LEN
                 PIC S9(4) USAGE COMP.
              49 BZDMANDATREFNR-TEXT
                 PIC X(35).
           10 LETZTEAENDERUNG      PIC X(26).
      ************************** ENDE-DCLGEN *************************
