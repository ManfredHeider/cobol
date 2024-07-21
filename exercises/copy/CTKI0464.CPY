      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  11/07/13
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0464.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTKI0464.CPv  $
      *
      *   Rev 2.1   Aug 21 2013 10:49:16   t13ex19
      *AKI01204: SEPA - DB-Online - OZPs für TCKI0458 / 0459
      *
      *   Rev 2.0   Jul 11 2013 16:33:39   T13EX19
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE BTGZAHLERKREDITK TABLE
CCC004*    ( BZKVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      BZKGHPLFDNR                    SMALLINT NOT NULL,
CCC004*      BZKDBPLFDNR                    SMALLINT NOT NULL,
CCC004*      BZKLFDNR                       SMALLINT NOT NULL,
CCC004*      BZKKNDPERSONENID               CHAR(13),
CCC004*      BZKNACHNAME                    CHAR(30),
CCC004*      BZKVORNAME                     CHAR(30),
CCC004*      BZKGEBDATUM                    DATE,
CCC004*      BZKADRESSEID                   CHAR(13),
CCC004*      BZKANREDE_KTY                  SMALLINT,
CCC004*      BZKANREDE_KNR                  SMALLINT,
CCC004*      BZKIBAN                        CHAR(34),
CCC004*      BZKBIC                         CHAR(11),
CCC004*      BZKABWBTGZAHLER                CHAR(1),
CCC004*      BZKMANDATREFNR                 VARCHAR(35),
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE BTGZAHLERKREDITK            *
      ******************************************************************
       01  BTGZAHLERKREDITK-GESAMT.
        05 NI-BTGZAHLERKREDITK-TAB.
           10 NI-BTGZAHLERKREDITK-ELE PIC S9(04) COMP OCCURS 16 TIMES.
        05 NI-BTGZAHLERKREDITK REDEFINES NI-BTGZAHLERKREDITK-TAB.
           10 NI-BZKVERTRAGSID      PIC S9(4) COMP.
           10 NI-BZKGHPLFDNR        PIC S9(4) COMP.
           10 NI-BZKDBPLFDNR        PIC S9(4) COMP.
           10 NI-BZKLFDNR           PIC S9(4) COMP.
           10 NI-BZKKNDPERSONENID   PIC S9(4) COMP.
           10 NI-BZKNACHNAME        PIC S9(4) COMP.
           10 NI-BZKVORNAME         PIC S9(4) COMP.
           10 NI-BZKGEBDATUM        PIC S9(4) COMP.
           10 NI-BZKADRESSEID       PIC S9(4) COMP.
           10 NI-BZKANREDE-KTY      PIC S9(4) COMP.
           10 NI-BZKANREDE-KNR      PIC S9(4) COMP.
           10 NI-BZKIBAN            PIC S9(4) COMP.
           10 NI-BZKBIC             PIC S9(4) COMP.
           10 NI-BZKABWBTGZAHLER    PIC S9(4) COMP.
           10 NI-BZKMANDATREFNR     PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.
        05 DCL-BTGZAHLERKREDITK.
           10 BZKVERTRAGSID        PIC X(13).
           10 BZKGHPLFDNR          PIC S9(4) USAGE COMP.
           10 BZKDBPLFDNR          PIC S9(4) USAGE COMP.
           10 BZKLFDNR             PIC S9(4) USAGE COMP.
           10 BZKKNDPERSONENID     PIC X(13).
           10 BZKNACHNAME          PIC X(30).
           10 BZKVORNAME           PIC X(30).
           10 BZKGEBDATUM          PIC X(10).
           10 BZKADRESSEID         PIC X(13).
           10 BZKANREDE-KTY        PIC S9(4) USAGE COMP.
           10 BZKANREDE-KNR        PIC S9(4) USAGE COMP.
           10 BZKIBAN              PIC X(34).
           10 BZKBIC               PIC X(11).
           10 BZKABWBTGZAHLER      PIC X(1).
           10 BZKMANDATREFNR.
              49 BZKMANDATREFNR-LEN
                 PIC S9(4) USAGE COMP.
              49 BZKMANDATREFNR-TEXT
                 PIC X(35).
           10 LETZTEAENDERUNG      PIC X(26).
      ************************** ENDE-DCLGEN *************************
