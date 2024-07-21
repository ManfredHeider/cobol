      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27/02/13
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0459.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CTKI0459.CPv  $
      *
      *   Rev 2.2   Aug 21 2013 10:47:54   t13ex19
      *AKI01204: SEPA - DB-Online - OZPs für TCKI0458 / 0459
      *
      *   Rev 2.1   Feb 27 2013 14:00:12   t13ex14
      *AKI01204: SEPA - DB-Online - OZPs für SEPA-Tabellen
      *
      *   Rev 2.0   Feb 27 2013 11:55:19   T13EX14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE BTGZAHLERSPARPLAN TABLE
CCC004*    ( BZSVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      BZSGHPLFDNR                    SMALLINT NOT NULL,
CCC004*      BZSGHZFONDSID                  SMALLINT NOT NULL,
CCC004*      BZSGHZLFDNR                    SMALLINT NOT NULL,
CCC004*      BZSLFDNR                       SMALLINT NOT NULL,
CCC004*      BZSKNDPERSONENID               CHAR(13),
CCC004*      BZSNACHNAME                    CHAR(30),
CCC004*      BZSVORNAME                     CHAR(30),
CCC004*      BZSGEBDATUM                    DATE,
CCC004*      BZSADRESSEID                   CHAR(13),
CCC004*      BZSANREDE_KTY                  SMALLINT,
CCC004*      BZSANREDE_KNR                  SMALLINT,
CCC004*      BZSIBAN                        CHAR(34),
CCC004*      BZSBIC                         CHAR(11),
CCC004*      BZSABWBTGZAHLER                CHAR(1),
CCC004*      BZSMANDATREFNR                 VARCHAR(35),
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE BTGZAHLERSPARPLAN           *
      ******************************************************************
       01  BTGZAHLERSPARPLAN-GESAMT.
        05 NI-BTGZAHLERSPARPLAN-TAB.
           10 NI-BTGZAHLERSPARPLAN-ELE PIC S9(04) COMP OCCURS 17 TIMES.
        05 NI-BTGZAHLERSPARPLAN REDEFINES NI-BTGZAHLERSPARPLAN-TAB.
           10 NI-BZSVERTRAGSID      PIC S9(4) COMP.
           10 NI-BZSGHPLFDNR        PIC S9(4) COMP.
           10 NI-BZSGHZFONDSID      PIC S9(4) COMP.
           10 NI-BZSGHZLFDNR        PIC S9(4) COMP.
           10 NI-BZSLFDNR           PIC S9(4) COMP.
           10 NI-BZSKNDPERSONENID   PIC S9(4) COMP.
           10 NI-BZSNACHNAME        PIC S9(4) COMP.
           10 NI-BZSVORNAME         PIC S9(4) COMP.
           10 NI-BZSGEBDATUM        PIC S9(4) COMP.
           10 NI-BZSADRESSEID       PIC S9(4) COMP.
           10 NI-BZSANREDE-KTY      PIC S9(4) COMP.
           10 NI-BZSANREDE-KNR      PIC S9(4) COMP.
           10 NI-BZSIBAN            PIC S9(4) COMP.
           10 NI-BZSBIC             PIC S9(4) COMP.
           10 NI-BZSABWBTGZAHLER    PIC S9(4) COMP.
           10 NI-BZSMANDATREFNR     PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.
        05 DCL-BTGZAHLERSPARPLAN.
           10 BZSVERTRAGSID        PIC X(13).
           10 BZSGHPLFDNR          PIC S9(4) USAGE COMP.
           10 BZSGHZFONDSID        PIC S9(4) USAGE COMP.
           10 BZSGHZLFDNR          PIC S9(4) USAGE COMP.
           10 BZSLFDNR             PIC S9(4) USAGE COMP.
           10 BZSKNDPERSONENID     PIC X(13).
           10 BZSNACHNAME          PIC X(30).
           10 BZSVORNAME           PIC X(30).
           10 BZSGEBDATUM          PIC X(10).
           10 BZSADRESSEID         PIC X(13).
           10 BZSANREDE-KTY        PIC S9(4) USAGE COMP.
           10 BZSANREDE-KNR        PIC S9(4) USAGE COMP.
           10 BZSIBAN              PIC X(34).
           10 BZSBIC               PIC X(11).
           10 BZSABWBTGZAHLER      PIC X(1).
           10 BZSMANDATREFNR.
              49 BZSMANDATREFNR-LEN
                 PIC S9(4) USAGE COMP.
              49 BZSMANDATREFNR-TEXT
                 PIC X(35).
           10 LETZTEAENDERUNG      PIC X(26).
      ************************** ENDE-DCLGEN *************************
