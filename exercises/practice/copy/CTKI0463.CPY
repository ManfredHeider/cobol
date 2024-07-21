      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  11/07/13
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0463.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTKI0463.CPv  $
      *
      *   Rev 2.1   Aug 21 2013 10:49:10   t13ex19
      *AKI01204: SEPA - DB-Online - OZPs für TCKI0458 / 0459
      *
      *   Rev 2.0   Jul 11 2013 16:29:11   T13EX19
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE BTGZAHLERBANKINGP TABLE
CCC004*    ( BZBDBPVERTRAGSID               CHAR(13) NOT NULL,
CCC004*      BZBDBPGHPLFDNR                 SMALLINT NOT NULL,
CCC004*      BZBDBPLFDNR                    SMALLINT NOT NULL,
CCC004*      BZBLFDNR                       SMALLINT NOT NULL,
CCC004*      BZBKNDPERSONENID               CHAR(13),
CCC004*      BZBNACHNAME                    CHAR(30),
CCC004*      BZBVORNAME                     CHAR(30),
CCC004*      BZBGEBDATUM                    DATE,
CCC004*      BZBADRESSEID                   CHAR(13),
CCC004*      BZBANREDE_KTY                  SMALLINT,
CCC004*      BZBANREDE_KNR                  SMALLINT,
CCC004*      BZBIBAN                        CHAR(34),
CCC004*      BZBBIC                         CHAR(11),
CCC004*      BZBABWBTGZAHLER                CHAR(1),
CCC004*      BZBMANDATREFNR                 VARCHAR(35),
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE BTGZAHLERBANKINGP           *
      ******************************************************************
       01  BTGZAHLERBANKINGP-GESAMT.
        05 NI-BTGZAHLERBANKINGP-TAB.
           10 NI-BTGZAHLERBANKINGP-ELE PIC S9(04) COMP OCCURS 16 TIMES.
        05 NI-BTGZAHLERBANKINGP REDEFINES NI-BTGZAHLERBANKINGP-TAB.
           10 NI-BZBDBPVERTRAGSID   PIC S9(4) COMP.
           10 NI-BZBDBPGHPLFDNR     PIC S9(4) COMP.
           10 NI-BZBDBPLFDNR        PIC S9(4) COMP.
           10 NI-BZBLFDNR           PIC S9(4) COMP.
           10 NI-BZBKNDPERSONENID   PIC S9(4) COMP.
           10 NI-BZBNACHNAME        PIC S9(4) COMP.
           10 NI-BZBVORNAME         PIC S9(4) COMP.
           10 NI-BZBGEBDATUM        PIC S9(4) COMP.
           10 NI-BZBADRESSEID       PIC S9(4) COMP.
           10 NI-BZBANREDE-KTY      PIC S9(4) COMP.
           10 NI-BZBANREDE-KNR      PIC S9(4) COMP.
           10 NI-BZBIBAN            PIC S9(4) COMP.
           10 NI-BZBBIC             PIC S9(4) COMP.
           10 NI-BZBABWBTGZAHLER    PIC S9(4) COMP.
           10 NI-BZBMANDATREFNR     PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.
        05 DCL-BTGZAHLERBANKINGP.
           10 BZBDBPVERTRAGSID     PIC X(13).
           10 BZBDBPGHPLFDNR       PIC S9(4) USAGE COMP.
           10 BZBDBPLFDNR          PIC S9(4) USAGE COMP.
           10 BZBLFDNR             PIC S9(4) USAGE COMP.
           10 BZBKNDPERSONENID     PIC X(13).
           10 BZBNACHNAME          PIC X(30).
           10 BZBVORNAME           PIC X(30).
           10 BZBGEBDATUM          PIC X(10).
           10 BZBADRESSEID         PIC X(13).
           10 BZBANREDE-KTY        PIC S9(4) USAGE COMP.
           10 BZBANREDE-KNR        PIC S9(4) USAGE COMP.
           10 BZBIBAN              PIC X(34).
           10 BZBBIC               PIC X(11).
           10 BZBABWBTGZAHLER      PIC X(1).
           10 BZBMANDATREFNR.
              49 BZBMANDATREFNR-LEN
                 PIC S9(4) USAGE COMP.
              49 BZBMANDATREFNR-TEXT
                 PIC X(35).
           10 LETZTEAENDERUNG      PIC X(26).
      ************************** ENDE-DCLGEN *************************
