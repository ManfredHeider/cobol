      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27/02/13
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0455.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/CTKI0455.CPv  $
      *
      *   Rev 2.3   Jun 13 2013 13:07:12   t13ex27
      *AKI01204: SEPA - DB-Online - OZP für BTGZAHLERVTG
      *
      *   Rev 2.1   Feb 27 2013 13:59:58   t13ex14
      *AKI01204: SEPA - DB-Online - OZPs für SEPA-Tabellen
      *
      *   Rev 2.0   Feb 27 2013 11:27:02   T13EX14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE BTGZAHLERVTG TABLE
CCC004*    ( BZVVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      BZVLFDNR                       SMALLINT NOT NULL,
CCC004*      BZVKNDPERSONENID               CHAR(13),
CCC004*      BZVNACHNAME                    CHAR(30),
CCC004*      BZVVORNAME                     CHAR(30),
CCC004*      BZVGEBDATUM                    DATE,
CCC004*      BZVADRESSEID                   CHAR(13),
CCC004*      BZVANREDE_KTY                  SMALLINT,
CCC004*      BZVANREDE_KNR                  SMALLINT,
CCC004*      BZVIBAN                        CHAR(34),
CCC004*      BZVBIC                         CHAR(11),
CCC004*      BZVABWBTGZAHLER                CHAR(1) NOT NULL,
CCC004*      BZVMANDATREFNR                 VARCHAR(35),
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE BTGZAHLERVTG                *
      ******************************************************************
       01  BTGZAHLERVTG-GESAMT.
        05 NI-BTGZAHLERVTG-TAB.
           10 NI-BTGZAHLERVTG-ELE PIC S9(04) COMP OCCURS 14 TIMES.
        05 NI-BTGZAHLERVTG REDEFINES NI-BTGZAHLERVTG-TAB.
           10 NI-BZVVERTRAGSID      PIC S9(4) COMP.
           10 NI-BZVLFDNR           PIC S9(4) COMP.
           10 NI-BZVKNDPERSONENID   PIC S9(4) COMP.
           10 NI-BZVNACHNAME        PIC S9(4) COMP.
           10 NI-BZVVORNAME         PIC S9(4) COMP.
           10 NI-BZVGEBDATUM        PIC S9(4) COMP.
           10 NI-BZVADRESSEID       PIC S9(4) COMP.
           10 NI-BZVANREDE-KTY      PIC S9(4) COMP.
           10 NI-BZVANREDE-KNR      PIC S9(4) COMP.
           10 NI-BZVIBAN            PIC S9(4) COMP.
           10 NI-BZVBIC             PIC S9(4) COMP.
           10 NI-BZVABWBTGZAHLER    PIC S9(4) COMP.
           10 NI-BZVMANDATREFNR     PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.
        05 DCL-BTGZAHLERVTG.
           10 BZVVERTRAGSID        PIC X(13).
           10 BZVLFDNR             PIC S9(4) USAGE COMP.
           10 BZVKNDPERSONENID     PIC X(13).
           10 BZVNACHNAME          PIC X(30).
           10 BZVVORNAME           PIC X(30).
           10 BZVGEBDATUM          PIC X(10).
           10 BZVADRESSEID         PIC X(13).
           10 BZVANREDE-KTY        PIC S9(4) USAGE COMP.
           10 BZVANREDE-KNR        PIC S9(4) USAGE COMP.
           10 BZVIBAN              PIC X(34).
           10 BZVBIC               PIC X(11).
           10 BZVABWBTGZAHLER      PIC X(1).
           10 BZVMANDATREFNR.
              49 BZVMANDATREFNR-LEN
                 PIC S9(4) USAGE COMP.
              49 BZVMANDATREFNR-TEXT
                 PIC X(35).
           10 LETZTEAENDERUNG      PIC X(26).
      ************************** ENDE-DCLGEN *************************
