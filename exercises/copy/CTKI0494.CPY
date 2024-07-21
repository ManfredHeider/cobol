      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27/02/18
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0494.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTKI0494.CPv  $
      *
      *   Rev 2.1   Jun 11 2018 14:39:48   t13ex27
      *AKI01570: FW - Generali-Partnergesellschaften-E/A/A-Kunden
      *
      *   Rev 2.0   Feb 27 2018 14:21:15   T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE GENUNFVERSPERSON TABLE
CCC004*    ( GUPVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      GUPPRODLFDNR                   SMALLINT NOT NULL,
CCC004*      GUPLFDNR                       SMALLINT NOT NULL,
CCC004*      GUPNACHNAME                    CHAR(40),
CCC004*      GUPVORNAME                     CHAR(30),
CCC004*      GUPGEBDATUM                    DATE,
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE GENUNFVERSPERSON            *
      ******************************************************************
       01  GENUNFVERSPERSON-GESAMT.
        05 NI-GENUNFVERSPERSON-TAB.
           10 NI-GENUNFVERSPERSON-ELE PIC S9(04) COMP OCCURS 7 TIMES.
        05 NI-GENUNFVERSPERSON REDEFINES NI-GENUNFVERSPERSON-TAB.
           10 NI-GUPVERTRAGSID      PIC S9(4) COMP.
           10 NI-GUPPRODLFDNR       PIC S9(4) COMP.
           10 NI-GUPLFDNR           PIC S9(4) COMP.
           10 NI-GUPNACHNAME        PIC S9(4) COMP.
           10 NI-GUPVORNAME         PIC S9(4) COMP.
           10 NI-GUPGEBDATUM        PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.
        05 DCL-GENUNFVERSPERSON.
           10 GUPVERTRAGSID        PIC X(13).
           10 GUPPRODLFDNR         PIC S9(4) USAGE COMP.
           10 GUPLFDNR             PIC S9(4) USAGE COMP.
           10 GUPNACHNAME          PIC X(40).
           10 GUPVORNAME           PIC X(30).
           10 GUPGEBDATUM          PIC X(10).
           10 LETZTEAENDERUNG      PIC X(26).
      ************************** ENDE-DCLGEN *************************

