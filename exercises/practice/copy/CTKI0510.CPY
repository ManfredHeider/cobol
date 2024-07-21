      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06/09/18
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0510.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/CTKI0510.CPv  $
      *
      *   Rev 2.5   May 17 2019 11:52:54   t13ac20
      *AKI01600: Erweiterung Daisy
      *
      *   Rev 2.4   Nov 28 2018 17:35:18   t13ac20
      *AKI01600: Sperren & Löschen von Verträgen - DB2 neue Tabellen
      *
      *   Rev 2.3   Sep 11 2018 12:18:44   t13ac20
      *AKI01600: Sperren & Löschen von Verträgen - DB2 neue Tabellen
      *
      *   Rev 2.0   Sep 6 2018 08:38:22   T13AC20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE SPERRSST TABLE
CCC004*    ( SPSVERTRAGSNR                  CHAR(15) NOT NULL,
CCC004*      SPSPARTGESELLID                SMALLINT NOT NULL,
CCC004*      SPSLFDNR                       SMALLINT NOT NULL,
CCC004*      SPSVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      SPSERSTELLTTST                 TIMESTAMP NOT NULL,
CCC004*      SPSAKTION_KZN                  CHAR(1) NOT NULL,
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL,
CCC004*      SPSDAISY_KZN                   CHAR(1) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE SPERRSST                    *
      ******************************************************************
       01  SPERRSST-GESAMT.
        05 NI-SPERRSST-TAB.
           10 NI-SPERRSST-ELE PIC S9(04) COMP OCCURS 8 TIMES.
        05 NI-SPERRSST REDEFINES NI-SPERRSST-TAB.
           10 NI-SPSVERTRAGSNR      PIC S9(4) COMP.
           10 NI-SPSPARTGESELLID    PIC S9(4) COMP.
           10 NI-SPSLFDNR           PIC S9(4) COMP.
           10 NI-SPSVERTRAGSID      PIC S9(4) COMP.
           10 NI-SPSERSTELLTTST     PIC S9(4) COMP.
           10 NI-SPSAKTION-KZN      PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.
           10 NI-SPSDAISY-KZN       PIC S9(4) COMP.
        05 DCL-SPERRSST.
           10 SPSVERTRAGSNR        PIC X(15).
           10 SPSPARTGESELLID      PIC S9(4) USAGE COMP.
           10 SPSLFDNR             PIC S9(4) USAGE COMP.
           10 SPSVERTRAGSID        PIC X(13).
           10 SPSERSTELLTTST       PIC X(26).
           10 SPSAKTION-KZN        PIC X(1).
           10 LETZTEAENDERUNG      PIC X(26).
           10 SPSDAISY-KZN         PIC X(1).
      ************************** ENDE-DCLGEN *************************

