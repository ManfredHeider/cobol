      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  08/03/12
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTOK0019.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTOK0019.CPv  $
      *
      *   Rev 2.1   May 29 2012 10:22:10   t13ex27
      *AOK00047: Auswertung Einarbeitungszeiten und Satzstatistik-D
      *B und OZPs
      *
      *   Rev 2.0   Mar 8 2012 15:49:33   T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE OKIEINARBEITUNG TABLE
CCC004*    ( OKEJAHR                        SMALLINT NOT NULL,
CCC004*      OKEMONAT                       SMALLINT NOT NULL,
CCC004*      OKE_SQL_ANZ_GES                INTEGER NOT NULL,
CCC004*      OKE_SATZ_ANZ_GES               INTEGER NOT NULL,
CCC004*      OKE_DAUER_10000                SMALLINT NOT NULL,
CCC004*      OKE_DAUER_GES                  INTEGER NOT NULL,
CCC004*      OKE_AKKEY_MAX                  TIMESTAMP NOT NULL,
CCC004*      OKE_SQL_MAX                    INTEGER NOT NULL,
CCC004*      OKE_SATZ_MAX                   INTEGER NOT NULL,
CCC004*      OKEAKKEY_LOG_EIN               TIMESTAMP NOT NULL,
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE OKIEINARBEITUNG             *
      ******************************************************************
       01  OKIEINARBEITUNG-GESAMT.
        05 NI-OKIEINARBEITUNG-TAB.
           10 NI-OKIEINARBEITUNG-ELE PIC S9(04) COMP OCCURS 11 TIMES.
        05 NI-OKIEINARBEITUNG REDEFINES NI-OKIEINARBEITUNG-TAB.
           10 NI-OKEJAHR            PIC S9(4) COMP.
           10 NI-OKEMONAT           PIC S9(4) COMP.
           10 NI-OKE-SQL-ANZ-GES    PIC S9(4) COMP.
           10 NI-OKE-SATZ-ANZ-GES   PIC S9(4) COMP.
           10 NI-OKE-DAUER-10000    PIC S9(4) COMP.
           10 NI-OKE-DAUER-GES      PIC S9(4) COMP.
           10 NI-OKE-AKKEY-MAX      PIC S9(4) COMP.
           10 NI-OKE-SQL-MAX        PIC S9(4) COMP.
           10 NI-OKE-SATZ-MAX       PIC S9(4) COMP.
           10 NI-OKEAKKEY-LOG-EIN   PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.
        05 DCL-OKIEINARBEITUNG.
           10 OKEJAHR              PIC S9(4) USAGE COMP.
           10 OKEMONAT             PIC S9(4) USAGE COMP.
           10 OKE-SQL-ANZ-GES      PIC S9(9) USAGE COMP.
           10 OKE-SATZ-ANZ-GES     PIC S9(9) USAGE COMP.
           10 OKE-DAUER-10000      PIC S9(4) USAGE COMP.
           10 OKE-DAUER-GES        PIC S9(9) USAGE COMP.
           10 OKE-AKKEY-MAX        PIC X(26).
           10 OKE-SQL-MAX          PIC S9(9) USAGE COMP.
           10 OKE-SATZ-MAX         PIC S9(9) USAGE COMP.
           10 OKEAKKEY-LOG-EIN     PIC X(26).
           10 LETZTEAENDERUNG      PIC X(26).
      ************************** ENDE-DCLGEN *************************
