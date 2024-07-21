      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  09.07.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0437.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/CTKI0437.CPv  $
      *
      *   Rev 2.3   Nov 21 2013 10:13:04   t13ex52
      *FKI01434: Ersetzen der ObjektId durch die VertragsId in KFZ-
      *Tabellen
      *
      *   Rev 2.2   15 Sep 2008 11:24:26   t13ex27
      *AKI00731: Bestandsdatenlieferung Kfz Tarifierung-DB-Änderung
      *
      *   Rev 2.1   10 Jul 2008 10:58:50   t13ex27
      *AKI00731: Bestandsdatenlieferung Kfz Tarifierung-DB-Änderung
      *
      *   Rev 2.0   09 Jul 2008 11:10:20   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE KFZZUKUENFTBEITR
      ******************************************************************
CCC005*    EXEC SQL DECLARE KFZZUKUENFTBEITR TABLE
CCC004*    ( KFZZUKVERTRAGSID               CHAR(13) NOT NULL,
CCC004*      KFZZUKLFDNR                    SMALLINT NOT NULL,
CCC004*      KFZZUKSPARTE                   CHAR(6),
CCC004*      KFZZUKBEITRAGNEU               DECIMAL(7, 2),
CCC004*      KFZZUKSFJANZNEU                SMALLINT,
CCC004*      KFZZUKSFJANZALT                SMALLINT
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE KFZZUKUENFTBEITR            *
      ******************************************************************
       01  KFZZUKUENFTBEITR-GESAMT.
        05 NI-KFZZUKUENFTBEITR-TAB.
           10 NI-KFZZUKUENFTBEITR-ELE PIC S9(04) COMP OCCURS 6 TIMES.
        05 NI-KFZZUKUENFTBEITR REDEFINES NI-KFZZUKUENFTBEITR-TAB.
           10 NI-KFZZUKVERTRAGSID   PIC S9(4) COMP.
           10 NI-KFZZUKLFDNR        PIC S9(4) COMP.
           10 NI-KFZZUKSPARTE       PIC S9(4) COMP.
           10 NI-KFZZUKBEITRAGNEU   PIC S9(4) COMP.
           10 NI-KFZZUKSFJANZNEU    PIC S9(4) COMP.
           10 NI-KFZZUKSFJANZALT    PIC S9(4) COMP.
        05 DCL-KFZZUKUENFTBEITR.
           10 KFZZUKVERTRAGSID     PIC X(13).
           10 KFZZUKLFDNR          PIC S9(4) USAGE COMP.
           10 KFZZUKSPARTE         PIC X(6).
           10 KFZZUKBEITRAGNEU     PIC S9(5)V9(2) USAGE COMP-3.
           10 KFZZUKSFJANZNEU      PIC S9(4) USAGE COMP.
           10 KFZZUKSFJANZALT      PIC S9(4) USAGE COMP.
      ************************** ENDE-DCLGEN *************************
