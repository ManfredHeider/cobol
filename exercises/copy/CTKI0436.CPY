      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  09.07.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0436.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CTKI0436.CPv  $
      *
      *   Rev 2.2   Nov 21 2013 10:13:00   t13ex52
      *FKI01434: Ersetzen der ObjektId durch die VertragsId in KFZ-
      *Tabellen
      *
      *   Rev 2.1   10 Jul 2008 10:58:44   t13ex27
      *AKI00731: Bestandsdatenlieferung Kfz Tarifierung-DB-Änderung
      *
      *   Rev 2.0   09 Jul 2008 11:10:12   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE KFZSONDAUSSTATT
      ******************************************************************
CCC005*    EXEC SQL DECLARE KFZSONDAUSSTATT TABLE
CCC004*    ( KFZSONVERTRAGSID               CHAR(13) NOT NULL,
CCC004*      KFZSONLFDNR                    SMALLINT NOT NULL,
CCC004*      KFZSONTEILNR_KTY               SMALLINT NOT NULL,
CCC004*      KFZSONTEILNR_KNR               DECIMAL(15, 0) NOT NULL,
CCC004*      KFZSONTEILEWERT                DECIMAL(9, 2)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE KFZSONDAUSSTATT             *
      ******************************************************************
       01  KFZSONDAUSSTATT-GESAMT.
        05 NI-KFZSONDAUSSTATT-TAB.
           10 NI-KFZSONDAUSSTATT-ELE PIC S9(04) COMP OCCURS 5 TIMES.
        05 NI-KFZSONDAUSSTATT REDEFINES NI-KFZSONDAUSSTATT-TAB.
           10 NI-KFZSONVERTRAGSID   PIC S9(4) COMP.
           10 NI-KFZSONLFDNR        PIC S9(4) COMP.
           10 NI-KFZSONTEILNR-KTY   PIC S9(4) COMP.
           10 NI-KFZSONTEILNR-KNR   PIC S9(4) COMP.
           10 NI-KFZSONTEILEWERT    PIC S9(4) COMP.
        05 DCL-KFZSONDAUSSTATT.
           10 KFZSONVERTRAGSID     PIC X(13).
           10 KFZSONLFDNR          PIC S9(4) USAGE COMP.
           10 KFZSONTEILNR-KTY     PIC S9(4) USAGE COMP.
           10 KFZSONTEILNR-KNR     PIC S9(15)V USAGE COMP-3.
           10 KFZSONTEILEWERT      PIC S9(7)V9(2) USAGE COMP-3.
      ************************** ENDE-DCLGEN *************************
