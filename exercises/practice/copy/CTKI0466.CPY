      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  09/12/13
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0466.CPY  $
      *    $Revision:   2.9  $
      *
      *    $Log:   L:/mf/CPY/CTKI0466.CPv  $
      *
      *   Rev 2.9   Oct 01 2018 09:36:10   t13ae52
      *AKI01632: G48 - Hinzufügen neuer Kreditkarten
      *
      *   Rev 2.8   Sep 27 2018 16:57:32   t13ae52
      *AKI01632: G48 - Hinzufügen neuer Kreditkarten
      *
      *   Rev 2.7   Jan 25 2018 06:54:52   t13ex39
      *AKI01556: DeuBa zusätzliche  PINs OZPs
      *
      *   Rev 2.5   Nov 06 2015 12:27:20   t13ex73
      *AKI01385: Korrektur der Darstellung des Eigenvermitteltkennz
      *eichens   OZP ZTKI0466
      *
      *   Rev 2.4   Oct 19 2015 13:34:44   t13ex39
      *AKI01361: DeuBa Sparverträge von DBP in BKP
      *W H O
      *
      *   Rev 2.3   Apr 04 2014 11:50:48   T13Ex77
      *FKI01460: Fehlerdateien und Fehlermails DWS
      *
      *   Rev 2.2   Jan 23 2014 15:36:20   i13az40
      *AKI01269: Tabelle DBPRODUKT -> Analog OKI.Kreditkarten_Eigen
      *
      *   Rev 2.1   Jan 14 2014 12:32:52   i13az40
      *AKI01269: Tabelle DBPRODUKT -> Analog OKI.Kreditkarten_Eigen
      *
      *   Rev 2.0   Dec 9 2013 16:32:29   T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE BANKKREDITKARTE TABLE
CCC004*    ( KKAVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      KKAPRODUKTLFDNR                SMALLINT NOT NULL,
CCC004*      KKALFDNR                       SMALLINT NOT NULL,
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL,
CCC004*      KKAPRODUKTID                   SMALLINT NOT NULL,
CCC004*      KKAKARTENART                   CHAR(3),
CCC004*      KKAKARTENNR                    CHAR(16),
CCC004*      KKAHAUPTKARTENNR               CHAR(16),
CCC004*      KKAVNRHAUPTKARTE               CHAR(15),
CCC004*      KKAABRECHKTONR                 CHAR(10),
CCC004*      KKABLZABRECHKTO                CHAR(8),
CCC004*      KKAFILNRABRECHKTO              SMALLINT,
CCC004*      KKAMAHNSTATUS                  CHAR(1),
CCC004*      KKAOEFFNUNGSDAT                DATE,
CCC004*      KKAEIGENVERMKZ                 CHAR(1) NOT NULL,
CCC004*      KKAABSCHLUSSVBNR               INTEGER,
CCC004*      KKAPROVISIONKZ                 CHAR(1),
CCC004*      KKAPIN                         INTEGER NOT NULL,
CCC004*      KKALIMIT                       DECIMAL(8, 2)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE BANKKREDITKARTE             *
      ******************************************************************
       01  BANKKREDITKARTE-GESAMT.
        05 NI-BANKKREDITKARTE-TAB.
           10 NI-BANKKREDITKARTE-ELE PIC S9(04) COMP OCCURS 19 TIMES.
        05 NI-BANKKREDITKARTE REDEFINES NI-BANKKREDITKARTE-TAB.
           10 NI-KKAVERTRAGSID      PIC S9(4) COMP.
           10 NI-KKAPRODUKTLFDNR    PIC S9(4) COMP.
           10 NI-KKALFDNR           PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.
           10 NI-KKAPRODUKTID       PIC S9(4) COMP.
           10 NI-KKAKARTENART       PIC S9(4) COMP.
           10 NI-KKAKARTENNR        PIC S9(4) COMP.
           10 NI-KKAHAUPTKARTENNR   PIC S9(4) COMP.
           10 NI-KKAVNRHAUPTKARTE   PIC S9(4) COMP.
           10 NI-KKAABRECHKTONR     PIC S9(4) COMP.
           10 NI-KKABLZABRECHKTO    PIC S9(4) COMP.
           10 NI-KKAFILNRABRECHKTO  PIC S9(4) COMP.
           10 NI-KKAMAHNSTATUS      PIC S9(4) COMP.
           10 NI-KKAOEFFNUNGSDAT    PIC S9(4) COMP.
           10 NI-KKAEIGENVERMKZ     PIC S9(4) COMP.
           10 NI-KKAABSCHLUSSVBNR   PIC S9(4) COMP.
           10 NI-KKAPROVISIONKZ     PIC S9(4) COMP.
           10 NI-KKAPIN             PIC S9(4) COMP.
           10 NI-KKALIMIT           PIC S9(4) COMP.
        05 DCL-BANKKREDITKARTE.
           10 KKAVERTRAGSID        PIC X(13).
           10 KKAPRODUKTLFDNR      PIC S9(4) USAGE COMP.
           10 KKALFDNR             PIC S9(4) USAGE COMP.
           10 LETZTEAENDERUNG      PIC X(26).
           10 KKAPRODUKTID         PIC S9(4) USAGE COMP.
           10 KKAKARTENART         PIC X(3).
           10 KKAKARTENNR          PIC X(16).
           10 KKAHAUPTKARTENNR     PIC X(16).
           10 KKAVNRHAUPTKARTE     PIC X(15).
           10 KKAABRECHKTONR       PIC X(10).
           10 KKABLZABRECHKTO      PIC X(8).
           10 KKAFILNRABRECHKTO    PIC S9(4) USAGE COMP.
           10 KKAMAHNSTATUS        PIC X(1).
           10 KKAOEFFNUNGSDAT      PIC X(10).
           10 KKAEIGENVERMKZ       PIC X(1).
           10 KKAABSCHLUSSVBNR     PIC S9(9) USAGE COMP.
           10 KKAPROVISIONKZ       PIC X(1).
           10 KKAPIN               PIC S9(9) USAGE COMP.
           10 KKALIMIT             PIC S9(6)V9(2) USAGE COMP-3.
      ************************** ENDE-DCLGEN *************************


