      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  10/06/11
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0447.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CTKI0447.CPv  $
      *
      *   Rev 2.2   Jul 12 2011 08:38:12   t13ex18
      *AKI01040: Erw. Deuba- inkl. DWS-Daten / Ausgabe in KI-Vertrag
      *
      *   Rev 2.1   Jul 11 2011 16:18:14   t13ex18
      *AKI01040: Erw. Deuba- inkl. DWS-Daten / Ausgabe in KI-Vertrag
      *
      *   Rev 2.0   Jun 10 2011 16:53:50   T13EX18
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE DWSZULAGE TABLE
CCC004*    ( ZULVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      ZULGUTHABPRODLFD               SMALLINT NOT NULL,
CCC004*      ZULFONDID                      SMALLINT NOT NULL,
CCC004*      ZULFOZLFDNR                    SMALLINT NOT NULL,
CCC004*      ZULLFDNR                       SMALLINT NOT NULL,
CCC004*      ZULPORTFOLIONR                 CHAR(10),
CCC004*      ZULBEITRJAHR                   SMALLINT,
CCC004*      ZULSUMBEITRJAHR                DECIMAL(15, 2),
CCC004*      LETZTEAENDERUNG                TIMESTAMP
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DWSZULAGE                   *
      ******************************************************************
       01  DWSZULAGE-GESAMT.
        05 NI-DWSZULAGE-TAB.
           10 NI-DWSZULAGE-ELE PIC S9(04) COMP OCCURS 9 TIMES.
        05 NI-DWSZULAGE REDEFINES NI-DWSZULAGE-TAB.
           10 NI-ZULVERTRAGSID      PIC S9(4) COMP.
           10 NI-ZULGUTHABPRODLFD   PIC S9(4) COMP.
           10 NI-ZULFONDID          PIC S9(4) COMP.
           10 NI-ZULFOZLFDNR        PIC S9(4) COMP.
           10 NI-ZULLFDNR           PIC S9(4) COMP.
           10 NI-ZULPORTFOLIONR     PIC S9(4) COMP.
           10 NI-ZULBEITRJAHR       PIC S9(4) COMP.
           10 NI-ZULSUMBEITRJAHR    PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.
        05 DCL-DWSZULAGE.
           10 ZULVERTRAGSID        PIC X(13).
           10 ZULGUTHABPRODLFD     PIC S9(4) USAGE COMP.
           10 ZULFONDID            PIC S9(4) USAGE COMP.
           10 ZULFOZLFDNR          PIC S9(4) USAGE COMP.
           10 ZULLFDNR             PIC S9(4) USAGE COMP.
           10 ZULPORTFOLIONR       PIC X(10).
           10 ZULBEITRJAHR         PIC S9(4) USAGE COMP.
           10 ZULSUMBEITRJAHR      PIC S9(13)V9(2) USAGE COMP-3.
           10 LETZTEAENDERUNG      PIC X(26).
      ************************** ENDE-DCLGEN *************************
