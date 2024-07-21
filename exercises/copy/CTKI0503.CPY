      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  25/03/15
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0503.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CTKI0503.CPv  $
      *
      *   Rev 2.2   Jun 30 2020 09:54:00   t13ex78
      *AKI01780: Standard-OZP ZTKI0503
      *
      *   Rev 2.2   Sep 02 2015 15:13:08   t13ex73
      *AKI01342: DVM G90 Erstellung - Verarbeitung Copybooks
      *
      *   Rev 2.1   Jul 09 2015 10:49:00   t13ex73
      *AKI01342: DVM G90 Erstellung - Verarbeitung Copybooks
      *
      *   Rev 2.0   Mar 25 2015 15:20:40   T13EX73
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE DVMVTGEIGEN TABLE
CCC004*    ( DVZVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      DVZPRODUKTID                   SMALLINT NOT NULL,
CCC004*      DVZPRODUKTART                  SMALLINT NOT NULL,
CCC004*      DVZABRGEBSATZ                  DECIMAL(3, 2),
CCC004*      DVZFACTGEBSATZ                 DECIMAL(3, 2),
CCC004*      DVZEINRGEBBETR                 DECIMAL(10, 2),
CCC004*      DVZPROGVONDAT                  DATE,
CCC004*      DVZPROGBISDAT                  DATE,
CCC004*      DVZPROGANZRECHN                INTEGER,
CCC004*      DVZPROGGESUMS                  DECIMAL(10, 2),
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL,
CCC004*      DVZERSTELLTTST                 TIMESTAMP NOT NULL,
CCC004*      DVZFACTANKBETR                 DECIMAL(12, 2),
CCC004*      DVZPROGVONDATMM                DATE,
CCC004*      DVZPROGBISDATMM                DATE,
CCC004*      DVZPROGANZRECHNMM              INTEGER,
CCC004*      DVZPROGGESUMSMM                DECIMAL(10, 2)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DVMVTGEIGEN                 *
      ******************************************************************
       01  DVMVTGEIGEN-GESAMT.
        05 NI-DVMVTGEIGEN-TAB.
           10 NI-DVMVTGEIGEN-ELE PIC S9(04) COMP OCCURS 17 TIMES.
        05 NI-DVMVTGEIGEN REDEFINES NI-DVMVTGEIGEN-TAB.
           10 NI-DVZVERTRAGSID      PIC S9(4) COMP.
           10 NI-DVZPRODUKTID       PIC S9(4) COMP.
           10 NI-DVZPRODUKTART      PIC S9(4) COMP.
           10 NI-DVZABRGEBSATZ      PIC S9(4) COMP.
           10 NI-DVZFACTGEBSATZ     PIC S9(4) COMP.
           10 NI-DVZEINRGEBBETR     PIC S9(4) COMP.
           10 NI-DVZPROGVONDAT      PIC S9(4) COMP.
           10 NI-DVZPROGBISDAT      PIC S9(4) COMP.
           10 NI-DVZPROGANZRECHN    PIC S9(4) COMP.
           10 NI-DVZPROGGESUMS      PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.
           10 NI-DVZERSTELLTTST     PIC S9(4) COMP.
           10 NI-DVZFACTANKBETR     PIC S9(4) COMP.
           10 NI-DVZPROGVONDATMM    PIC S9(4) COMP.
           10 NI-DVZPROGBISDATMM    PIC S9(4) COMP.
           10 NI-DVZPROGANZRECHNMM  PIC S9(4) COMP.
           10 NI-DVZPROGGESUMSMM    PIC S9(4) COMP.
        05 DCL-DVMVTGEIGEN.
           10 DVZVERTRAGSID        PIC X(13).
           10 DVZPRODUKTID         PIC S9(4) USAGE COMP.
           10 DVZPRODUKTART        PIC S9(4) USAGE COMP.
           10 DVZABRGEBSATZ        PIC S9(1)V9(2) USAGE COMP-3.
           10 DVZFACTGEBSATZ       PIC S9(1)V9(2) USAGE COMP-3.
           10 DVZEINRGEBBETR       PIC S9(8)V9(2) USAGE COMP-3.
           10 DVZPROGVONDAT        PIC X(10).
           10 DVZPROGBISDAT        PIC X(10).
           10 DVZPROGANZRECHN      PIC S9(9) USAGE COMP.
           10 DVZPROGGESUMS        PIC S9(8)V9(2) USAGE COMP-3.
           10 LETZTEAENDERUNG      PIC X(26).
           10 DVZERSTELLTTST       PIC X(26).
           10 DVZFACTANKBETR       PIC S9(10)V9(2) USAGE COMP-3.
           10 DVZPROGVONDATMM      PIC X(10).
           10 DVZPROGBISDATMM      PIC X(10).
           10 DVZPROGANZRECHNMM    PIC S9(9) USAGE COMP.
           10 DVZPROGGESUMSMM      PIC S9(8)V9(2) USAGE COMP-3.
      ************************** ENDE-DCLGEN *************************

