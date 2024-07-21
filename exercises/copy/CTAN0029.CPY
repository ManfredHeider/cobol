      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  22.11.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTAN0029.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/CTAN0029.CPv  $
      *
      *   Rev 2.4   Nov 13 2019 18:38:08   t13ae34
      *AAN01396: Antragserfassung Geiger Gold in einem System 
      *
      *   Rev 2.3   Sep 12 2019 09:58:42   t13ae23
      *AAN01396: Antragserfassung Geiger Gold in einem System
      *
      *   Rev 2.2   Aug 27 2019 11:21:36   t13ae23
      *AAN01396: Antragserfassung Geiger Gold in einem System
      *
      *   Rev 2.1   Nov 23 2018 16:16:36   t13ae34
      *AAN01396: Antragserfassung Geiger Gold in einem System
      *
      *   Rev 2.0   Nov 22 2018 16:08:06   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE TBAN0029 TABLE
CCC004*    ( PGEB_GES                       SMALLINT NOT NULL,
CCC004*      VORP_VORSCHLAG_NR              CHAR(17) NOT NULL,
CCC004*      VORP_LFD_NR                    SMALLINT NOT NULL,
CCC004*      PROD_PROD_SCHL                 SMALLINT NOT NULL,
CCC004*      VORP_TARIF                     CHAR(15) NOT NULL,
CCC004*      VORP_BETR                      DECIMAL(11, 2) NOT NULL,
CCC004*      VORP_SUMME                     DECIMAL(11, 2) NOT NULL,
CCC004*      KNR_WAEHRUNG                   SMALLINT NOT NULL,
CCC004*      VORP_ZAHLWEISE                 SMALLINT NOT NULL,
CCC004*      VORP_LAUFZEIT                  SMALLINT NOT NULL,
CCC004*      VORP_EINH_AVERM                DECIMAL(8, 2) NOT NULL,
CCC004*      VORP_ZUSATZ_N_1                DECIMAL(12, 3) NOT NULL,
CCC004*      VORP_ZUSATZ_N_2                DECIMAL(12, 3) NOT NULL,
CCC004*      VORP_ZUSATZ_A_1                CHAR(50) NOT NULL,
CCC004*      VORP_ZUSATZ_A_2                CHAR(50) NOT NULL,
CCC004*      VORP_ZEIT_INSERT               TIMESTAMP NOT NULL,
CCC004*      VORP_KENNUNG_INS               CHAR(8) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBAN0029                    *
      ******************************************************************
       01  TBAN0029-GESAMT.
        05 NI-TBAN0029-TAB.
           10 NI-TBAN0029-ELE PIC S9(04) COMP OCCURS 17 TIMES.
        05 NI-TBAN0029 REDEFINES NI-TBAN0029-TAB.
           10 NI-PGEB-GES           PIC S9(4) COMP.
           10 NI-VORP-VORSCHLAG-NR  PIC S9(4) COMP.
           10 NI-VORP-LFD-NR        PIC S9(4) COMP.
           10 NI-PROD-PROD-SCHL     PIC S9(4) COMP.
           10 NI-VORP-TARIF         PIC S9(4) COMP.
           10 NI-VORP-BETR          PIC S9(4) COMP.
           10 NI-VORP-SUMME         PIC S9(4) COMP.
           10 NI-KNR-WAEHRUNG       PIC S9(4) COMP.
           10 NI-VORP-ZAHLWEISE     PIC S9(4) COMP.
           10 NI-VORP-LAUFZEIT      PIC S9(4) COMP.
           10 NI-VORP-EINH-AVERM    PIC S9(4) COMP.
           10 NI-VORP-ZUSATZ-N-1    PIC S9(4) COMP.
           10 NI-VORP-ZUSATZ-N-2    PIC S9(4) COMP.
           10 NI-VORP-ZUSATZ-A-1    PIC S9(4) COMP.
           10 NI-VORP-ZUSATZ-A-2    PIC S9(4) COMP.
           10 NI-VORP-ZEIT-INSERT   PIC S9(4) COMP.
           10 NI-VORP-KENNUNG-INS   PIC S9(4) COMP.
        05 DCL-TBAN0029.
           10 PGEB-GES             PIC S9(4) USAGE COMP.
           10 VORP-VORSCHLAG-NR    PIC X(17).
           10 VORP-LFD-NR          PIC S9(4) USAGE COMP.
           10 PROD-PROD-SCHL       PIC S9(4) USAGE COMP.
           10 VORP-TARIF           PIC X(15).
           10 VORP-BETR            PIC S9(9)V9(2) USAGE COMP-3.
           10 VORP-SUMME           PIC S9(9)V9(2) USAGE COMP-3.
           10 KNR-WAEHRUNG         PIC S9(4) USAGE COMP.
           10 VORP-ZAHLWEISE       PIC S9(4) USAGE COMP.
           10 VORP-LAUFZEIT        PIC S9(4) USAGE COMP.
           10 VORP-EINH-AVERM      PIC S9(6)V9(2) USAGE COMP-3.
           10 VORP-ZUSATZ-N-1      PIC S9(9)V9(3) USAGE COMP-3.
           10 VORP-ZUSATZ-N-2      PIC S9(9)V9(3) USAGE COMP-3.
           10 VORP-ZUSATZ-A-1      PIC X(50).
           10 VORP-ZUSATZ-A-2      PIC X(50).
           10 VORP-ZEIT-INSERT     PIC X(26).
           10 VORP-KENNUNG-INS     PIC X(8).
      ************************** ENDE-DCLGEN *************************

