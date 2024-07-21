      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01/10/12
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0452.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CTKI0452.CPv  $
      *
      *   Rev 2.2   Oct 19 2012 13:40:04   T13EX20
      *AKI01147: Central-Tarifumstellung (Bordero-Tool)
      *
      *   Rev 2.1   Oct 18 2012 10:27:58   T13EX20
      *AKI01147: Central-Tarifumstellung (Bordero-Tool)
      *
      *   Rev 2.0   Oct 1 2012 16:56:43   T13EX20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE CKVPERSHISTORIE TABLE
CCC004*    ( PHIVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      PHIPERSONENNR                  SMALLINT NOT NULL,
CCC004*      PHIDATUMHISTORIE               INTEGER NOT NULL,
CCC004*      PHITARIFHISTORIE               CHAR(12) NOT NULL,
CCC004*      PHITAGESSATZ                   DECIMAL(9, 2),
CCC004*      PHIBEITRAG                     DECIMAL(9, 2),
CCC004*      PHIGESETZZUSCHLG               DECIMAL(9, 2),
CCC004*      PHIRISIKOZUSCHLG               DECIMAL(9, 2),
CCC004*      PHISELBSTBEHALT                DECIMAL(9, 2),
CCC004*      PHIGUTSCHRIFTBEF               DECIMAL(9, 2),
CCC004*      LETZTEAENDERUNG                TIMESTAMP
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE CKVPERSHISTORIE             *
      ******************************************************************
       01  CKVPERSHISTORIE-GESAMT.
        05 NI-CKVPERSHISTORIE-TAB.
           10 NI-CKVPERSHISTORIE-ELE PIC S9(04) COMP OCCURS 11 TIMES.
        05 NI-CKVPERSHISTORIE REDEFINES NI-CKVPERSHISTORIE-TAB.
           10 NI-PHIVERTRAGSID      PIC S9(4) COMP.
           10 NI-PHIPERSONENNR      PIC S9(4) COMP.
           10 NI-PHIDATUMHISTORIE   PIC S9(4) COMP.
           10 NI-PHITARIFHISTORIE   PIC S9(4) COMP.
           10 NI-PHITAGESSATZ       PIC S9(4) COMP.
           10 NI-PHIBEITRAG         PIC S9(4) COMP.
           10 NI-PHIGESETZZUSCHLG   PIC S9(4) COMP.
           10 NI-PHIRISIKOZUSCHLG   PIC S9(4) COMP.
           10 NI-PHISELBSTBEHALT    PIC S9(4) COMP.
           10 NI-PHIGUTSCHRIFTBEF   PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.
        05 DCL-CKVPERSHISTORIE.
           10 PHIVERTRAGSID        PIC X(13).
           10 PHIPERSONENNR        PIC S9(4) USAGE COMP.
           10 PHIDATUMHISTORIE     PIC S9(9) USAGE COMP.
           10 PHITARIFHISTORIE     PIC X(12).
           10 PHITAGESSATZ         PIC S9(7)V9(2) USAGE COMP-3.
           10 PHIBEITRAG           PIC S9(7)V9(2) USAGE COMP-3.
           10 PHIGESETZZUSCHLG     PIC S9(7)V9(2) USAGE COMP-3.
           10 PHIRISIKOZUSCHLG     PIC S9(7)V9(2) USAGE COMP-3.
           10 PHISELBSTBEHALT      PIC S9(7)V9(2) USAGE COMP-3.
           10 PHIGUTSCHRIFTBEF     PIC S9(7)V9(2) USAGE COMP-3.
           10 LETZTEAENDERUNG      PIC X(26).
      ************************** ENDE-DCLGEN *************************
