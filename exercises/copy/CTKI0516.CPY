      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12/09/18
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0516.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/CTKI0516.CPv  $
      *
      *   Rev 2.3   Nov 22 2018 14:51:54   t13ex78
      *AKI01615: OZPs für G42_BANKPRODUKT
      *
      *   Rev 2.2   Oct 27 2018 15:29:44   t13ex78
      *AKI01615: OZPs für G42_BANKPRODUKT
      *
      *   Rev 2.1   Sep 21 2018 10:15:02   t13ex78
      *AKI01615: OZPs für G42_BANKPRODUKT
      *
      *   Rev 2.0   Sep 12 2018 08:05:58   T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE G42_BANKPRODUKT TABLE
CCC004*    ( G42PVERTRAGSNR                 CHAR(15) NOT NULL,
CCC004*      G42PDATEROFFNUNG               CHAR(8) NOT NULL,
CCC004*      G42PDATSCHLIESS                CHAR(8) NOT NULL,
CCC004*      G42PEXTLIMIT                   DECIMAL(14, 2) NOT NULL,
CCC004*      G42PSALDOBETR                  DECIMAL(14, 2) NOT NULL,
CCC004*      G42PKONTOART                   CHAR(2) NOT NULL,
CCC004*      G42PSUMHABENAM                 DECIMAL(14, 2) NOT NULL,
CCC004*      G42PSUMSOLLAM                  DECIMAL(14, 2) NOT NULL,
CCC004*      G42PKUMHABENVM                 DECIMAL(14, 2) NOT NULL,
CCC004*      G42PKUMSOLLVM                  DECIMAL(14, 2) NOT NULL,
CCC004*      G42PAKTMONAT                   CHAR(5) NOT NULL,
CCC004*      G42PVORMONAT                   CHAR(5) NOT NULL,
CCC004*      G42PSOLLZINS                   DECIMAL(6, 4) NOT NULL,
CCC004*      G42PUEBERZZINS                 DECIMAL(6, 4) NOT NULL,
CCC004*      G42PHABENZINS                  DECIMAL(6, 4) NOT NULL,
CCC004*      G42PBUCHDAT                    CHAR(8) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE G42_BANKPRODUKT             *
      ******************************************************************
       01  G42-BANKPRODUKT-GESAMT.
        05 NI-G42-BANKPRODUKT-TAB.
           10 NI-G42-BANKPRODUKT-ELE PIC S9(04) COMP OCCURS 16 TIMES.
        05 NI-G42-BANKPRODUKT REDEFINES NI-G42-BANKPRODUKT-TAB.
           10 NI-G42PVERTRAGSNR     PIC S9(4) COMP.
           10 NI-G42PDATEROFFNUNG   PIC S9(4) COMP.
           10 NI-G42PDATSCHLIESS    PIC S9(4) COMP.
           10 NI-G42PEXTLIMIT       PIC S9(4) COMP.
           10 NI-G42PSALDOBETR      PIC S9(4) COMP.
           10 NI-G42PKONTOART       PIC S9(4) COMP.
           10 NI-G42PSUMHABENAM     PIC S9(4) COMP.
           10 NI-G42PSUMSOLLAM      PIC S9(4) COMP.
           10 NI-G42PKUMHABENVM     PIC S9(4) COMP.
           10 NI-G42PKUMSOLLVM      PIC S9(4) COMP.
           10 NI-G42PAKTMONAT       PIC S9(4) COMP.
           10 NI-G42PVORMONAT       PIC S9(4) COMP.
           10 NI-G42PSOLLZINS       PIC S9(4) COMP.
           10 NI-G42PUEBERZZINS     PIC S9(4) COMP.
           10 NI-G42PHABENZINS      PIC S9(4) COMP.
           10 NI-G42PBUCHDAT        PIC S9(4) COMP.
        05 DCL-G42_BANKPRODUKT.
           10 G42PVERTRAGSNR       PIC X(15).
           10 G42PDATEROFFNUNG     PIC X(8).
           10 G42PDATSCHLIESS      PIC X(8).
           10 G42PEXTLIMIT         PIC S9(12)V9(2) USAGE COMP-3.
           10 G42PSALDOBETR        PIC S9(12)V9(2) USAGE COMP-3.
           10 G42PKONTOART         PIC X(2).
           10 G42PSUMHABENAM       PIC S9(12)V9(2) USAGE COMP-3.
           10 G42PSUMSOLLAM        PIC S9(12)V9(2) USAGE COMP-3.
           10 G42PKUMHABENVM       PIC S9(12)V9(2) USAGE COMP-3.
           10 G42PKUMSOLLVM        PIC S9(12)V9(2) USAGE COMP-3.
           10 G42PAKTMONAT         PIC X(5).
           10 G42PVORMONAT         PIC X(5).
           10 G42PSOLLZINS         PIC S9(2)V9(4) USAGE COMP-3.
           10 G42PUEBERZZINS       PIC S9(2)V9(4) USAGE COMP-3.
           10 G42PHABENZINS        PIC S9(2)V9(4) USAGE COMP-3.
           10 G42PBUCHDAT          PIC X(8).
      ************************** ENDE-DCLGEN *************************
 
 

