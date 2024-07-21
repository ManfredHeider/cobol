      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  20/05/20
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0541.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CTKI0541.CPv  $
      *
      *   Rev 2.2   Jul 07 2020 12:33:34   t13ex78
      *AKI01780: Standard OZP G90EVERTRAG
      *
      *   Rev 2.1   Jun 12 2020 11:45:00   t13ex78
      *AKI01780: Standard OZP G90EVERTRAG
      *
      *   Rev 2.0   May 20 2020 08:50:57   T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE G90EVERTRAG TABLE
CCC004*    ( G90EVERTRAGSNR                 CHAR(15) NOT NULL,
CCC004*      G90EMANDANT                    INTEGER NOT NULL,
CCC004*      G90EZUSTAND                    CHAR(1) NOT NULL,
CCC004*      G90EABSCHLVBNR                 INTEGER NOT NULL,
CCC004*      G90EPRODUKTART                 DECIMAL(1, 0) NOT NULL,
CCC004*      G90EGEBNICHTANG                DECIMAL(3, 2) NOT NULL,
CCC004*      G90EFACTGEBPROZ                DECIMAL(3, 2) NOT NULL,
CCC004*      G90EFACTANKRAHMEN              DECIMAL(12, 2) NOT NULL,
CCC004*      G90EVERTRAGBEGINN              CHAR(8) NOT NULL,
CCC004*      G90EVERTRAGENDE                CHAR(8) NOT NULL,
CCC004*      G90EEINRICHTGEB                DECIMAL(10, 2) NOT NULL,
CCC004*      G90EIBAN                       CHAR(34) NOT NULL,
CCC004*      G90EINFOSPERREKZN              CHAR(1) NOT NULL,
CCC004*      G90EVONDATJAHR                 CHAR(8) NOT NULL,
CCC004*      G90EBISDATJAHR                 CHAR(8) NOT NULL,
CCC004*      G90ERECHNUNGJAHR               DECIMAL(8, 0) NOT NULL,
CCC004*      G90EUMSATZJAHR                 DECIMAL(10, 2) NOT NULL,
CCC004*      G90EVONDATMONAT                CHAR(8) NOT NULL,
CCC004*      G90EBISDATMONAT                CHAR(8) NOT NULL,
CCC004*      G90ERECHNUNGMONAT              DECIMAL(8, 0) NOT NULL,
CCC004*      G90EUMSATZMONAT                DECIMAL(10, 2) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE G90EVERTRAG                 *
      ******************************************************************
       01  G90EVERTRAG-GESAMT.
        05 NI-G90EVERTRAG-TAB.
           10 NI-G90EVERTRAG-ELE PIC S9(04) COMP OCCURS 21 TIMES.
        05 NI-G90EVERTRAG REDEFINES NI-G90EVERTRAG-TAB.
           10 NI-G90EVERTRAGSNR     PIC S9(4) COMP.
           10 NI-G90EMANDANT        PIC S9(4) COMP.
           10 NI-G90EZUSTAND        PIC S9(4) COMP.
           10 NI-G90EABSCHLVBNR     PIC S9(4) COMP.
           10 NI-G90EPRODUKTART     PIC S9(4) COMP.
           10 NI-G90EGEBNICHTANG    PIC S9(4) COMP.
           10 NI-G90EFACTGEBPROZ    PIC S9(4) COMP.
           10 NI-G90EFACTANKRAHMEN  PIC S9(4) COMP.
           10 NI-G90EVERTRAGBEGINN  PIC S9(4) COMP.
           10 NI-G90EVERTRAGENDE    PIC S9(4) COMP.
           10 NI-G90EEINRICHTGEB    PIC S9(4) COMP.
           10 NI-G90EIBAN           PIC S9(4) COMP.
           10 NI-G90EINFOSPERREKZN  PIC S9(4) COMP.
           10 NI-G90EVONDATJAHR     PIC S9(4) COMP.
           10 NI-G90EBISDATJAHR     PIC S9(4) COMP.
           10 NI-G90ERECHNUNGJAHR   PIC S9(4) COMP.
           10 NI-G90EUMSATZJAHR     PIC S9(4) COMP.
           10 NI-G90EVONDATMONAT    PIC S9(4) COMP.
           10 NI-G90EBISDATMONAT    PIC S9(4) COMP.
           10 NI-G90ERECHNUNGMONAT  PIC S9(4) COMP.
           10 NI-G90EUMSATZMONAT    PIC S9(4) COMP.
        05 DCL-G90EVERTRAG.
           10 G90EVERTRAGSNR       PIC X(15).
           10 G90EMANDANT          PIC S9(9) USAGE COMP.
           10 G90EZUSTAND          PIC X(1).
           10 G90EABSCHLVBNR       PIC S9(9) USAGE COMP.
           10 G90EPRODUKTART       PIC S9(1)V USAGE COMP-3.
           10 G90EGEBNICHTANG      PIC S9(1)V9(2) USAGE COMP-3.
           10 G90EFACTGEBPROZ      PIC S9(1)V9(2) USAGE COMP-3.
           10 G90EFACTANKRAHMEN    PIC S9(10)V9(2) USAGE COMP-3.
           10 G90EVERTRAGBEGINN    PIC X(8).
           10 G90EVERTRAGENDE      PIC X(8).
           10 G90EEINRICHTGEB      PIC S9(8)V9(2) USAGE COMP-3.
           10 G90EIBAN             PIC X(34).
           10 G90EINFOSPERREKZN    PIC X(1).
           10 G90EVONDATJAHR       PIC X(8).
           10 G90EBISDATJAHR       PIC X(8).
           10 G90ERECHNUNGJAHR     PIC S9(8)V USAGE COMP-3.
           10 G90EUMSATZJAHR       PIC S9(8)V9(2) USAGE COMP-3.
           10 G90EVONDATMONAT      PIC X(8).
           10 G90EBISDATMONAT      PIC X(8).
           10 G90ERECHNUNGMONAT    PIC S9(8)V USAGE COMP-3.
           10 G90EUMSATZMONAT      PIC S9(8)V9(2) USAGE COMP-3.
      ************************** ENDE-DCLGEN *************************

