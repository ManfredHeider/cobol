      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13/03/18
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0495.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTKI0495.CPv  $
      *
      *   Rev 2.1   May 24 2018 08:07:20   t13ae31
      *AKI01575: G70 Umstellung KSDS auf DB2 OZP Umsatz
      *
      *   Rev 2.0   Mar 13 2018 09:39:15   T13AE31
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE G70KSDSBADUMSATZ TABLE
CCC004*    ( G70UVERTRAGSNR                 CHAR(15) NOT NULL,
CCC004*      G70ULFDNR                      CHAR(1) NOT NULL,
CCC004*      G70UAENDKZ                     CHAR(2) NOT NULL,
CCC004*      G70UPRODSCHL                   CHAR(2) NOT NULL,
CCC004*      G70UFREIBETR                   DECIMAL(10, 2) NOT NULL,
CCC004*      G70UBSPGUTHAB                  DECIMAL(10, 2) NOT NULL,
CCC004*      G70USTANDDARL                  DECIMAL(10, 2) NOT NULL,
CCC004*      G70UKUMWOBAUPR                 DECIMAL(10, 2) NOT NULL,
CCC004*      G70UKUMANZUL                   DECIMAL(10, 2) NOT NULL,
CCC004*      G70UMSATZTAB                   VARCHAR(105) NOT NULL,
CCC004*      G70UUMSWKZ                     CHAR(3) NOT NULL,
CCC004*      G70ULETZTAEND                  CHAR(8) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE G70KSDSBADUMSATZ            *
      ******************************************************************
       01  G70KSDSBADUMSATZ-GESAMT.
        05 NI-G70KSDSBADUMSATZ-TAB.
           10 NI-G70KSDSBADUMSATZ-ELE PIC S9(04) COMP OCCURS 12 TIMES.
        05 NI-G70KSDSBADUMSATZ REDEFINES NI-G70KSDSBADUMSATZ-TAB.
           10 NI-G70UVERTRAGSNR     PIC S9(4) COMP.
           10 NI-G70ULFDNR          PIC S9(4) COMP.
           10 NI-G70UAENDKZ         PIC S9(4) COMP.
           10 NI-G70UPRODSCHL       PIC S9(4) COMP.
           10 NI-G70UFREIBETR       PIC S9(4) COMP.
           10 NI-G70UBSPGUTHAB      PIC S9(4) COMP.
           10 NI-G70USTANDDARL      PIC S9(4) COMP.
           10 NI-G70UKUMWOBAUPR     PIC S9(4) COMP.
           10 NI-G70UKUMANZUL       PIC S9(4) COMP.
           10 NI-G70UMSATZTAB       PIC S9(4) COMP.
           10 NI-G70UUMSWKZ         PIC S9(4) COMP.
           10 NI-G70ULETZTAEND      PIC S9(4) COMP.
        05 DCL-G70KSDSBADUMSATZ.
           10 G70UVERTRAGSNR       PIC X(15).
           10 G70ULFDNR            PIC X(1).
           10 G70UAENDKZ           PIC X(2).
           10 G70UPRODSCHL         PIC X(2).
           10 G70UFREIBETR         PIC S9(8)V9(2) USAGE COMP-3.
           10 G70UBSPGUTHAB        PIC S9(8)V9(2) USAGE COMP-3.
           10 G70USTANDDARL        PIC S9(8)V9(2) USAGE COMP-3.
           10 G70UKUMWOBAUPR       PIC S9(8)V9(2) USAGE COMP-3.
           10 G70UKUMANZUL         PIC S9(8)V9(2) USAGE COMP-3.
           10 G70UMSATZTAB.
              49 G70UMSATZTAB-LEN
                 PIC S9(4) USAGE COMP.
              49 G70UMSATZTAB-TEXT
                 PIC X(105).
           10 G70UUMSWKZ           PIC X(3).
           10 G70ULETZTAEND        PIC X(8).
      ************************** ENDE-DCLGEN *************************

