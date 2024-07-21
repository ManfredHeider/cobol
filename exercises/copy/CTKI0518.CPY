      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  05.11.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0518.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CTKI0518.CPv  $
      *
      *   Rev 2.2   Jan 22 2019 14:38:34   t13ex78
      *AKI01641: OZPs & Copies
      *
      *   Rev 2.1   Dec 19 2018 09:38:34   t13ex78
      *AKI01641: OZPs & Copies
      *
      *   Rev 2.0   Nov 05 2018 08:52:22   t13ae31
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE G11_KUNDE TABLE
CCC004*    ( G11KDSPARTE                    CHAR(3) NOT NULL,
CCC004*      G11KDVERTRAGSNR                CHAR(15) NOT NULL,
CCC004*      G11KDROLLE                     SMALLINT NOT NULL,
CCC004*      G11KDAENDKZ                    SMALLINT NOT NULL,
CCC004*      G11KDNACHNAME                  CHAR(40) NOT NULL,
CCC004*      G11KDVORNAME                   CHAR(30) NOT NULL,
CCC004*      G11KDGEBDAT                    CHAR(8) NOT NULL,
CCC004*      G11KDANREDEKZ                  CHAR(2) NOT NULL,
CCC004*      G11KDGESCHLECHT                CHAR(1) NOT NULL,
CCC004*      G11KDSTRASSE                   CHAR(45) NOT NULL,
CCC004*      G11KDPOSTFACH                  CHAR(45) NOT NULL,
CCC004*      G11KDPLZ                       CHAR(9) NOT NULL,
CCC004*      G11KDORT                       CHAR(40) NOT NULL,
CCC004*      G11KDLKZ                       CHAR(3) NOT NULL,
CCC004*      G11KDIBAN                      CHAR(34) NOT NULL,
CCC004*      G11KDBIC                       CHAR(11) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE G11_KUNDE                   *
      ******************************************************************
       01  G11-KUNDE-GESAMT.
        05 NI-G11-KUNDE-TAB.
           10 NI-G11-KUNDE-ELE PIC S9(04) COMP OCCURS 16 TIMES.
        05 NI-G11-KUNDE REDEFINES NI-G11-KUNDE-TAB.
           10 NI-G11KDSPARTE        PIC S9(4) COMP.
           10 NI-G11KDVERTRAGSNR    PIC S9(4) COMP.
           10 NI-G11KDROLLE         PIC S9(4) COMP.
           10 NI-G11KDAENDKZ        PIC S9(4) COMP.
           10 NI-G11KDNACHNAME      PIC S9(4) COMP.
           10 NI-G11KDVORNAME       PIC S9(4) COMP.
           10 NI-G11KDGEBDAT        PIC S9(4) COMP.
           10 NI-G11KDANREDEKZ      PIC S9(4) COMP.
           10 NI-G11KDGESCHLECHT    PIC S9(4) COMP.
           10 NI-G11KDSTRASSE       PIC S9(4) COMP.
           10 NI-G11KDPOSTFACH      PIC S9(4) COMP.
           10 NI-G11KDPLZ           PIC S9(4) COMP.
           10 NI-G11KDORT           PIC S9(4) COMP.
           10 NI-G11KDLKZ           PIC S9(4) COMP.
           10 NI-G11KDIBAN          PIC S9(4) COMP.
           10 NI-G11KDBIC           PIC S9(4) COMP.
        05 DCL-G11_KUNDE.
           10 G11KDSPARTE          PIC X(3).
           10 G11KDVERTRAGSNR      PIC X(15).
           10 G11KDROLLE           PIC S9(4) USAGE COMP.
           10 G11KDAENDKZ          PIC S9(4) USAGE COMP.
           10 G11KDNACHNAME        PIC X(40).
           10 G11KDVORNAME         PIC X(30).
           10 G11KDGEBDAT          PIC X(8).
           10 G11KDANREDEKZ        PIC X(2).
           10 G11KDGESCHLECHT      PIC X(1).
           10 G11KDSTRASSE         PIC X(45).
           10 G11KDPOSTFACH        PIC X(45).
           10 G11KDPLZ             PIC X(9).
           10 G11KDORT             PIC X(40).
           10 G11KDLKZ             PIC X(3).
           10 G11KDIBAN            PIC X(34).
           10 G11KDBIC             PIC X(11).
      ************************** ENDE-DCLGEN *************************

