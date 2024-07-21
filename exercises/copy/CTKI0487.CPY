      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  30.01.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0487.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CTKI0487.CPv  $
      *
      *   Rev 2.2   Jun 08 2018 11:56:58   t13ae31
      *AKI01567: G10 Umstellung auf KSDS in DB2
      *
      *   Rev 2.1   Jun 07 2018 10:13:40   t13ae31
      *AKI01567: G10 Umstellung KSDS in DB2 OZP Kunde
      *
      *   Rev 2.0   Jan 30 2018 10:37:28   t13ae31
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE G10KSDSKUNDE TABLE
CCC004*    ( G10KDSPARTE                    CHAR(3) NOT NULL,
CCC004*      G10KDVERTRAGSNR                CHAR(15) NOT NULL,
CCC004*      G10KDROLLE                     SMALLINT NOT NULL,
CCC004*      G10KDAENDKZ                    SMALLINT NOT NULL,
CCC004*      G10KDNACHNAME                  CHAR(40) NOT NULL,
CCC004*      G10KDVORNAME                   CHAR(30) NOT NULL,
CCC004*      G10KDGEBDAT                    CHAR(8) NOT NULL,
CCC004*      G10KDANREDEKZ                  CHAR(2) NOT NULL,
CCC004*      G10KDGESCHLECHT                CHAR(1) NOT NULL,
CCC004*      G10KDSTRASSE                   CHAR(45) NOT NULL,
CCC004*      G10KDPOSTFACH                  CHAR(45) NOT NULL,
CCC004*      G10KDPLZ                       CHAR(9) NOT NULL,
CCC004*      G10KDORT                       CHAR(40) NOT NULL,
CCC004*      G10KDLKZ                       CHAR(3) NOT NULL,
CCC004*      G10KDIBAN                      CHAR(34) NOT NULL,
CCC004*      G10KDBIC                       CHAR(11) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE G10KSDSKUNDE                *
      ******************************************************************
       01  G10KSDSKUNDE-GESAMT.
        05 NI-G10KSDSKUNDE-TAB.
           10 NI-G10KSDSKUNDE-ELE PIC S9(04) COMP OCCURS 16 TIMES.
        05 NI-G10KSDSKUNDE REDEFINES NI-G10KSDSKUNDE-TAB.
           10 NI-G10KDSPARTE        PIC S9(4) COMP.
           10 NI-G10KDVERTRAGSNR    PIC S9(4) COMP.
           10 NI-G10KDROLLE         PIC S9(4) COMP.
           10 NI-G10KDAENDKZ        PIC S9(4) COMP.
           10 NI-G10KDNACHNAME      PIC S9(4) COMP.
           10 NI-G10KDVORNAME       PIC S9(4) COMP.
           10 NI-G10KDGEBDAT        PIC S9(4) COMP.
           10 NI-G10KDANREDEKZ      PIC S9(4) COMP.
           10 NI-G10KDGESCHLECHT    PIC S9(4) COMP.
           10 NI-G10KDSTRASSE       PIC S9(4) COMP.
           10 NI-G10KDPOSTFACH      PIC S9(4) COMP.
           10 NI-G10KDPLZ           PIC S9(4) COMP.
           10 NI-G10KDORT           PIC S9(4) COMP.
           10 NI-G10KDLKZ           PIC S9(4) COMP.
           10 NI-G10KDIBAN          PIC S9(4) COMP.
           10 NI-G10KDBIC           PIC S9(4) COMP.
        05 DCL-G10KSDSKUNDE.
           10 G10KDSPARTE          PIC X(3).
           10 G10KDVERTRAGSNR      PIC X(15).
           10 G10KDROLLE           PIC S9(4) USAGE COMP.
           10 G10KDAENDKZ          PIC S9(4) USAGE COMP.
           10 G10KDNACHNAME        PIC X(40).
           10 G10KDVORNAME         PIC X(30).
           10 G10KDGEBDAT          PIC X(8).
           10 G10KDANREDEKZ        PIC X(2).
           10 G10KDGESCHLECHT      PIC X(1).
           10 G10KDSTRASSE         PIC X(45).
           10 G10KDPOSTFACH        PIC X(45).
           10 G10KDPLZ             PIC X(9).
           10 G10KDORT             PIC X(40).
           10 G10KDLKZ             PIC X(3).
           10 G10KDIBAN            PIC X(34).
           10 G10KDBIC             PIC X(11).
      ************************** ENDE-DCLGEN *************************
 

