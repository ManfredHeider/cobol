      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19/03/18
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0496.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTKI0496.CPv  $
      *
      *   Rev 2.1   May 24 2018 08:12:04   t13ae31
      *AKI01575: G70 Umstellung KSDS auf DB2 OZP Kunde
      *
      *   Rev 2.0   Mar 19 2018 07:29:56   T13AE31
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE G70KSDSBADKUNDE TABLE
CCC004*    ( G70KVERTRAGSNR                 CHAR(15) NOT NULL,
CCC004*      G70KAENDKZ                     CHAR(2) NOT NULL,
CCC004*      G70KNACHNAME                   CHAR(40) NOT NULL,
CCC004*      G70KVORNAME                    CHAR(30) NOT NULL,
CCC004*      G70KGEBDAT                     CHAR(8) NOT NULL,
CCC004*      G70KANREDEKZ                   CHAR(2) NOT NULL,
CCC004*      G70KGESCHL                     CHAR(1) NOT NULL,
CCC004*      G70KSTRASSE                    CHAR(45) NOT NULL,
CCC004*      G70KPOSTFACH                   CHAR(45) NOT NULL,
CCC004*      G70KPLZ                        CHAR(9) NOT NULL,
CCC004*      G70KORT                        CHAR(40) NOT NULL,
CCC004*      G70KLKZ                        CHAR(3) NOT NULL,
CCC004*      G70KBUNDLAND                   CHAR(2) NOT NULL,
CCC004*      G70KFREIBETR                   DECIMAL(7, 2) NOT NULL,
CCC004*      G70KROLLEKZ                    CHAR(2) NOT NULL,
CCC004*      G70KNACHNAMEMI                 CHAR(40) NOT NULL,
CCC004*      G70KVORNAMEMI                  CHAR(30) NOT NULL,
CCC004*      G70KGEBDATMI                   CHAR(8) NOT NULL,
CCC004*      G70KANREDEKZMI                 CHAR(2) NOT NULL,
CCC004*      G70KGESCHLMI                   CHAR(1) NOT NULL,
CCC004*      G70KSTRASSEMI                  CHAR(45) NOT NULL,
CCC004*      G70KPOSTFACHMI                 CHAR(45) NOT NULL,
CCC004*      G70KPLZMI                      CHAR(9) NOT NULL,
CCC004*      G70KORTMI                      CHAR(40) NOT NULL,
CCC004*      G70KLKZMI                      CHAR(3) NOT NULL,
CCC004*      G70KIBAN                       CHAR(34) NOT NULL,
CCC004*      G70KBIC                        CHAR(11) NOT NULL,
CCC004*      G70KABWBZ                      CHAR(1) NOT NULL,
CCC004*      G70KMANDREF                    CHAR(35) NOT NULL,
CCC004*      G70KLETZTAEND                  CHAR(8) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE G70KSDSBADKUNDE             *
      ******************************************************************
       01  G70KSDSBADKUNDE-GESAMT.
        05 NI-G70KSDSBADKUNDE-TAB.
           10 NI-G70KSDSBADKUNDE-ELE PIC S9(04) COMP OCCURS 30 TIMES.
        05 NI-G70KSDSBADKUNDE REDEFINES NI-G70KSDSBADKUNDE-TAB.
           10 NI-G70KVERTRAGSNR     PIC S9(4) COMP.
           10 NI-G70KAENDKZ         PIC S9(4) COMP.
           10 NI-G70KNACHNAME       PIC S9(4) COMP.
           10 NI-G70KVORNAME        PIC S9(4) COMP.
           10 NI-G70KGEBDAT         PIC S9(4) COMP.
           10 NI-G70KANREDEKZ       PIC S9(4) COMP.
           10 NI-G70KGESCHL         PIC S9(4) COMP.
           10 NI-G70KSTRASSE        PIC S9(4) COMP.
           10 NI-G70KPOSTFACH       PIC S9(4) COMP.
           10 NI-G70KPLZ            PIC S9(4) COMP.
           10 NI-G70KORT            PIC S9(4) COMP.
           10 NI-G70KLKZ            PIC S9(4) COMP.
           10 NI-G70KBUNDLAND       PIC S9(4) COMP.
           10 NI-G70KFREIBETR       PIC S9(4) COMP.
           10 NI-G70KROLLEKZ        PIC S9(4) COMP.
           10 NI-G70KNACHNAMEMI     PIC S9(4) COMP.
           10 NI-G70KVORNAMEMI      PIC S9(4) COMP.
           10 NI-G70KGEBDATMI       PIC S9(4) COMP.
           10 NI-G70KANREDEKZMI     PIC S9(4) COMP.
           10 NI-G70KGESCHLMI       PIC S9(4) COMP.
           10 NI-G70KSTRASSEMI      PIC S9(4) COMP.
           10 NI-G70KPOSTFACHMI     PIC S9(4) COMP.
           10 NI-G70KPLZMI          PIC S9(4) COMP.
           10 NI-G70KORTMI          PIC S9(4) COMP.
           10 NI-G70KLKZMI          PIC S9(4) COMP.
           10 NI-G70KIBAN           PIC S9(4) COMP.
           10 NI-G70KBIC            PIC S9(4) COMP.
           10 NI-G70KABWBZ          PIC S9(4) COMP.
           10 NI-G70KMANDREF        PIC S9(4) COMP.
           10 NI-G70KLETZTAEND      PIC S9(4) COMP.
        05 DCL-G70KSDSBADKUNDE.
           10 G70KVERTRAGSNR       PIC X(15).
           10 G70KAENDKZ           PIC X(2).
           10 G70KNACHNAME         PIC X(40).
           10 G70KVORNAME          PIC X(30).
           10 G70KGEBDAT           PIC X(8).
           10 G70KANREDEKZ         PIC X(2).
           10 G70KGESCHL           PIC X(1).
           10 G70KSTRASSE          PIC X(45).
           10 G70KPOSTFACH         PIC X(45).
           10 G70KPLZ              PIC X(9).
           10 G70KORT              PIC X(40).
           10 G70KLKZ              PIC X(3).
           10 G70KBUNDLAND         PIC X(2).
           10 G70KFREIBETR         PIC S9(5)V9(2) USAGE COMP-3.
           10 G70KROLLEKZ          PIC X(2).
           10 G70KNACHNAMEMI       PIC X(40).
           10 G70KVORNAMEMI        PIC X(30).
           10 G70KGEBDATMI         PIC X(8).
           10 G70KANREDEKZMI       PIC X(2).
           10 G70KGESCHLMI         PIC X(1).
           10 G70KSTRASSEMI        PIC X(45).
           10 G70KPOSTFACHMI       PIC X(45).
           10 G70KPLZMI            PIC X(9).
           10 G70KORTMI            PIC X(40).
           10 G70KLKZMI            PIC X(3).
           10 G70KIBAN             PIC X(34).
           10 G70KBIC              PIC X(11).
           10 G70KABWBZ            PIC X(1).
           10 G70KMANDREF          PIC X(35).
           10 G70KLETZTAEND        PIC X(8).
      ************************** ENDE-DCLGEN *************************

