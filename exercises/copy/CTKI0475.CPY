      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28/01/15
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0475.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CTKI0475.CPv  $
      *
      *   Rev 2.2   Sep 02 2015 15:11:02   t13ex73
      *AKI01342: DVM G90 Erstellung - Verarbeitung Copybooks
      *
      *   Rev 2.1   Jul 09 2015 10:47:02   t13ex73
      *AKI01342: DVM G90 Erstellung - Verarbeitung Copybooks
      *
      *   Rev 2.0   Jan 28 2015 12:11:11   T13EX73
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE G90KSDSKUNDE TABLE
CCC004*    ( G90KVERTRAGSNR                 CHAR(15) NOT NULL,
CCC004*      G90KMANDANT                    INTEGER NOT NULL,
CCC004*      G90KAENDKZ                     CHAR(2) NOT NULL,
CCC004*      G90KGESID                      SMALLINT NOT NULL,
CCC004*      G90KRECHTSFORM                 SMALLINT NOT NULL,
CCC004*      G90KANREDE                     SMALLINT NOT NULL,
CCC004*      G90KVORNAME                    CHAR(30) NOT NULL,
CCC004*      G90KNACHNAME                   CHAR(30) NOT NULL,
CCC004*      G90KGRUENDDAT                  DATE NOT NULL,
CCC004*      G90KINHABER                    CHAR(100) NOT NULL,
CCC004*      G90KSTRASSE                    CHAR(40) NOT NULL,
CCC004*      G90KPLZ                        INTEGER NOT NULL,
CCC004*      G90KORT                        CHAR(40) NOT NULL,
CCC004*      G90KPFPOSTFACH                 INTEGER NOT NULL,
CCC004*      G90KPFPLZ                      INTEGER NOT NULL,
CCC004*      G90KPFORT                      CHAR(40) NOT NULL,
CCC004*      G90KTELNR                      CHAR(30) NOT NULL,
CCC004*      G90KMOBILNR                    CHAR(30) NOT NULL,
CCC004*      G90KFAXNR                      CHAR(30) NOT NULL,
CCC004*      G90KAPTITEL                    SMALLINT NOT NULL,
CCC004*      G90KAPANREDE                   SMALLINT NOT NULL,
CCC004*      G90KAPVORNAME                  CHAR(30) NOT NULL,
CCC004*      G90KAPNACHNAME                 CHAR(30) NOT NULL,
CCC004*      G90KAPGEBDAT                   DATE NOT NULL,
CCC004*      G90KAPEMAIL                    CHAR(128) NOT NULL,
CCC004*      G90KAPFUNKTION                 CHAR(50) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE G90KSDSKUNDE                *
      ******************************************************************
       01  G90KSDSKUNDE-GESAMT.
        05 NI-G90KSDSKUNDE-TAB.
           10 NI-G90KSDSKUNDE-ELE PIC S9(04) COMP OCCURS 26 TIMES.
        05 NI-G90KSDSKUNDE REDEFINES NI-G90KSDSKUNDE-TAB.
           10 NI-G90KVERTRAGSNR     PIC S9(4) COMP.
           10 NI-G90KMANDANT        PIC S9(4) COMP.
           10 NI-G90KAENDKZ         PIC S9(4) COMP.
           10 NI-G90KGESID          PIC S9(4) COMP.
           10 NI-G90KRECHTSFORM     PIC S9(4) COMP.
           10 NI-G90KANREDE         PIC S9(4) COMP.
           10 NI-G90KVORNAME        PIC S9(4) COMP.
           10 NI-G90KNACHNAME       PIC S9(4) COMP.
           10 NI-G90KGRUENDDAT      PIC S9(4) COMP.
           10 NI-G90KINHABER        PIC S9(4) COMP.
           10 NI-G90KSTRASSE        PIC S9(4) COMP.
           10 NI-G90KPLZ            PIC S9(4) COMP.
           10 NI-G90KORT            PIC S9(4) COMP.
           10 NI-G90KPFPOSTFACH     PIC S9(4) COMP.
           10 NI-G90KPFPLZ          PIC S9(4) COMP.
           10 NI-G90KPFORT          PIC S9(4) COMP.
           10 NI-G90KTELNR          PIC S9(4) COMP.
           10 NI-G90KMOBILNR        PIC S9(4) COMP.
           10 NI-G90KFAXNR          PIC S9(4) COMP.
           10 NI-G90KAPTITEL        PIC S9(4) COMP.
           10 NI-G90KAPANREDE       PIC S9(4) COMP.
           10 NI-G90KAPVORNAME      PIC S9(4) COMP.
           10 NI-G90KAPNACHNAME     PIC S9(4) COMP.
           10 NI-G90KAPGEBDAT       PIC S9(4) COMP.
           10 NI-G90KAPEMAIL        PIC S9(4) COMP.
           10 NI-G90KAPFUNKTION     PIC S9(4) COMP.
        05 DCL-G90KSDSKUNDE.
           10 G90KVERTRAGSNR       PIC X(15).
           10 G90KMANDANT          PIC S9(9) USAGE COMP.
           10 G90KAENDKZ           PIC X(2).
           10 G90KGESID            PIC S9(4) USAGE COMP.
           10 G90KRECHTSFORM       PIC S9(4) USAGE COMP.
           10 G90KANREDE           PIC S9(4) USAGE COMP.
           10 G90KVORNAME          PIC X(30).
           10 G90KNACHNAME         PIC X(30).
           10 G90KGRUENDDAT        PIC X(10).
           10 G90KINHABER          PIC X(100).
           10 G90KSTRASSE          PIC X(40).
           10 G90KPLZ              PIC S9(9) USAGE COMP.
           10 G90KORT              PIC X(40).
           10 G90KPFPOSTFACH       PIC S9(9) USAGE COMP.
           10 G90KPFPLZ            PIC S9(9) USAGE COMP.
           10 G90KPFORT            PIC X(40).
           10 G90KTELNR            PIC X(30).
           10 G90KMOBILNR          PIC X(30).
           10 G90KFAXNR            PIC X(30).
           10 G90KAPTITEL          PIC S9(4) USAGE COMP.
           10 G90KAPANREDE         PIC S9(4) USAGE COMP.
           10 G90KAPVORNAME        PIC X(30).
           10 G90KAPNACHNAME       PIC X(30).
           10 G90KAPGEBDAT         PIC X(10).
           10 G90KAPEMAIL          PIC X(128).
           10 G90KAPFUNKTION       PIC X(50).
      ************************** ENDE-DCLGEN *************************

