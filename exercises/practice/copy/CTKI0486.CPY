      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13/12/17
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0486.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTKI0486.CPv  $
      *
      *   Rev 2.1   Feb 19 2018 07:38:58   t13ac20
      *AKI01534: G20 - Projekt LENA: DB2-Zugriffe
      *
      *   Rev 2.0   Dec 13 2017 15:08:22   T13AC20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE G20_KUNDE TABLE
CCC004*    ( G20KVNR                        CHAR(15) NOT NULL,
CCC004*      G20KROLLE                      CHAR(4) NOT NULL,
CCC004*      G20KVERARB_KZN                 CHAR(1) NOT NULL,
CCC004*      G20KNACHNAME                   CHAR(40) NOT NULL,
CCC004*      G20KVORNAME                    CHAR(40) NOT NULL,
CCC004*      G20KGEBDAT                     CHAR(10) NOT NULL,
CCC004*      G20KANREDE                     SMALLINT NOT NULL,
CCC004*      G20KGESCHLECHT                 CHAR(1) NOT NULL,
CCC004*      G20KSTRASSE                    CHAR(40) NOT NULL,
CCC004*      G20KPOSTFACH                   CHAR(6) NOT NULL,
CCC004*      G20KAUSL_KZN                   CHAR(3) NOT NULL,
CCC004*      G20KPLZ                        CHAR(9) NOT NULL,
CCC004*      G20KORT                        CHAR(40) NOT NULL,
CCC004*      G20KIBAN                       CHAR(34) NOT NULL,
CCC004*      G20KBIC                        CHAR(11) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE G20_KUNDE                   *
      ******************************************************************
       01  G20-KUNDE-GESAMT.
        05 NI-G20-KUNDE-TAB.
           10 NI-G20-KUNDE-ELE PIC S9(04) COMP OCCURS 15 TIMES.
        05 NI-G20-KUNDE REDEFINES NI-G20-KUNDE-TAB.
           10 NI-G20KVNR            PIC S9(4) COMP.
           10 NI-G20KROLLE          PIC S9(4) COMP.
           10 NI-G20KVERARB-KZN     PIC S9(4) COMP.
           10 NI-G20KNACHNAME       PIC S9(4) COMP.
           10 NI-G20KVORNAME        PIC S9(4) COMP.
           10 NI-G20KGEBDAT         PIC S9(4) COMP.
           10 NI-G20KANREDE         PIC S9(4) COMP.
           10 NI-G20KGESCHLECHT     PIC S9(4) COMP.
           10 NI-G20KSTRASSE        PIC S9(4) COMP.
           10 NI-G20KPOSTFACH       PIC S9(4) COMP.
           10 NI-G20KAUSL-KZN       PIC S9(4) COMP.
           10 NI-G20KPLZ            PIC S9(4) COMP.
           10 NI-G20KORT            PIC S9(4) COMP.
           10 NI-G20KIBAN           PIC S9(4) COMP.
           10 NI-G20KBIC            PIC S9(4) COMP.
        05 DCL-G20_KUNDE.
           10 G20KVNR              PIC X(15).
           10 G20KROLLE            PIC X(4).
           10 G20KVERARB-KZN       PIC X(1).
           10 G20KNACHNAME         PIC X(40).
           10 G20KVORNAME          PIC X(40).
           10 G20KGEBDAT           PIC X(10).
           10 G20KANREDE           PIC S9(4) USAGE COMP.
           10 G20KGESCHLECHT       PIC X(1).
           10 G20KSTRASSE          PIC X(40).
           10 G20KPOSTFACH         PIC X(6).
           10 G20KAUSL-KZN         PIC X(3).
           10 G20KPLZ              PIC X(9).
           10 G20KORT              PIC X(40).
           10 G20KIBAN             PIC X(34).
           10 G20KBIC              PIC X(11).
      ************************** ENDE-DCLGEN *************************

