      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  18/03/19
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0531.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CTKI0531.CPv  $
      *
      *   Rev 2.2   Jun 07 2019 08:23:50   t13ex78
      *AKI01669: OZP G67_KDETICKETS
      *
      *   Rev 2.1   Mar 19 2019 12:44:08   t13ex78
      *AKI01669: OZP G67_KDETICKETS
      *
      *   Rev 2.0   Mar 18 2019 11:12:09   T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE G67_KDETICKETS TABLE
CCC004*    ( G67TVERTRAGSNR                 CHAR(15) NOT NULL,
CCC004*      G67TLFDNR                      SMALLINT NOT NULL,
CCC004*      G67TNACHNAME                   CHAR(30) NOT NULL,
CCC004*      G67TVORNAME                    CHAR(30) NOT NULL,
CCC004*      G67TGEBDAT                     CHAR(10) NOT NULL,
CCC004*      G67TSTRHNR                     CHAR(40) NOT NULL,
CCC004*      G67TPLZ                        CHAR(9) NOT NULL,
CCC004*      G67TORT                        CHAR(30) NOT NULL,
CCC004*      G67TLAND                       CHAR(3) NOT NULL,
CCC004*      G67TANREDE                     CHAR(1) NOT NULL,
CCC004*      G67TTICKETID                   DECIMAL(18, 0) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE G67_KDETICKETS              *
      ******************************************************************
       01  G67-KDETICKETS-GESAMT.
        05 NI-G67-KDETICKETS-TAB.
           10 NI-G67-KDETICKETS-ELE PIC S9(04) COMP OCCURS 11 TIMES.
        05 NI-G67-KDETICKETS REDEFINES NI-G67-KDETICKETS-TAB.
           10 NI-G67TVERTRAGSNR     PIC S9(4) COMP.
           10 NI-G67TLFDNR          PIC S9(4) COMP.
           10 NI-G67TNACHNAME       PIC S9(4) COMP.
           10 NI-G67TVORNAME        PIC S9(4) COMP.
           10 NI-G67TGEBDAT         PIC S9(4) COMP.
           10 NI-G67TSTRHNR         PIC S9(4) COMP.
           10 NI-G67TPLZ            PIC S9(4) COMP.
           10 NI-G67TORT            PIC S9(4) COMP.
           10 NI-G67TLAND           PIC S9(4) COMP.
           10 NI-G67TANREDE         PIC S9(4) COMP.
           10 NI-G67TTICKETID       PIC S9(4) COMP.
        05 DCL-G67_KDETICKETS.
           10 G67TVERTRAGSNR       PIC X(15).
           10 G67TLFDNR            PIC S9(4) USAGE COMP.
           10 G67TNACHNAME         PIC X(30).
           10 G67TVORNAME          PIC X(30).
           10 G67TGEBDAT           PIC X(10).
           10 G67TSTRHNR           PIC X(40).
           10 G67TPLZ              PIC X(9).
           10 G67TORT              PIC X(30).
           10 G67TLAND             PIC X(3).
           10 G67TANREDE           PIC X(1).
           10 G67TTICKETID         PIC S9(18)V USAGE COMP-3.
      ************************** ENDE-DCLGEN *************************


