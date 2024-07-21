      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  21/09/16
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0480.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/CTKI0480.CPv  $
      *
      *   Rev 2.3   Jun 27 2018 12:22:44   t13ac17
      *AKI01594: BKK-Linde Vertragslöschung - Sourcen
      *
      *   Rev 2.2   Dec 19 2016 13:17:18   t13ac20
      *AKI01466: Standard OZP
      *
      *   Rev 2.1   Dec 07 2016 10:53:24   t13ac20
      *AKI01466: Standard OZP
      *
      *   Rev 2.0   Sep 21 2016 13:35:05   T13AC20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE G51_PERSON TABLE
CCC004*    ( G51PVERTRAGSNR                 CHAR(15) NOT NULL,
CCC004*      G51PLFDNR                      SMALLINT NOT NULL,
CCC004*      G51PSTATUS                     CHAR(1) NOT NULL,
CCC004*      G51PVERSART                    CHAR(1) NOT NULL,
CCC004*      G51PVRTRGBEGINN                CHAR(8) NOT NULL,
CCC004*      G51PVRTRGENDE                  CHAR(8) NOT NULL,
CCC004*      G51PNACHNAME                   CHAR(30) NOT NULL,
CCC004*      G51PVORNAME                    CHAR(30) NOT NULL,
CCC004*      G51PGEBDAT                     CHAR(8) NOT NULL,
CCC004*      G51PANREDE                     SMALLINT NOT NULL,
CCC004*      G51PGESCHLECHT                 CHAR(1) NOT NULL,
CCC004*      G51PABRECHKZ                   CHAR(1) NOT NULL,
CCC004*      G51PZUSRAHMEN                  CHAR(1) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE G51_PERSON                  *
      ******************************************************************
       01  G51-PERSON-GESAMT.
        05 NI-G51-PERSON-TAB.
           10 NI-G51-PERSON-ELE PIC S9(04) COMP OCCURS 13 TIMES.
        05 NI-G51-PERSON REDEFINES NI-G51-PERSON-TAB.
           10 NI-G51PVERTRAGSNR     PIC S9(4) COMP.
           10 NI-G51PLFDNR          PIC S9(4) COMP.
           10 NI-G51PSTATUS         PIC S9(4) COMP.
           10 NI-G51PVERSART        PIC S9(4) COMP.
           10 NI-G51PVRTRGBEGINN    PIC S9(4) COMP.
           10 NI-G51PVRTRGENDE      PIC S9(4) COMP.
           10 NI-G51PNACHNAME       PIC S9(4) COMP.
           10 NI-G51PVORNAME        PIC S9(4) COMP.
           10 NI-G51PGEBDAT         PIC S9(4) COMP.
           10 NI-G51PANREDE         PIC S9(4) COMP.
           10 NI-G51PGESCHLECHT     PIC S9(4) COMP.
           10 NI-G51PABRECHKZ       PIC S9(4) COMP.
           10 NI-G51PZUSRAHMEN      PIC S9(4) COMP.
        05 DCL-G51_PERSON.
           10 G51PVERTRAGSNR       PIC X(15).
           10 G51PLFDNR            PIC S9(4) USAGE COMP.
           10 G51PSTATUS           PIC X(1).
           10 G51PVERSART          PIC X(1).
           10 G51PVRTRGBEGINN      PIC X(8).
           10 G51PVRTRGENDE        PIC X(8).
           10 G51PNACHNAME         PIC X(30).
           10 G51PVORNAME          PIC X(30).
           10 G51PGEBDAT           PIC X(8).
           10 G51PANREDE           PIC S9(4) USAGE COMP.
           10 G51PGESCHLECHT       PIC X(1).
           10 G51PABRECHKZ         PIC X(1).
           10 G51PZUSRAHMEN        PIC X(1).
      ************************** ENDE-DCLGEN *************************

