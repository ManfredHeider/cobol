      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  21/09/16
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0479.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CTKI0479.CPv  $
      *
      *   Rev 2.2   Dec 19 2016 13:10:24   t13ac20
      *AKI01466: Standard OZP
      *
      *   Rev 2.1   Dec 07 2016 10:53:14   t13ac20
      *AKI01466: Standard OZP
      *
      *   Rev 2.0   Sep 21 2016 11:51:59   T13AC20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE G51_VERTRAG TABLE
CCC004*    ( G51VVERTRAGSNR                 CHAR(15) NOT NULL,
CCC004*      G51VSTATUS                     CHAR(1) NOT NULL,
CCC004*      G51VMAKLERDAT                  CHAR(8) NOT NULL,
CCC004*      G51VANTRAGSPIN                 INTEGER NOT NULL,
CCC004*      G51VVERSART                    CHAR(1) NOT NULL,
CCC004*      G51VMANDANT                    INTEGER NOT NULL,
CCC004*      G51VABSCHLVBNR                 INTEGER NOT NULL,
CCC004*      G51VVRTRGBEGINN                CHAR(8) NOT NULL,
CCC004*      G51VVRTRGENDE                  CHAR(8) NOT NULL,
CCC004*      G51VNACHNAME                   CHAR(30) NOT NULL,
CCC004*      G51VVORNAME                    CHAR(30) NOT NULL,
CCC004*      G51VGEBDAT                     CHAR(8) NOT NULL,
CCC004*      G51VANREDE                     SMALLINT NOT NULL,
CCC004*      G51VGESCHLECHT                 CHAR(1) NOT NULL,
CCC004*      G51VSTRASSE                    CHAR(40) NOT NULL,
CCC004*      G51VPLZ                        CHAR(9) NOT NULL,
CCC004*      G51VLANDID                     CHAR(3) NOT NULL,
CCC004*      G51VORT                        CHAR(30) NOT NULL,
CCC004*      G51VABRECHKZ                   CHAR(1) NOT NULL,
CCC004*      G51VZUSRAHMEN                  CHAR(1) NOT NULL,
CCC004*      G51VVORVENDDATUM               CHAR(8) NOT NULL,
CCC004*      G51VVORVGES                    SMALLINT NOT NULL,
CCC004*      G51VVORVKUENDKZ                CHAR(1) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE G51_VERTRAG                 *
      ******************************************************************
       01  G51_VERTRAG-GESAMT.
        05 NI-G51_VERTRAG-TAB.
           10 NI-G51_VERTRAG-ELE PIC S9(04) COMP OCCURS 23 TIMES.
        05 NI-G51_VERTRAG REDEFINES NI-G51_VERTRAG-TAB.
           10 NI-G51VVERTRAGSNR     PIC S9(4) COMP.
           10 NI-G51VSTATUS         PIC S9(4) COMP.
           10 NI-G51VMAKLERDAT      PIC S9(4) COMP.
           10 NI-G51VANTRAGSPIN     PIC S9(4) COMP.
           10 NI-G51VVERSART        PIC S9(4) COMP.
           10 NI-G51VMANDANT        PIC S9(4) COMP.
           10 NI-G51VABSCHLVBNR     PIC S9(4) COMP.
           10 NI-G51VVRTRGBEGINN    PIC S9(4) COMP.
           10 NI-G51VVRTRGENDE      PIC S9(4) COMP.
           10 NI-G51VNACHNAME       PIC S9(4) COMP.
           10 NI-G51VVORNAME        PIC S9(4) COMP.
           10 NI-G51VGEBDAT         PIC S9(4) COMP.
           10 NI-G51VANREDE         PIC S9(4) COMP.
           10 NI-G51VGESCHLECHT     PIC S9(4) COMP.
           10 NI-G51VSTRASSE        PIC S9(4) COMP.
           10 NI-G51VPLZ            PIC S9(4) COMP.
           10 NI-G51VLANDID         PIC S9(4) COMP.
           10 NI-G51VORT            PIC S9(4) COMP.
           10 NI-G51VABRECHKZ       PIC S9(4) COMP.
           10 NI-G51VZUSRAHMEN      PIC S9(4) COMP.
           10 NI-G51VVORVENDDATUM   PIC S9(4) COMP.
           10 NI-G51VVORVGES        PIC S9(4) COMP.
           10 NI-G51VVORVKUENDKZ    PIC S9(4) COMP.
        05 DCL-G51_VERTRAG.
           10 G51VVERTRAGSNR       PIC X(15).
           10 G51VSTATUS           PIC X(1).
           10 G51VMAKLERDAT        PIC X(8).
           10 G51VANTRAGSPIN       PIC S9(9) USAGE COMP.
           10 G51VVERSART          PIC X(1).
           10 G51VMANDANT          PIC S9(9) USAGE COMP.
           10 G51VABSCHLVBNR       PIC S9(9) USAGE COMP.
           10 G51VVRTRGBEGINN      PIC X(8).
           10 G51VVRTRGENDE        PIC X(8).
           10 G51VNACHNAME         PIC X(30).
           10 G51VVORNAME          PIC X(30).
           10 G51VGEBDAT           PIC X(8).
           10 G51VANREDE           PIC S9(4) USAGE COMP.
           10 G51VGESCHLECHT       PIC X(1).
           10 G51VSTRASSE          PIC X(40).
           10 G51VPLZ              PIC X(9).
           10 G51VLANDID           PIC X(3).
           10 G51VORT              PIC X(30).
           10 G51VABRECHKZ         PIC X(1).
           10 G51VZUSRAHMEN        PIC X(1).
           10 G51VVORVENDDATUM     PIC X(8).
           10 G51VVORVGES          PIC S9(4) USAGE COMP.
           10 G51VVORVKUENDKZ      PIC X(1).
      ************************** ENDE-DCLGEN *************************

