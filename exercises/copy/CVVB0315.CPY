      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  30.03.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0315.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVB0315.CPv  $
      *
      *   Rev 2.1   Apr 13 2012 17:02:40   t13ae36
      *AAS00996: Allfinanz Wettbewerbe 2012:Bestandsübertragungen i
      *n akt. Bewertung
      *
      *   Rev 2.0   Mar 30 2012 14:44:42   t13ae36
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    ERSTELLT: -                                                *
      *    DATUM:    30.03.2012                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * -       !30.03.12!2.1 ! NEU                       ! -         *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVB0315) BESTUEB                                *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0315)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************

CCC005*    EXEC SQL DECLARE VIVB0315 TABLE
CCC004*    ( MA_ST_NR                    INTEGER NOT NULL
CCC004*    , BESTUEB_MONAT               DATE NOT NULL
CCC004*    , BESTUEB_BEZ_MON             DATE NOT NULL
CCC004*    , BESTUEB_HIN                 DECIMAL(11, 2) NOT NULL
CCC004*    , BESTUEB_WEG                 DECIMAL(11, 2) NOT NULL
CCC003*    ) END-EXEC.

      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVB0315                           *
      *****************************************************************

       01  VIVB0315-GESAMT.
        05 NI-VIVB0315-TAB.
           10 NI-VIVB0315-ELE      PIC S9(04)  BINARY OCCURS   5.
        05 NI-VIVB0315             REDEFINES   NI-VIVB0315-TAB.
           10 NI-MA-ST-NR                 PIC S9(04)       BINARY.
           10 NI-BESTUEB-MONAT            PIC S9(04)       BINARY.
           10 NI-BESTUEB-BEZ-MON          PIC S9(04)       BINARY.
           10 NI-BESTUEB-HIN              PIC S9(04)       BINARY.
           10 NI-BESTUEB-WEG              PIC S9(04)       BINARY.

        05 DCL-VIVB0315.
           10 MA-ST-NR                    PIC S9(09)       BINARY.
           10 BESTUEB-MONAT               PIC  X(10).
           10 BESTUEB-BEZ-MON             PIC  X(10).
           10 BESTUEB-HIN                 PIC S9(09)V9(02) COMP-3.
           10 BESTUEB-WEG                 PIC S9(09)V9(02) COMP-3.

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :   5                    *
      *****************************************************************

