      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  11.05.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0312.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVB0312.CPv  $
      *
      *   Rev 2.1   Jul 07 2011 15:49:02   t13ae30
      *AAS00964: Zustrukturierung von Partnern
      *
      *   Rev 2.0   May 11 2011 17:07:28   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    ERSTELLT: TH.WILLIG                                        *
      *    DATUM:    11.05.2011                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * AAS00964!11.05.11!2.1 ! NEU                       ! TH.WILLIG *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVB0312) DIRW                                   *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0312)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************

CCC005*    EXEC SQL DECLARE VIVB0312 TABLE
CCC004*    ( MA_ST_NR                    INTEGER NOT NULL
CCC004*    , DIR_ALT                     SMALLINT NOT NULL
CCC004*    , DIR_NEU                     SMALLINT NOT NULL
CCC004*    , DIRW_DAT_VON                DATE NOT NULL
CCC004*    , DIRW_DAT_BIS                DATE NOT NULL
CCC004*    , DIRW_LAST_UPDATE            TIMESTAMP NOT NULL
CCC003*    ) END-EXEC.

      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVB0312                           *
      *****************************************************************

       01  VIVB0312-GESAMT.
        05 NI-VIVB0312-TAB.
           10 NI-VIVB0312-ELE      PIC S9(04)  BINARY OCCURS   6.
        05 NI-VIVB0312             REDEFINES   NI-VIVB0312-TAB.
           10 NI-MA-ST-NR                 PIC S9(04)       BINARY.
           10 NI-DIR-ALT                  PIC S9(04)       BINARY.
           10 NI-DIR-NEU                  PIC S9(04)       BINARY.
           10 NI-DIRW-DAT-VON             PIC S9(04)       BINARY.
           10 NI-DIRW-DAT-BIS             PIC S9(04)       BINARY.
           10 NI-DIRW-LAST-UPDATE         PIC S9(04)       BINARY.

        05 DCL-VIVB0312.
           10 MA-ST-NR                    PIC S9(09)       BINARY.
           10 DIR-ALT                     PIC S9(04)       BINARY.
           10 DIR-NEU                     PIC S9(04)       BINARY.
           10 DIRW-DAT-VON                PIC  X(26).
           10 DIRW-DAT-BIS                PIC  X(10).
           10 DIRW-LAST-UPDATE            PIC  X(26).

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :   6                    *
      *****************************************************************
