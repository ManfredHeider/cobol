      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.12.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0119.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVB0119.CPv  $
      *
      *   Rev 2.1   Feb 24 2016 09:59:08   t13ae13
      *AVB01383: NDADZ in VB22 einbauen
      *
      *   Rev 2.0   Dec 04 2015 12:51:40   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    ERSTELLT: WALB                                             *
      *    DATUM:    04.12.2015                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * AVB01383!04.12.15!2.1 ! NEU                       ! WALB      *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVB0119) NDADZ                                  *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0119)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************

CCC005*    EXEC SQL DECLARE VIVB0119 TABLE
CCC004*    ( NDADZ_DIR_NR_NEU            SMALLINT NOT NULL
CCC004*    , NDADZ_DL_NEU                INTEGER NOT NULL
CCC004*    , NDADZ_DAT_VON               DATE NOT NULL
CCC004*    , NDADZ_DIR_NR_ALT            SMALLINT NOT NULL
CCC004*    , NDADZ_DL_ALT                INTEGER NOT NULL
CCC004*    , NDADZ_TEXT                  CHAR(40) NOT NULL
CCC004*    , NDADZ_LAST_UPDATE           TIMESTAMP NOT NULL
CCC003*    ) END-EXEC.

      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVB0119                           *
      *****************************************************************

       01  VIVB0119-GESAMT.
        05 NI-VIVB0119-TAB.
           10 NI-VIVB0119-ELE      PIC S9(04)  BINARY OCCURS   7.
        05 NI-VIVB0119             REDEFINES   NI-VIVB0119-TAB.
           10 NI-NDADZ-DIR-NR-NEU         PIC S9(04)       BINARY.
           10 NI-NDADZ-DL-NEU             PIC S9(04)       BINARY.
           10 NI-NDADZ-DAT-VON            PIC S9(04)       BINARY.
           10 NI-NDADZ-DIR-NR-ALT         PIC S9(04)       BINARY.
           10 NI-NDADZ-DL-ALT             PIC S9(04)       BINARY.
           10 NI-NDADZ-TEXT               PIC S9(04)       BINARY.
           10 NI-NDADZ-LAST-UPDATE        PIC S9(04)       BINARY.

        05 DCL-VIVB0119.
           10 NDADZ-DIR-NR-NEU            PIC S9(04)       BINARY.
           10 NDADZ-DL-NEU                PIC S9(09)       BINARY.
           10 NDADZ-DAT-VON               PIC  X(10).
           10 NDADZ-DIR-NR-ALT            PIC S9(04)       BINARY.
           10 NDADZ-DL-ALT                PIC S9(09)       BINARY.
           10 NDADZ-TEXT                  PIC  X(40).
           10 NDADZ-LAST-UPDATE           PIC  X(26).

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :   7                    *
      *****************************************************************

