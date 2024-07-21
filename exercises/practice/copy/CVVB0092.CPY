      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.07.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0092.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVB0092.CPv  $
      *
      *   Rev 2.1   Oct 07 2004 07:51:24   t13ex43
      *ASL00183: PGM - Ermittlung MAP-Kandidaten für D-Leiter
      *
      *   Rev 2.0   Jul 02 2004 15:57:26   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    ERSTELLT: BERND KLEEMEIER                                  *
      *    DATUM:    12.07.2004                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00183!12.07.04!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVB0092) MAPB                                   *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0092)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS  *
      *****************************************************************
CCC005*    EXEC SQL DECLARE VIVB0092 TABLE
CCC004*    ( MA_ST_NR                    INTEGER NOT NULL,
CCC004*      MAPB_MONAT                  DATE NOT NULL,
CCC004*      MAPB_STUFE_AKT              CHAR(04) NOT NULL,
CCC004*      MAPB_STUFE_NEXT             CHAR(04) NOT NULL,
CCC004*      MAPB_LAST_UPDATE            TIMESTAMP NOT NULL,
CCC004*      MAPB_EH_ZUGANG              DECIMAL(10, 1) NOT NULL,
CCC004*      MAPB_EH_ABGANG              DECIMAL(10, 1) NOT NULL,
CCC004*      MAPB_EH_GESAMT              DECIMAL(10, 1) NOT NULL,
CCC004*      MAPB_EQF_GESAMT             DECIMAL( 5, 2) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0092
      ******************************************************************
       01  VIVB0092-GESAMT.
        05  NI-VIVB0092-TAB.
           10 NI-VIVB0092-ELE      PIC S9(04) BINARY OCCURS   9 TIMES.
        05  NI-VIVB0092            REDEFINES   NI-VIVB0092-TAB.
           10 NI-MA-ST-NR                 PIC S9(04)       BINARY.
           10 NI-MAPB-MONAT               PIC S9(04)       BINARY.
           10 NI-MAPB-STUFE-AKT           PIC S9(04)       BINARY.
           10 NI-MAPB-STUFE-NEXT          PIC S9(04)       BINARY.
           10 NI-MAPB-LAST-UPDATE         PIC S9(04)       BINARY.
           10 NI-MAPB-EH-ZUGANG           PIC S9(04)       BINARY.
           10 NI-MAPB-EH-ABGANG           PIC S9(04)       BINARY.
           10 NI-MAPB-EH-GESAMT           PIC S9(04)       BINARY.
           10 NI-MAPB-EQF-GESAMT          PIC S9(04)       BINARY.

        05  DCL-VIVB0092.
           10 MA-ST-NR                    PIC S9(09)       BINARY.
           10 MAPB-MONAT                  PIC X(10).
           10 MAPB-STUFE-AKT              PIC X(04).
           10 MAPB-STUFE-NEXT             PIC X(04).
           10 MAPB-LAST-UPDATE            PIC X(26).
           10 MAPB-EH-ZUGANG              PIC S9(09)V9(01) USAGE COMP-3.
           10 MAPB-EH-ABGANG              PIC S9(09)V9(01) USAGE COMP-3.
           10 MAPB-EH-GESAMT              PIC S9(09)V9(01) USAGE COMP-3.
           10 MAPB-EQF-GESAMT             PIC S9(03)V9(02) USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 9       *
      ******************************************************************
