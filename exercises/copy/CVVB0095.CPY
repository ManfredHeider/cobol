      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.07.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0095.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVB0095.CPv  $
      *
      *   Rev 2.1   Oct 07 2004 07:51:40   t13ex43
      *ASL00183: PGM - Ermittlung MAP-Kandidaten für D-Leiter
      *
      *   Rev 2.0   Jul 02 2004 15:57:42   t13ex43
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
      * DCLGEN TABLE(VIVB0095) MAPC                                   *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0095)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS  *
      *****************************************************************
CCC005*    EXEC SQL DECLARE VIVB0095 TABLE
CCC004*    ( MAPC_BETR_NR                INTEGER NOT NULL,
CCC004*      MAPC_MONAT                  DATE NOT NULL,
CCC004*      MAPC_UNTST_NR               INTEGER NOT NULL,
CCC004*      MAPC_VB_SEIT                DATE NOT NULL,
CCC004*      MAPC_LAST_UPDATE            TIMESTAMP NOT NULL,
CCC004*      MAPC_EH_ZUGANG              DECIMAL(10, 1) NOT NULL,
CCC004*      MAPC_EH_ABGANG              DECIMAL(10, 1) NOT NULL,
CCC004*      MAPC_EH_GESAMT              DECIMAL(10, 1) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0095
      ******************************************************************
       01  VIVB0095-GESAMT.
        05  NI-VIVB0095-TAB.
           10 NI-VIVB0095-ELE      PIC S9(04) BINARY OCCURS   8 TIMES.
        05  NI-VIVB0095            REDEFINES   NI-VIVB0095-TAB.
           10 NI-MAPC-BETR-NR             PIC S9(04)       BINARY.
           10 NI-MAPC-MONAT               PIC S9(04)       BINARY.
           10 NI-MAPC-UNTST-NR            PIC S9(04)       BINARY.
           10 NI-MAPC-VB-SEIT             PIC S9(04)       BINARY.
           10 NI-MAPC-LAST-UPDATE         PIC S9(04)       BINARY.
           10 NI-MAPC-EH-ZUGANG           PIC S9(04)       BINARY.
           10 NI-MAPC-EH-ABGANG           PIC S9(04)       BINARY.
           10 NI-MAPC-EH-GESAMT           PIC S9(04)       BINARY.

        05  DCL-VIVB0095.
           10 MAPC-BETR-NR                PIC S9(09)       BINARY.
           10 MAPC-MONAT                  PIC X(10).
           10 MAPC-UNTST-NR               PIC S9(09)       BINARY.
           10 MAPC-VB-SEIT                PIC X(10).
           10 MAPC-LAST-UPDATE            PIC X(26).
           10 MAPC-EH-ZUGANG              PIC S9(09)V9(01) USAGE COMP-3.
           10 MAPC-EH-ABGANG              PIC S9(09)V9(01) USAGE COMP-3.
           10 MAPC-EH-GESAMT              PIC S9(09)V9(01) USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8       *
      ******************************************************************
