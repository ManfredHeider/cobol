      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.08.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0100.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVB0100.CPv  $
      *
      *   Rev 2.1   Sep 27 2006 09:32:00   t13ex43
      *AIS00060: PGM - Geldleistungen konfigurierbar machen
      *
      *   Rev 2.0   Aug 04 2006 09:01:54   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    ERSTELLT: BERND KLEEMEIER                                  *
      *    DATUM:    08.08.2006                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AIS00060!08.08.06!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      * DCLGEN TABLE(VIVB0100) FIBUZ                                  *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0100)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS  *
      *****************************************************************
CCC005*    EXEC SQL DECLARE VIVB0100 TABLE
CCC004*    ( KTY_GESELL_TYP              SMALLINT NOT NULL,
CCC004*      KNR_GESELL_TYP              SMALLINT NOT NULL,
CCC004*      FIBUZ_KONTO                 CHAR(12) NOT NULL,
CCC004*      FIBUZ_DAT_VON               TIMESTAMP NOT NULL,
CCC004*      FIBUZ_LAST_UPDATE           TIMESTAMP NOT NULL,
CCC004*      FIBUZ_DAT_BIS               DATE NOT NULL,
CCC004*      FIBUZ_INFO                  CHAR(30) NOT NULL,
CCC004*      KTY_LEISTUNGSTYP            SMALLINT NOT NULL,
CCC004*      KNR_LEISTUNGSTYP            SMALLINT NOT NULL,
CCC004*      KTY_DATUM                   SMALLINT NOT NULL,
CCC004*      KNR_DATUM                   SMALLINT NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0100
      ******************************************************************
       01  VIVB0100-GESAMT.
        05  NI-VIVB0100-TAB.
           10 NI-VIVB0100-ELE      PIC S9(04) BINARY OCCURS  11 TIMES.
        05  NI-VIVB0100            REDEFINES   NI-VIVB0100-TAB.
           10 NI-KTY-GESELL-TYP           PIC S9(04)       BINARY.
           10 NI-KNR-GESELL-TYP           PIC S9(04)       BINARY.
           10 NI-FIBUZ-KONTO              PIC S9(04)       BINARY.
           10 NI-FIBUZ-DAT-VON            PIC S9(04)       BINARY.
           10 NI-FIBUZ-LAST-UPDATE        PIC S9(04)       BINARY.
           10 NI-FIBUZ-DAT-BIS            PIC S9(04)       BINARY.
           10 NI-FIBUZ-INFO               PIC S9(04)       BINARY.
           10 NI-KTY-LEISTUNGSTYP         PIC S9(04)       BINARY.
           10 NI-KNR-LEISTUNGSTYP         PIC S9(04)       BINARY.
           10 NI-KTY-DATUM                PIC S9(04)       BINARY.
           10 NI-KNR-DATUM                PIC S9(04)       BINARY.

        05  DCL-VIVB0100.
           10 KTY-GESELL-TYP              PIC S9(04)       BINARY.
           10 KNR-GESELL-TYP              PIC S9(04)       BINARY.
           10 FIBUZ-KONTO                 PIC  X(12).
           10 FIBUZ-DAT-VON               PIC  X(26).
           10 FIBUZ-LAST-UPDATE           PIC  X(26).
           10 FIBUZ-DAT-BIS               PIC  X(10).
           10 FIBUZ-INFO                  PIC  X(30).
           10 KTY-LEISTUNGSTYP            PIC S9(04)       BINARY.
           10 KNR-LEISTUNGSTYP            PIC S9(04)       BINARY.
           10 KTY-DATUM                   PIC S9(04)       BINARY.
           10 KNR-DATUM                   PIC S9(04)       BINARY.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 11      *
      ******************************************************************
