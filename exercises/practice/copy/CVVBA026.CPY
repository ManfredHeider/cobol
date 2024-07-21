      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  05.08.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVBA026.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVBA026.CPv  $
      *
      *   Rev 2.1   Sep 08 2011 15:54:02   t13ex43
      *AIS00070: PGM - Turnus in SLM und OLZZ
      *
      *   Rev 2.0   Aug 05 2011 16:03:48   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    08.08.2011                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * AIS00070!08.08.11!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVBA026) OLZZ                                   *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVBA026)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************

CCC005*    EXEC SQL DECLARE VIVBA026 TABLE
CCC004*    ( MA_ST_NR                    INTEGER NOT NULL
CCC004*    , OLZZ_MONAT                  DATE NOT NULL
CCC004*    , KNR_OLZ_TYP                 SMALLINT NOT NULL
CCC004*    , OLZZ_TURNUS                 INTEGER NOT NULL
CCC004*    , OLZZ_HERKUNFT               CHAR(01) NOT NULL
CCC004*    , OLZZ_VERARB_TYP             CHAR(01) NOT NULL
CCC004*    , KNR_WAEHRUNG                SMALLINT NOT NULL
CCC004*    , OLZZ_LAST_UPDATE            TIMESTAMP NOT NULL
CCC004*    , KTY_OLZ_TYP                 SMALLINT NOT NULL
CCC004*    , KTY_WAEHRUNG                SMALLINT NOT NULL
CCC004*    , OLZZ_BETRAG                 DECIMAL(11, 2) NOT NULL
CCC003*    ) END-EXEC.

      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVBA026                           *
      *****************************************************************

       01  VIVBA026-GESAMT.
        05 NI-VIVBA026-TAB.
           10 NI-VIVBA026-ELE      PIC S9(04)  BINARY OCCURS  11.
        05 NI-VIVBA026             REDEFINES   NI-VIVBA026-TAB.
           10 NI-MA-ST-NR                 PIC S9(04)       BINARY.
           10 NI-OLZZ-MONAT               PIC S9(04)       BINARY.
           10 NI-KNR-OLZ-TYP              PIC S9(04)       BINARY.
           10 NI-OLZZ-TURNUS              PIC S9(04)       BINARY.
           10 NI-OLZZ-HERKUNFT            PIC S9(04)       BINARY.
           10 NI-OLZZ-VERARB-TYP          PIC S9(04)       BINARY.
           10 NI-KNR-WAEHRUNG             PIC S9(04)       BINARY.
           10 NI-OLZZ-LAST-UPDATE         PIC S9(04)       BINARY.
           10 NI-KTY-OLZ-TYP              PIC S9(04)       BINARY.
           10 NI-KTY-WAEHRUNG             PIC S9(04)       BINARY.
           10 NI-OLZZ-BETRAG              PIC S9(04)       BINARY.

        05 DCL-VIVBA026.
           10 MA-ST-NR                    PIC S9(09)       BINARY.
           10 OLZZ-MONAT                  PIC  X(10).
           10 KNR-OLZ-TYP                 PIC S9(04)       BINARY.
           10 OLZZ-TURNUS                 PIC S9(09)       BINARY.
           10 OLZZ-HERKUNFT               PIC  X(01).
           10 OLZZ-VERARB-TYP             PIC  X(01).
           10 KNR-WAEHRUNG                PIC S9(04)       BINARY.
           10 OLZZ-LAST-UPDATE            PIC  X(26).
           10 KTY-OLZ-TYP                 PIC S9(04)       BINARY.
           10 KTY-WAEHRUNG                PIC S9(04)       BINARY.
           10 OLZZ-BETRAG                 PIC S9(09)V9(02) COMP-3.

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  11                    *
      *****************************************************************
