      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  05.08.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVBA064.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVBA064.CPv  $
      *
      *   Rev 2.1   Sep 08 2011 15:54:10   t13ex43
      *AIS00070: PGM - Turnus in SLM und OLZZ
      *
      *   Rev 2.0   Aug 05 2011 16:03:54   t13ex43
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
      * DCLGEN TABLE(VIVBA064) SLM                                    *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVBA064)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************

CCC005*    EXEC SQL DECLARE VIVBA064 TABLE
CCC004*    ( ST_NR                       INTEGER NOT NULL
CCC004*    , SLM_DAT_VON                 DATE NOT NULL
CCC004*    , KNR_SLM_TYP                 SMALLINT NOT NULL
CCC004*    , SLM_TURNUS                  INTEGER NOT NULL
CCC004*    , SLM_HERKUNFT                CHAR(01) NOT NULL
CCC004*    , SLM_VERARB_TYP              CHAR(01) NOT NULL
CCC004*    , KNR_WAEHRUNG                SMALLINT NOT NULL
CCC004*    , SLM_LAST_UPDATE             TIMESTAMP NOT NULL
CCC004*    , KTY_SLM_TYP                 SMALLINT NOT NULL
CCC004*    , KTY_WAEHRUNG                SMALLINT NOT NULL
CCC004*    , SLM_AUSZAHLUNG              DECIMAL(11, 2) NOT NULL
CCC003*    ) END-EXEC.

      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVBA064                           *
      *****************************************************************

       01  VIVBA064-GESAMT.
        05 NI-VIVBA064-TAB.
           10 NI-VIVBA064-ELE      PIC S9(04)  BINARY OCCURS  11.
        05 NI-VIVBA064             REDEFINES   NI-VIVBA064-TAB.
           10 NI-ST-NR                    PIC S9(04)       BINARY.
           10 NI-SLM-DAT-VON              PIC S9(04)       BINARY.
           10 NI-KNR-SLM-TYP              PIC S9(04)       BINARY.
           10 NI-SLM-TURNUS               PIC S9(04)       BINARY.
           10 NI-SLM-HERKUNFT             PIC S9(04)       BINARY.
           10 NI-SLM-VERARB-TYP           PIC S9(04)       BINARY.
           10 NI-KNR-WAEHRUNG             PIC S9(04)       BINARY.
           10 NI-SLM-LAST-UPDATE          PIC S9(04)       BINARY.
           10 NI-KTY-SLM-TYP              PIC S9(04)       BINARY.
           10 NI-KTY-WAEHRUNG             PIC S9(04)       BINARY.
           10 NI-SLM-AUSZAHLUNG           PIC S9(04)       BINARY.

        05 DCL-VIVBA064.
           10 ST-NR                       PIC S9(09)       BINARY.
           10 SLM-DAT-VON                 PIC  X(10).
           10 KNR-SLM-TYP                 PIC S9(04)       BINARY.
           10 SLM-TURNUS                  PIC S9(09)       BINARY.
           10 SLM-HERKUNFT                PIC  X(01).
           10 SLM-VERARB-TYP              PIC  X(01).
           10 KNR-WAEHRUNG                PIC S9(04)       BINARY.
           10 SLM-LAST-UPDATE             PIC  X(26).
           10 KTY-SLM-TYP                 PIC S9(04)       BINARY.
           10 KTY-WAEHRUNG                PIC S9(04)       BINARY.
           10 SLM-AUSZAHLUNG              PIC S9(09)V9(02) COMP-3.

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  11                    *
      *****************************************************************
