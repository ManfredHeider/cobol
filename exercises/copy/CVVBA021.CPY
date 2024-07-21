      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.01.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVBA021.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVBA021.CPv  $
      *
      *   Rev 2.1   Jan 28 2013 17:51:46   t13ex71
      *AVB01244: Projekt SEPA - Realisierung VB und AN - BV-Erweite
      *rung
      *
      *   Rev 2.0   Jan 15 2013 11:18:10   t13ex71
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    ERSTELLT: PRETSCH                                          *
      *    DATUM:    15.01.2013                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * AVB01244!15.01.13!2.1 ! NEU                       ! PRETSCH   *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVBA021) BV                                     *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVBA021)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************

CCC005*    EXEC SQL DECLARE VIVBA021 TABLE
CCC004*    ( ST_NR                       INTEGER NOT NULL
CCC004*    , KTY_BV_TYP                  SMALLINT NOT NULL
CCC004*    , KNR_BV_TYP                  SMALLINT NOT NULL
CCC004*    , BV_DAT_VON                  TIMESTAMP NOT NULL
CCC004*    , BV_LAST_UPDATE              TIMESTAMP NOT NULL
CCC004*    , BV_DAT_BIS                  DATE NOT NULL
CCC004*    , KTY_SPERRE                  SMALLINT NOT NULL
CCC004*    , KNR_SPERRE                  SMALLINT NOT NULL
CCC004*    , BV_KTO_NR                   CHAR(20) NOT NULL
CCC004*    , BV_BLZ                      CHAR(11) NOT NULL
CCC004*    , BV_BANKNAME                 CHAR(40) NOT NULL
CCC004*    , BV_KTO_INHABER              CHAR(40) NOT NULL
CCC004*    , BV_IBAN                     CHAR(34)
CCC004*    , BV_BIC                      CHAR(11)
CCC003*    ) END-EXEC.

      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVBA021                           *
      *****************************************************************

       01  VIVBA021-GESAMT.
        05 NI-VIVBA021-TAB.
           10 NI-VIVBA021-ELE      PIC S9(04)  BINARY OCCURS  14.
        05 NI-VIVBA021             REDEFINES   NI-VIVBA021-TAB.
           10 NI-ST-NR                    PIC S9(04)       BINARY.
           10 NI-KTY-BV-TYP               PIC S9(04)       BINARY.
           10 NI-KNR-BV-TYP               PIC S9(04)       BINARY.
           10 NI-BV-DAT-VON               PIC S9(04)       BINARY.
           10 NI-BV-LAST-UPDATE           PIC S9(04)       BINARY.
           10 NI-BV-DAT-BIS               PIC S9(04)       BINARY.
           10 NI-KTY-SPERRE               PIC S9(04)       BINARY.
           10 NI-KNR-SPERRE               PIC S9(04)       BINARY.
           10 NI-BV-KTO-NR                PIC S9(04)       BINARY.
           10 NI-BV-BLZ                   PIC S9(04)       BINARY.
           10 NI-BV-BANKNAME              PIC S9(04)       BINARY.
           10 NI-BV-KTO-INHABER           PIC S9(04)       BINARY.
           10 NI-BV-IBAN                  PIC S9(04)       BINARY.
           10 NI-BV-BIC                   PIC S9(04)       BINARY.

        05 DCL-VIVBA021.
           10 ST-NR                       PIC S9(09)       BINARY.
           10 KTY-BV-TYP                  PIC S9(04)       BINARY.
           10 KNR-BV-TYP                  PIC S9(04)       BINARY.
           10 BV-DAT-VON                  PIC  X(26).
           10 BV-LAST-UPDATE              PIC  X(26).
           10 BV-DAT-BIS                  PIC  X(10).
           10 KTY-SPERRE                  PIC S9(04)       BINARY.
           10 KNR-SPERRE                  PIC S9(04)       BINARY.
           10 BV-KTO-NR                   PIC  X(20).
           10 BV-BLZ                      PIC  X(11).
           10 BV-BANKNAME                 PIC  X(40).
           10 BV-KTO-INHABER              PIC  X(40).
           10 BV-IBAN                     PIC  X(34).
           10 BV-BIC                      PIC  X(11).

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  14                    *
      *****************************************************************
