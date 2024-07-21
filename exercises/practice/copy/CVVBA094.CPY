      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  11.12.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVBA094.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CVVBA094.CPv  $
      *
      *   Rev 2.2   Jan 21 2016 13:46:22   t13ae30
      *ASL00552: Leistungszuschuss Allfinanz
      *
      *   Rev 2.1   Dec 16 2014 10:51:54   t13ex13
      *ASL00527: Leistungszuschuﬂ Allfinanz
      *
      *   Rev 2.0   Dec 11 2014 14:16:14   t13ex13
      *Archiverstellung
      *
      *****************************************************************
      *    ERSTELLT: TH.WILLIG                                        *
      *    DATUM:    16.12.2015                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * ASL00552!16.12.15!2.1 ! NEU                       ! TH.WILLIG *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVBA094) LZUB                                   *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVBA094)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************

CCC005*    EXEC SQL DECLARE VIVBA094 TABLE
CCC004*    ( LZUB_ST_NR                  INTEGER NOT NULL
CCC004*    , KTY_OLZ_TYP                 SMALLINT NOT NULL
CCC004*    , KNR_OLZ_TYP                 SMALLINT NOT NULL
CCC004*    , LZUB_STICHTAG               DATE NOT NULL
CCC004*    , LZUB_ANSPR_VOM              DATE NOT NULL
CCC004*    , LZUB_LAST_UPDATE            TIMESTAMP NOT NULL
CCC004*    , LZUB_MS_NR                  SMALLINT NOT NULL
CCC004*    , LZUB_VBP_NR                 SMALLINT NOT NULL
CCC004*    , LZUB_12MON_UMSATZ           DECIMAL(10, 1) NOT NULL
CCC004*    , LZUB_03MON_UMSATZ           DECIMAL(10, 1) NOT NULL
CCC004*    , LZUB_FAKTOR                 DECIMAL(05, 2) NOT NULL
CCC004*    , LZUB_ANZ_MONATE             SMALLINT NOT NULL
CCC004*    , LZUB_HH_VBA_GES             SMALLINT NOT NULL
CCC004*    , LZUB_HH_VBA_ERF             SMALLINT NOT NULL
CCC004*    , LZUB_HH_VBA_SOLL            SMALLINT NOT NULL
CCC004*    , LZUB_HH_AL_GES              SMALLINT NOT NULL
CCC004*    , LZUB_HH_AL_ERF              SMALLINT NOT NULL
CCC004*    , LZUB_HH_AL_SOLL             SMALLINT NOT NULL
CCC004*    , LZUB_BESHH_STEIG            DECIMAL(11, 2) NOT NULL
CCC003*    ) END-EXEC.

      *****************************************************************
      * COBOL DEKLARATION F‹R VIEW VIVBA094                           *
      *****************************************************************

       01  VIVBA094-GESAMT.
        05 NI-VIVBA094-TAB.
           10 NI-VIVBA094-ELE      PIC S9(04)  BINARY OCCURS  23.
        05 NI-VIVBA094             REDEFINES   NI-VIVBA094-TAB.
           10 NI-LZUB-ST-NR               PIC S9(04)       BINARY.
           10 NI-KTY-OLZ-TYP              PIC S9(04)       BINARY.
           10 NI-KNR-OLZ-TYP              PIC S9(04)       BINARY.
           10 NI-LZUB-STICHTAG            PIC S9(04)       BINARY.
           10 NI-LZUB-ANSPR-VOM           PIC S9(04)       BINARY.
           10 NI-LZUB-LAST-UPDATE         PIC S9(04)       BINARY.
           10 NI-LZUB-MS-NR               PIC S9(04)       BINARY.
           10 NI-LZUB-VBP-NR              PIC S9(04)       BINARY.
           10 NI-LZUB-12MON-UMSATZ        PIC S9(04)       BINARY.
           10 NI-LZUB-03MON-UMSATZ        PIC S9(04)       BINARY.
           10 NI-LZUB-FAKTOR              PIC S9(04)       BINARY.
           10 NI-LZUB-ANZ-MONATE          PIC S9(04)       BINARY.
           10 NI-LZUB-HH-VBA-GES          PIC S9(04)       BINARY.
           10 NI-LZUB-HH-VBA-ERF          PIC S9(04)       BINARY.
           10 NI-LZUB-HH-VBA-SOLL         PIC S9(04)       BINARY.
           10 NI-LZUB-HH-AL-GES           PIC S9(04)       BINARY.
           10 NI-LZUB-HH-AL-ERF           PIC S9(04)       BINARY.
           10 NI-LZUB-HH-AL-SOLL          PIC S9(04)       BINARY.
           10 NI-LZUB-BESHH-STEIG         PIC S9(04)       BINARY.
           10 NI-LZUB-BES-STEIG           PIC S9(04)       BINARY.
           10 NI-LZUB-WERT-GRUPPE         PIC S9(04)       BINARY.
           10 NI-LZUB-QUALI-KZ            PIC S9(04)       BINARY.
           10 NI-LZUB-HH-ERF-KZ           PIC S9(04)       BINARY.

        05 DCL-VIVBA094.
           10 LZUB-ST-NR                  PIC S9(09)       BINARY.
           10 KTY-OLZ-TYP                 PIC S9(04)       BINARY.
           10 KNR-OLZ-TYP                 PIC S9(04)       BINARY.
           10 LZUB-STICHTAG               PIC  X(10).
           10 LZUB-ANSPR-VOM              PIC  X(10).
           10 LZUB-LAST-UPDATE            PIC  X(26).
           10 LZUB-MS-NR                  PIC S9(04)       BINARY.
           10 LZUB-VBP-NR                 PIC S9(04)       BINARY.
           10 LZUB-12MON-UMSATZ           PIC S9(09)V9(01) COMP-3.
           10 LZUB-03MON-UMSATZ           PIC S9(09)V9(01) COMP-3.
           10 LZUB-FAKTOR                 PIC S9(03)V9(02) COMP-3.
           10 LZUB-ANZ-MONATE             PIC S9(04)       BINARY.
           10 LZUB-HH-VBA-GES             PIC S9(04)       BINARY.
           10 LZUB-HH-VBA-ERF             PIC S9(04)       BINARY.
           10 LZUB-HH-VBA-SOLL            PIC S9(04)       BINARY.
           10 LZUB-HH-AL-GES              PIC S9(04)       BINARY.
           10 LZUB-HH-AL-ERF              PIC S9(04)       BINARY.
           10 LZUB-HH-AL-SOLL             PIC S9(04)       BINARY.
           10 LZUB-BESHH-STEIG            PIC S9(09)V9(02) COMP-3.
           10 LZUB-BES-STEIG              PIC S9(09)V9(02) COMP-3.
           10 LZUB-WERT-GRUPPE            PIC S9(04)       BINARY.
           10 LZUB-QUALI-KZ               PIC  X(01).
           10 LZUB-HH-ERF-KZ              PIC  X(01).

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  23                    *
      *****************************************************************

