      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.04.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVBB094.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVBB094.CPv  $
      *
      *   Rev 2.1   Apr 29 2016 11:35:38   t13ex30
      *AVB01400: Projekt Lucy2 - ZVVBA093 u. ZVVBB094
      *
      *   Rev 2.0   Apr 19 2016 09:24:04   t13ex30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    ERSTELLT: H. WÖLFLIK                                       *
      *    DATUM:    19.04.2016                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * AVB01400!19.04.16!2.1 ! NEU                       ! H. WÖLFLIK*
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVBB094) ALIAS                               *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVBB094)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************
 
CCC005*    EXEC SQL DECLARE VIVBB094 TABLE
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
CCC004*    , LZUB_BES_STEIG              DECIMAL(11, 2) NOT NULL
CCC004*    , LZUB_WERT_GRUPPE            SMALLINT NOT NULL
CCC004*    , LZUB_QUALI_KZ               CHAR(01) NOT NULL
CCC004*    , LZUB_HH_ERF_KZ              CHAR(01) NOT NULL
CCC004*    , LZUB_12M_UMS_UPFL           DECIMAL(10, 1) NOT NULL
CCC004*    , LZUB_03M_UMS_UPFL           DECIMAL(10, 1) NOT NULL
CCC003*    ) END-EXEC.
 
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVBB094                           *
      *****************************************************************
 
       01  VIVBB094-GESAMT.
        05 NI-VIVBB094-TAB.
           10 NI-VIVBB094-ELE      PIC S9(04)  BINARY OCCURS  25.
        05 NI-VIVBB094             REDEFINES   NI-VIVBB094-TAB.
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
           10 NI-LZUB-12M-UMS-UPFL        PIC S9(04)       BINARY.
           10 NI-LZUB-03M-UMS-UPFL        PIC S9(04)       BINARY.
 
        05 DCL-VIVBB094.
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
           10 LZUB-12M-UMS-UPFL           PIC S9(09)V9(01) COMP-3.
           10 LZUB-03M-UMS-UPFL           PIC S9(09)V9(01) COMP-3.
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  25                    *
      *****************************************************************

