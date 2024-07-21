      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  20.01.2021
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2021 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVBA121.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVBA121.CPv  $
      *
      *   Rev 2.1   Jan 20 2021 14:56:10   t13ae56
      *AVB01644: PGM - Tabelle BEFINFO erweitern
      *
      *   Rev 2.0   Jan 20 2021 10:52:30   t13ae56
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    20.01.2021                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * VLA-1004!20.01.21!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVBA121) BEFINFO                                *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVBA121)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************
 
CCC005*    EXEC SQL DECLARE VIVBA121 TABLE
CCC004*    ( MA_ST_NR                    INTEGER NOT NULL
CCC004*    , MS_NR                       SMALLINT NOT NULL
CCC004*    , BEFINFO_DAT_VON             DATE NOT NULL
CCC004*    , BEFINFO_LAST_UPD            TIMESTAMP NOT NULL
CCC004*    , KTY_BEFBED_TYP              SMALLINT NOT NULL
CCC004*    , KNR_BEFBED_TYP              SMALLINT NOT NULL
CCC004*    , BEFINFO_E_PR_ALT            CHAR(01) NOT NULL
CCC004*    , BEFINFO_S_PR_ALT            CHAR(01) NOT NULL
CCC004*    , BEFINFO_E_PR_07             CHAR(01) NOT NULL
CCC004*    , BEFINFO_S_PR_07             CHAR(01) NOT NULL
CCC004*    , BEFINFO_E_PR_11             CHAR(01) NOT NULL
CCC004*    , BEFINFO_S_PR_11             CHAR(01) NOT NULL
CCC004*    , BEFINFO_E_PR_15             CHAR(01) NOT NULL
CCC004*    , BEFINFO_S_PR_15             CHAR(01) NOT NULL
CCC004*    , BEFINFO_E_PR_21             CHAR(01) NOT NULL
CCC004*    , BEFINFO_S_PR_21             CHAR(01) NOT NULL
CCC003*    ) END-EXEC.
 
      *****************************************************************
      * COBOL DEKLARATION FUER VIEW VIVBA121                          *
      *****************************************************************
 
       01  VIVBA121-GESAMT.
        05 NI-VIVBA121-TAB.
           10 NI-VIVBA121-ELE      PIC S9(04)  BINARY OCCURS  16.
        05 NI-VIVBA121             REDEFINES   NI-VIVBA121-TAB.
           10 NI-MA-ST-NR                 PIC S9(04)       BINARY.
           10 NI-MS-NR                    PIC S9(04)       BINARY.
           10 NI-BEFINFO-DAT-VON          PIC S9(04)       BINARY.
           10 NI-BEFINFO-LAST-UPD         PIC S9(04)       BINARY.
           10 NI-KTY-BEFBED-TYP           PIC S9(04)       BINARY.
           10 NI-KNR-BEFBED-TYP           PIC S9(04)       BINARY.
           10 NI-BEFINFO-E-PR-ALT         PIC S9(04)       BINARY.
           10 NI-BEFINFO-S-PR-ALT         PIC S9(04)       BINARY.
           10 NI-BEFINFO-E-PR-07          PIC S9(04)       BINARY.
           10 NI-BEFINFO-S-PR-07          PIC S9(04)       BINARY.
           10 NI-BEFINFO-E-PR-11          PIC S9(04)       BINARY.
           10 NI-BEFINFO-S-PR-11          PIC S9(04)       BINARY.
           10 NI-BEFINFO-E-PR-15          PIC S9(04)       BINARY.
           10 NI-BEFINFO-S-PR-15          PIC S9(04)       BINARY.
           10 NI-BEFINFO-E-PR-21          PIC S9(04)       BINARY.
           10 NI-BEFINFO-S-PR-21          PIC S9(04)       BINARY.
 
        05 DCL-VIVBA121.
           10 MA-ST-NR                    PIC S9(09)       BINARY.
           10 MS-NR                       PIC S9(04)       BINARY.
           10 BEFINFO-DAT-VON             PIC  X(10).
           10 BEFINFO-LAST-UPD            PIC  X(26).
           10 KTY-BEFBED-TYP              PIC S9(04)       BINARY.
           10 KNR-BEFBED-TYP              PIC S9(04)       BINARY.
           10 BEFINFO-E-PR-ALT            PIC  X(01).
           10 BEFINFO-S-PR-ALT            PIC  X(01).
           10 BEFINFO-E-PR-07             PIC  X(01).
           10 BEFINFO-S-PR-07             PIC  X(01).
           10 BEFINFO-E-PR-11             PIC  X(01).
           10 BEFINFO-S-PR-11             PIC  X(01).
           10 BEFINFO-E-PR-15             PIC  X(01).
           10 BEFINFO-S-PR-15             PIC  X(01).
           10 BEFINFO-E-PR-21             PIC  X(01).
           10 BEFINFO-S-PR-21             PIC  X(01).
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  16                    *
      *****************************************************************

