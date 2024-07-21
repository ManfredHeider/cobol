      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  06.11.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0329.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVB0329.CPv  $
      *
      *   Rev 2.1   Nov 07 2019 11:46:42   u130213
      *AVB01580: Einführung neues ZUSTRUK-Verfahren
      *
      *   Rev 2.0   Nov 06 2019 11:57:54   u130213
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    ERSTELLT: U130213                                          *
      *    DATUM:    06.11.2019                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * AVB01580!06.11.19!2.1 ! NEU                       ! U130213   *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVB0329) SWABZ                                  *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0329)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************
 
CCC005*    EXEC SQL DECLARE VIVB0329 TABLE
CCC004*    ( SWABZ_BETR_NR               INTEGER NOT NULL
CCC004*    , SWABZ_MONAT                 DATE NOT NULL
CCC004*    , KTY_UST_TYP                 SMALLINT NOT NULL
CCC004*    , KNR_UST_TYP                 SMALLINT NOT NULL
CCC004*    , KTY_WB_TYP                  SMALLINT NOT NULL
CCC004*    , KNR_WB_TYP                  SMALLINT NOT NULL
CCC004*    , SWABZ_EH_ABZUG              DECIMAL(10, 1) NOT NULL
CCC004*    , SWABZ_LAST_UPDATE           TIMESTAMP NOT NULL
CCC003*    ) END-EXEC.
 
      *****************************************************************
      * COBOL DEKLARATION FUER VIEW VIVB0329                          *
      *****************************************************************
 
       01  VIVB0329-GESAMT.
        05 NI-VIVB0329-TAB.
           10 NI-VIVB0329-ELE      PIC S9(04)  BINARY OCCURS   8.
        05 NI-VIVB0329             REDEFINES   NI-VIVB0329-TAB.
           10 NI-SWABZ-BETR-NR            PIC S9(04)       BINARY.
           10 NI-SWABZ-MONAT              PIC S9(04)       BINARY.
           10 NI-KTY-UST-TYP              PIC S9(04)       BINARY.
           10 NI-KNR-UST-TYP              PIC S9(04)       BINARY.
           10 NI-KTY-WB-TYP               PIC S9(04)       BINARY.
           10 NI-KNR-WB-TYP               PIC S9(04)       BINARY.
           10 NI-SWABZ-EH-ABZUG           PIC S9(04)       BINARY.
           10 NI-SWABZ-LAST-UPDATE        PIC S9(04)       BINARY.
 
        05 DCL-VIVB0329.
           10 SWABZ-BETR-NR               PIC S9(09)       BINARY.
           10 SWABZ-MONAT                 PIC  X(10).
           10 KTY-UST-TYP                 PIC S9(04)       BINARY.
           10 KNR-UST-TYP                 PIC S9(04)       BINARY.
           10 KTY-WB-TYP                  PIC S9(04)       BINARY.
           10 KNR-WB-TYP                  PIC S9(04)       BINARY.
           10 SWABZ-EH-ABZUG              PIC S9(09)V9(01) COMP-3.
           10 SWABZ-LAST-UPDATE           PIC  X(26).
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :   8                    *
      *****************************************************************
