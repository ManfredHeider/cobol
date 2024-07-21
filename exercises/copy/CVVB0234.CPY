      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  29.07.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0234.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVB0234.CPv  $
      *
      *   Rev 2.1   Aug 27 2019 12:00:54   t13ae56
      *AVB01581: PGM - Paket 1 - Zentrale Ermittlungsroutine
      *
      *   Rev 2.0   Jul 29 2019 16:27:08   t13ae56
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    31.07.2019                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * VBL-819 !31.07.19!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVB0234) FAPP                                   *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0234)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************
 
CCC005*    EXEC SQL DECLARE VIVB0234 TABLE
CCC004*    ( FAPP_LFDNR                  SMALLINT NOT NULL
CCC004*    , KTY_GESELL_TYP              SMALLINT NOT NULL
CCC004*    , KNR_GESELL_TYP              SMALLINT NOT NULL
CCC004*    , FAPP_BIS_STAT               DATE NOT NULL
CCC004*    , FAPP_ART                    CHAR(01) NOT NULL
CCC004*    , FAPP_EQF                    DECIMAL(05, 2)
CCC004*    , FAPP_EH_PROZ                DECIMAL(05, 2)
CCC004*    , FAPP_VB_VERTR               SMALLINT
CCC004*    , FAPP_BS                     CHAR(01) NOT NULL
CCC004*    , FAPP_MIN_PROZ               DECIMAL(05, 2)
CCC004*    , FAPP_STATUS                 CHAR(01) NOT NULL
CCC004*    , FAPP_LAST_UPDATE            TIMESTAMP NOT NULL
CCC003*    ) END-EXEC.
 
      *****************************************************************
      * COBOL DEKLARATION FUER VIEW VIVB0234                          *
      *****************************************************************
 
       01  VIVB0234-GESAMT.
        05 NI-VIVB0234-TAB.
           10 NI-VIVB0234-ELE      PIC S9(04)  BINARY OCCURS  12.
        05 NI-VIVB0234             REDEFINES   NI-VIVB0234-TAB.
           10 NI-FAPP-LFDNR               PIC S9(04)       BINARY.
           10 NI-KTY-GESELL-TYP           PIC S9(04)       BINARY.
           10 NI-KNR-GESELL-TYP           PIC S9(04)       BINARY.
           10 NI-FAPP-BIS-STAT            PIC S9(04)       BINARY.
           10 NI-FAPP-ART                 PIC S9(04)       BINARY.
           10 NI-FAPP-EQF                 PIC S9(04)       BINARY.
           10 NI-FAPP-EH-PROZ             PIC S9(04)       BINARY.
           10 NI-FAPP-VB-VERTR            PIC S9(04)       BINARY.
           10 NI-FAPP-BS                  PIC S9(04)       BINARY.
           10 NI-FAPP-MIN-PROZ            PIC S9(04)       BINARY.
           10 NI-FAPP-STATUS              PIC S9(04)       BINARY.
           10 NI-FAPP-LAST-UPDATE         PIC S9(04)       BINARY.
 
        05 DCL-VIVB0234.
           10 FAPP-LFDNR                  PIC S9(04)       BINARY.
           10 KTY-GESELL-TYP              PIC S9(04)       BINARY.
           10 KNR-GESELL-TYP              PIC S9(04)       BINARY.
           10 FAPP-BIS-STAT               PIC  X(10).
           10 FAPP-ART                    PIC  X(01).
           10 FAPP-EQF                    PIC S9(03)V9(02) COMP-3.
           10 FAPP-EH-PROZ                PIC S9(03)V9(02) COMP-3.
           10 FAPP-VB-VERTR               PIC S9(04)       BINARY.
           10 FAPP-BS                     PIC  X(01).
           10 FAPP-MIN-PROZ               PIC S9(03)V9(02) COMP-3.
           10 FAPP-STATUS                 PIC  X(01).
           10 FAPP-LAST-UPDATE            PIC  X(26).
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  12                    *
      *****************************************************************

