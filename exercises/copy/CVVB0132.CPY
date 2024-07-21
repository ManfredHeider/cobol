      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  07.06.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0132.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVB0132.CPv  $
      *
      *   Rev 2.1   Jun 28 2019 12:38:18   t13ae56
      *AVB01570: PGM - Gutscheincode
      *
      *   Rev 2.0   Jun 07 2019 14:54:12   t13ae56
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    07.06.2019                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * VBL-998 !07.06.19!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVB0132) GUTSC                                  *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0132)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************
 
CCC005*    EXEC SQL DECLARE VIVB0132 TABLE
CCC004*    ( GUTSC_ST_NR                 INTEGER NOT NULL
CCC004*    , KTY_GUTSC_TYP               SMALLINT NOT NULL
CCC004*    , KNR_GUTSC_TYP               SMALLINT NOT NULL
CCC004*    , GUTSC_CODE                  CHAR(20) NOT NULL
CCC004*    , GUTSC_GUELT_AB              DATE NOT NULL
CCC004*    , GUTSC_GUELT_BIS             DATE NOT NULL
CCC004*    , GUTSC_MELD_DAT              DATE
CCC004*    , GUTSC_LAST_UPD              TIMESTAMP NOT NULL
CCC003*    ) END-EXEC.
 
      *****************************************************************
      * COBOL DEKLARATION FUER VIEW VIVB0132                          *
      *****************************************************************
 
       01  VIVB0132-GESAMT.
        05 NI-VIVB0132-TAB.
           10 NI-VIVB0132-ELE      PIC S9(04)  BINARY OCCURS   8.
        05 NI-VIVB0132             REDEFINES   NI-VIVB0132-TAB.
           10 NI-GUTSC-ST-NR              PIC S9(04)       BINARY.
           10 NI-KTY-GUTSC-TYP            PIC S9(04)       BINARY.
           10 NI-KNR-GUTSC-TYP            PIC S9(04)       BINARY.
           10 NI-GUTSC-CODE               PIC S9(04)       BINARY.
           10 NI-GUTSC-GUELT-AB           PIC S9(04)       BINARY.
           10 NI-GUTSC-GUELT-BIS          PIC S9(04)       BINARY.
           10 NI-GUTSC-MELD-DAT           PIC S9(04)       BINARY.
           10 NI-GUTSC-LAST-UPD           PIC S9(04)       BINARY.
 
        05 DCL-VIVB0132.
           10 GUTSC-ST-NR                 PIC S9(09)       BINARY.
           10 KTY-GUTSC-TYP               PIC S9(04)       BINARY.
           10 KNR-GUTSC-TYP               PIC S9(04)       BINARY.
           10 GUTSC-CODE                  PIC  X(20).
           10 GUTSC-GUELT-AB              PIC  X(10).
           10 GUTSC-GUELT-BIS             PIC  X(10).
           10 GUTSC-MELD-DAT              PIC  X(10).
           10 GUTSC-LAST-UPD              PIC  X(26).
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :   8                    *
      *****************************************************************
