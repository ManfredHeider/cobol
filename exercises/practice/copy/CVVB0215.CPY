      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.12.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0215.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/CVVB0215.CPv  $
      *
      *   Rev 2.6   Feb 01 2012 15:43:04   t13ae14
      *ASL00420: Partnerbonus - DLGEN, Lese OZP, Schreibmodule
      *
      *   Rev 2.5   Jan 16 2012 14:53:08   t13ae14
      *ASL00420: Partnerbonus DCLGEN, Lese OZP, Ermittlungsprogramm
      * ...
      *
      *   Rev 2.4   Jan 16 2012 14:39:48   t13ae14
      *ASL00420: Partnerbonus DCLGEN, Lese OZP, Ermittlungsprogramm
      * ...
      *
      *   Rev 2.3   Jan 16 2012 14:31:04   t13ae14
      *ASL00420: Partnerbonus DCLGEN, Lese OZP, Ermittlungsprogramm
      * ...
      *
      *   Rev 2.2   Jan 16 2012 14:16:20   t13ae14
      *ASL00420: Partnerbonus DCLGEN, Lese OZP, Ermittlungsprogramm
      * ...
      *
      *   Rev 2.1   Jan 16 2012 14:06:46   t13ae14
      *ASL00420: Partnerbonus DCLGEN, Lese OZP, Ermittlungsprogramm
      * ...
      *
      *   Rev 2.0   Dec 19 2011 11:20:34   t13ae14
      *Archiverstellung
      *
      *****************************************************************
      *    ERSTELLT: K.OBER                                           *
      *    DATUM:    19.12.2011                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * ASL00420!19.12.11!2.1 ! NEU                       ! K.OBER    *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVB0215) PABONERF                               *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0215)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************

CCC005*    EXEC SQL DECLARE VIVB0215 TABLE
CCC004*    ( ST_NR                       INTEGER NOT NULL
CCC004*    , PABONERF_UNTST              INTEGER NOT NULL
CCC004*    , STICHTAG                    DATE NOT NULL
CCC004*    , PABON_LAST_UPD              TIMESTAMP NOT NULL
CCC004*    , KTY_WAEHRUNG                SMALLINT NOT NULL
CCC004*    , KNR_WAEHRUNG                SMALLINT NOT NULL
CCC004*    , PABON_ANSPR_BETR            DECIMAL(11, 2) NOT NULL
CCC004*    , KNR_NERF_GRUND              SMALLINT NOT NULL
CCC004*    , KTY_NERF_GRUND              SMALLINT NOT NULL
CCC003*    ) END-EXEC.

      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVB0215                           *
      *****************************************************************

       01  VIVB0215-GESAMT.
        05 NI-VIVB0215-TAB.
           10 NI-VIVB0215-ELE      PIC S9(04)  BINARY OCCURS   9.
        05 NI-VIVB0215             REDEFINES   NI-VIVB0215-TAB.
           10 NI-ST-NR                    PIC S9(04)       BINARY.
           10 NI-PABONERF-UNTST           PIC S9(04)       BINARY.
           10 NI-STICHTAG                 PIC S9(04)       BINARY.
           10 NI-PABON-LAST-UPD           PIC S9(04)       BINARY.
           10 NI-KTY-WAEHRUNG             PIC S9(04)       BINARY.
           10 NI-KNR-WAEHRUNG             PIC S9(04)       BINARY.
           10 NI-PABON-ANSPR-BETR         PIC S9(04)       BINARY.
           10 NI-KNR-NERF-GRUND           PIC S9(04)       BINARY.
           10 NI-KTY-NERF-GRUND           PIC S9(04)       BINARY.

        05 DCL-VIVB0215.
           10 ST-NR                       PIC S9(09)       BINARY.
           10 PABONERF-UNTST              PIC S9(09)       BINARY.
           10 STICHTAG                    PIC  X(10).
           10 PABON-LAST-UPD              PIC  X(26).
           10 KTY-WAEHRUNG                PIC S9(04)       BINARY.
           10 KNR-WAEHRUNG                PIC S9(04)       BINARY.
           10 PABON-ANSPR-BETR            PIC S9(09)V9(02) COMP-3.
           10 KNR-NERF-GRUND              PIC S9(04)       BINARY.
           10 KTY-NERF-GRUND              PIC S9(04)       BINARY.

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :   9                    *
      *****************************************************************
