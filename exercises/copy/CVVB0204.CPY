      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.07.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0204.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVB0204.CPv  $
      *
      *   Rev 2.1   Jul 15 2010 14:43:24   t13ae14
      *ASL00352: BOZ DVAG Schreibmodule
      *
      *   Rev 2.0   Jul 15 2010 14:38:40   t13ae14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    ERSTELLT: OBER                                             *
      *    DATUM:    15.07.2010                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * ASL00352!15.07.10!2.1 ! NEU                       ! OBER      *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVB0204) BOZNERF                                *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0204)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************

CCC005*    EXEC SQL DECLARE VIVB0204 TABLE
CCC004*    ( ST_NR                       INTEGER NOT NULL
CCC004*    , STICHTAG                    DATE NOT NULL
CCC004*    , BOZNERF_LAST_UPD            TIMESTAMP NOT NULL
CCC004*    , KNR_NERF_GRUND              SMALLINT NOT NULL
CCC004*    , KTY_NERF_GRUND              SMALLINT NOT NULL
CCC003*    ) END-EXEC.

      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVB0204                           *
      *****************************************************************

       01  VIVB0204-GESAMT.
        05 NI-VIVB0204-TAB.
           10 NI-VIVB0204-ELE      PIC S9(04)  BINARY OCCURS   5.
        05 NI-VIVB0204             REDEFINES   NI-VIVB0204-TAB.
           10 NI-ST-NR                    PIC S9(04)       BINARY.
           10 NI-STICHTAG                 PIC S9(04)       BINARY.
           10 NI-BOZNERF-LAST-UPD         PIC S9(04)       BINARY.
           10 NI-KNR-NERF-GRUND           PIC S9(04)       BINARY.
           10 NI-KTY-NERF-GRUND           PIC S9(04)       BINARY.

        05 DCL-VIVB0204.
           10 ST-NR                       PIC S9(09)       BINARY.
           10 STICHTAG                    PIC  X(10).
           10 BOZNERF-LAST-UPD            PIC  X(26).
           10 KNR-NERF-GRUND              PIC S9(04)       BINARY.
           10 KTY-NERF-GRUND              PIC S9(04)       BINARY.

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :   5                    *
      *****************************************************************
