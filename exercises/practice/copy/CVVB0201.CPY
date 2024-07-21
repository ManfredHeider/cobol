      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01.06.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0201.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVB0201.CPv  $
      *
      *   Rev 2.1   Jun 11 2010 08:50:40   t13ex43
      *ASL00353: PGM - Nichterfüller Columbus-Bonus
      *
      *   Rev 2.0   Jun 01 2010 10:25:32   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    01.06.2010                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * ASL00353!01.06.10!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVB0201) CBAN                                   *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0201)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************

CCC005*    EXEC SQL DECLARE VIVB0201 TABLE
CCC004*    ( MA_ST_NR                    INTEGER NOT NULL
CCC004*    , CBAN_ANSPR_VOM              DATE NOT NULL
CCC004*    , CBAN_PART_VBNR              INTEGER NOT NULL
CCC004*    , KNR_NERF_GRUND              SMALLINT NOT NULL
CCC004*    , KTY_NERF_GRUND              SMALLINT NOT NULL
CCC004*    , CBAN_LAST_UPDATE            TIMESTAMP NOT NULL
CCC004*    , KTY_WAEHRUNG                SMALLINT NOT NULL
CCC004*    , KNR_WAEHRUNG                SMALLINT NOT NULL
CCC004*    , CBAN_PART_BETRAG            DECIMAL(11, 2) NOT NULL
CCC004*    , CBAN_PART_EH                DECIMAL(10, 1) NOT NULL
CCC004*    , CBAN_PART_SEIT              DATE NOT NULL
CCC004*    , KTY_NERF_STATUS             SMALLINT NOT NULL
CCC004*    , KNR_NERF_STATUS             SMALLINT NOT NULL
CCC003*    ) END-EXEC.

      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVB0201                           *
      *****************************************************************

       01  VIVB0201-GESAMT.
        05 NI-VIVB0201-TAB.
           10 NI-VIVB0201-ELE      PIC S9(04)  BINARY OCCURS  13.
        05 NI-VIVB0201             REDEFINES   NI-VIVB0201-TAB.
           10 NI-MA-ST-NR                 PIC S9(04)       BINARY.
           10 NI-CBAN-ANSPR-VOM           PIC S9(04)       BINARY.
           10 NI-CBAN-PART-VBNR           PIC S9(04)       BINARY.
           10 NI-KNR-NERF-GRUND           PIC S9(04)       BINARY.
           10 NI-KTY-NERF-GRUND           PIC S9(04)       BINARY.
           10 NI-CBAN-LAST-UPDATE         PIC S9(04)       BINARY.
           10 NI-KTY-WAEHRUNG             PIC S9(04)       BINARY.
           10 NI-KNR-WAEHRUNG             PIC S9(04)       BINARY.
           10 NI-CBAN-PART-BETRAG         PIC S9(04)       BINARY.
           10 NI-CBAN-PART-EH             PIC S9(04)       BINARY.
           10 NI-CBAN-PART-SEIT           PIC S9(04)       BINARY.
           10 NI-KTY-NERF-STATUS          PIC S9(04)       BINARY.
           10 NI-KNR-NERF-STATUS          PIC S9(04)       BINARY.

        05 DCL-VIVB0201.
           10 MA-ST-NR                    PIC S9(09)       BINARY.
           10 CBAN-ANSPR-VOM              PIC  X(10).
           10 CBAN-PART-VBNR              PIC S9(09)       BINARY.
           10 KNR-NERF-GRUND              PIC S9(04)       BINARY.
           10 KTY-NERF-GRUND              PIC S9(04)       BINARY.
           10 CBAN-LAST-UPDATE            PIC  X(26).
           10 KTY-WAEHRUNG                PIC S9(04)       BINARY.
           10 KNR-WAEHRUNG                PIC S9(04)       BINARY.
           10 CBAN-PART-BETRAG            PIC S9(09)V9(02) COMP-3.
           10 CBAN-PART-EH                PIC S9(09)V9(01) COMP-3.
           10 CBAN-PART-SEIT              PIC  X(10).
           10 KTY-NERF-STATUS             PIC S9(04)       BINARY.
           10 KNR-NERF-STATUS             PIC S9(04)       BINARY.

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  13                    *
      *****************************************************************
