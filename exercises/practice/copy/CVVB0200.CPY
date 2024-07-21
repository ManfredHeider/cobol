      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  24.02.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0200.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVB0200.CPv  $
      *
      *   Rev 2.1   Mar 10 2010 15:03:10   t13ex43
      *ASL00344: PGM - Module für Tabelle CBA
      *
      *   Rev 2.0   Feb 24 2010 13:58:52   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    24.02.2010                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * ASL00344!24.02.10!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVB0200) CBA                                    *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0200)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************

CCC005*    EXEC SQL DECLARE VIVB0200 TABLE
CCC004*    ( MA_ST_NR                    INTEGER NOT NULL
CCC004*    , CBA_ANSPR_VOM               DATE NOT NULL
CCC004*    , CBA_PART_VBNR               INTEGER NOT NULL
CCC004*    , KNR_ANSPR_GRUND             SMALLINT NOT NULL
CCC004*    , KTY_ANSPR_GRUND             SMALLINT NOT NULL
CCC004*    , CBA_LAST_UPDATE             TIMESTAMP NOT NULL
CCC004*    , KTY_WAEHRUNG                SMALLINT NOT NULL
CCC004*    , KNR_WAEHRUNG                SMALLINT NOT NULL
CCC004*    , CBA_PART_BETRAG             DECIMAL(11, 2) NOT NULL
CCC004*    , CBA_PART_EH                 DECIMAL(10, 1) NOT NULL
CCC004*    , CBA_PART_SEIT               DATE NOT NULL
CCC004*    , KTY_ANSPR_STATUS            SMALLINT NOT NULL
CCC004*    , KNR_ANSPR_STATUS            SMALLINT NOT NULL
CCC003*    ) END-EXEC.

      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVB0200                           *
      *****************************************************************

       01  VIVB0200-GESAMT.
        05 NI-VIVB0200-TAB.
           10 NI-VIVB0200-ELE      PIC S9(04)  BINARY OCCURS  13.
        05 NI-VIVB0200             REDEFINES   NI-VIVB0200-TAB.
           10 NI-MA-ST-NR                 PIC S9(04)       BINARY.
           10 NI-CBA-ANSPR-VOM            PIC S9(04)       BINARY.
           10 NI-CBA-PART-VBNR            PIC S9(04)       BINARY.
           10 NI-KNR-ANSPR-GRUND          PIC S9(04)       BINARY.
           10 NI-KTY-ANSPR-GRUND          PIC S9(04)       BINARY.
           10 NI-CBA-LAST-UPDATE          PIC S9(04)       BINARY.
           10 NI-KTY-WAEHRUNG             PIC S9(04)       BINARY.
           10 NI-KNR-WAEHRUNG             PIC S9(04)       BINARY.
           10 NI-CBA-PART-BETRAG          PIC S9(04)       BINARY.
           10 NI-CBA-PART-EH              PIC S9(04)       BINARY.
           10 NI-CBA-PART-SEIT            PIC S9(04)       BINARY.
           10 NI-KTY-ANSPR-STATUS         PIC S9(04)       BINARY.
           10 NI-KNR-ANSPR-STATUS         PIC S9(04)       BINARY.

        05 DCL-VIVB0200.
           10 MA-ST-NR                    PIC S9(09)       BINARY.
           10 CBA-ANSPR-VOM               PIC  X(10).
           10 CBA-PART-VBNR               PIC S9(09)       BINARY.
           10 KNR-ANSPR-GRUND             PIC S9(04)       BINARY.
           10 KTY-ANSPR-GRUND             PIC S9(04)       BINARY.
           10 CBA-LAST-UPDATE             PIC  X(26).
           10 KTY-WAEHRUNG                PIC S9(04)       BINARY.
           10 KNR-WAEHRUNG                PIC S9(04)       BINARY.
           10 CBA-PART-BETRAG             PIC S9(09)V9(02) COMP-3.
           10 CBA-PART-EH                 PIC S9(09)V9(01) COMP-3.
           10 CBA-PART-SEIT               PIC  X(10).
           10 KTY-ANSPR-STATUS            PIC S9(04)       BINARY.
           10 KNR-ANSPR-STATUS            PIC S9(04)       BINARY.

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  13                    *
      *****************************************************************
