      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  03.03.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVBA036.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVBA036.CPv  $
      *
      *   Rev 2.1   Mar 04 2015 10:15:32   t13ae30
      *ASL00530: Ext.grd.-Zuschuss - Anz. Monate für VBI
      *
      *   Rev 2.0   Mar 03 2015 16:14:36   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    ERSTELLT: TH.WILLIG                                        *
      *    DATUM:    03.03.2015                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * ASL00530!03.03.15!2.1 ! NEU                       ! TH.WILLIG *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVBA036) ANZ                                    *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVBA036)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************

CCC005*    EXEC SQL DECLARE VIVBA036 TABLE
CCC004*    ( ANZ_MONAT                   DATE NOT NULL
CCC004*    , KNR_ANZ_TYP                 SMALLINT NOT NULL
CCC004*    , KNR_GESELL_TYP              SMALLINT NOT NULL
CCC004*    , KTY_ANZ_TYP                 SMALLINT NOT NULL
CCC004*    , KTY_GESELL_TYP              SMALLINT NOT NULL
CCC004*    , ANZ_LAST_UPDATE             TIMESTAMP NOT NULL
CCC004*    , ANZ_WERT                    DECIMAL(15, 2) NOT NULL
CCC004*    , MA_ST_NR                    INTEGER NOT NULL
CCC003*    ) END-EXEC.

      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVBA036                           *
      *****************************************************************

       01  VIVBA036-GESAMT.
        05 NI-VIVBA036-TAB.
           10 NI-VIVBA036-ELE      PIC S9(04)  BINARY OCCURS   8.
        05 NI-VIVBA036             REDEFINES   NI-VIVBA036-TAB.
           10 NI-ANZ-MONAT                PIC S9(04)       BINARY.
           10 NI-KNR-ANZ-TYP              PIC S9(04)       BINARY.
           10 NI-KNR-GESELL-TYP           PIC S9(04)       BINARY.
           10 NI-KTY-ANZ-TYP              PIC S9(04)       BINARY.
           10 NI-KTY-GESELL-TYP           PIC S9(04)       BINARY.
           10 NI-ANZ-LAST-UPDATE          PIC S9(04)       BINARY.
           10 NI-ANZ-WERT                 PIC S9(04)       BINARY.
           10 NI-MA-ST-NR                 PIC S9(04)       BINARY.

        05 DCL-VIVBA036.
           10 ANZ-MONAT                   PIC  X(10).
           10 KNR-ANZ-TYP                 PIC S9(04)       BINARY.
           10 KNR-GESELL-TYP              PIC S9(04)       BINARY.
           10 KTY-ANZ-TYP                 PIC S9(04)       BINARY.
           10 KTY-GESELL-TYP              PIC S9(04)       BINARY.
           10 ANZ-LAST-UPDATE             PIC  X(26).
           10 ANZ-WERT                    PIC S9(13)V9(02) COMP-3.
           10 MA-ST-NR                    PIC S9(09)       BINARY.

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :   8                    *
      *****************************************************************

