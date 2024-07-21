      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  24.05.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0231.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CVVB0231.CPv  $
      *
      *   Rev 2.2   19 Nov 2019 08:34:16   t13ae56
      *ASL00584: SL15 - Pflege Tabelle VHV
      *
      *   Rev 2.1   Aug 27 2019 07:55:52   t13ae56
      *ASL00584: SL15 - Pflege Tabelle VHV
      *
      *   Rev 2.0   May 24 2019 16:00:28   t13ae56
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    19.06.2019                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * ASL00584!19.06.19!2.1 ! NEU                       ! KLEEMEIER *
111119* ASL00584!11.11.19!2.2 ! VHV-ANZAHL HINZU          ! KLEEMEIER *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVB0231) VHV                                    *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0231)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************
 
CCC005*    EXEC SQL DECLARE VIVB0231 TABLE
CCC004*    ( VHV_ST_NR                   INTEGER NOT NULL
CCC004*    , KNR_VERSICH_TYP             SMALLINT NOT NULL
CCC004*    , KTY_VERSICH_TYP             SMALLINT NOT NULL
CCC004*    , VHV_BEGINN_DAT              DATE NOT NULL
CCC004*    , VHV_LAST_UPD                TIMESTAMP NOT NULL
CCC004*    , VHV_ENDE_DAT                DATE NOT NULL
CCC004*    , KNR_WAEHRUNG                SMALLINT NOT NULL
CCC004*    , KTY_WAEHRUNG                SMALLINT NOT NULL
CCC004*    , VHV_VER_BETRAG              DECIMAL(11, 2) NOT NULL
CCC004*    , VHV_VER_NR                  DECIMAL(15, 0) NOT NULL
CCC004*    , VHV_ERST_BETRAG             DECIMAL(11, 2) NOT NULL
CCC004*    , VHV_ERST_DAT                DATE NOT NULL
CCC004*    , VHV_INFO                    CHAR(100)
CCC004*    , VHV_EINZ_VT                 CHAR(50)
CCC004*    , VHV_ANZAHL                  SMALLINT
CCC003*    ) END-EXEC.
 
      *****************************************************************
      * COBOL DEKLARATION FUER VIEW VIVB0231                          *
      *****************************************************************
 
       01  VIVB0231-GESAMT.
        05 NI-VIVB0231-TAB.
111119**** 10 NI-VIVB0231-ELE      PIC S9(04)  BINARY OCCURS  14.
111119     10 NI-VIVB0231-ELE      PIC S9(04)  BINARY OCCURS  15.
        05 NI-VIVB0231             REDEFINES   NI-VIVB0231-TAB.
           10 NI-VHV-ST-NR                PIC S9(04)       BINARY.
           10 NI-KNR-VERSICH-TYP          PIC S9(04)       BINARY.
           10 NI-KTY-VERSICH-TYP          PIC S9(04)       BINARY.
           10 NI-VHV-BEGINN-DAT           PIC S9(04)       BINARY.
           10 NI-VHV-LAST-UPD             PIC S9(04)       BINARY.
           10 NI-VHV-ENDE-DAT             PIC S9(04)       BINARY.
           10 NI-KNR-WAEHRUNG             PIC S9(04)       BINARY.
           10 NI-KTY-WAEHRUNG             PIC S9(04)       BINARY.
           10 NI-VHV-VER-BETRAG           PIC S9(04)       BINARY.
           10 NI-VHV-VER-NR               PIC S9(04)       BINARY.
           10 NI-VHV-ERST-BETRAG          PIC S9(04)       BINARY.
           10 NI-VHV-ERST-DAT             PIC S9(04)       BINARY.
           10 NI-VHV-INFO                 PIC S9(04)       BINARY.
           10 NI-VHV-EINZ-VT              PIC S9(04)       BINARY.
111119     10 NI-VHV-ANZAHL               PIC S9(04)       BINARY.
 
        05 DCL-VIVB0231.
           10 VHV-ST-NR                   PIC S9(09)       BINARY.
           10 KNR-VERSICH-TYP             PIC S9(04)       BINARY.
           10 KTY-VERSICH-TYP             PIC S9(04)       BINARY.
           10 VHV-BEGINN-DAT              PIC  X(10).
           10 VHV-LAST-UPD                PIC  X(26).
           10 VHV-ENDE-DAT                PIC  X(10).
           10 KNR-WAEHRUNG                PIC S9(04)       BINARY.
           10 KTY-WAEHRUNG                PIC S9(04)       BINARY.
           10 VHV-VER-BETRAG              PIC S9(09)V9(02) COMP-3.
           10 VHV-VER-NR                  PIC S9(15)       COMP-3.
           10 VHV-ERST-BETRAG             PIC S9(09)V9(02) COMP-3.
           10 VHV-ERST-DAT                PIC  X(10).
           10 VHV-INFO                    PIC  X(100).
           10 VHV-EINZ-VT                 PIC  X(50).
111119     10 VHV-ANZAHL                  PIC S9(04)       BINARY.
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  15                    *
      *****************************************************************

