      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  21.11.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0124.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVB0124.CPv  $
      *
      *   Rev 2.1   Jan 24 2013 12:14:00   t13ex57
      *AVB01257: Mailadresse für VM
      *
      *   Rev 2.0   Nov 21 2012 13:25:56   t13ex57
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    ERSTELLT: -                                                *
      *    DATUM:    21.11.2012                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * AVB01257!21.11.12!2.1 ! NEU                       ! BOLL      *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVB0124) MAINFO                                 *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0124)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************

CCC005*    EXEC SQL DECLARE VIVB0124 TABLE
CCC004*    ( MA_ST_NR                    INTEGER NOT NULL
CCC004*    , KTY_MAINFO_TYP              SMALLINT NOT NULL
CCC004*    , KNR_MAINFO_TYP              SMALLINT NOT NULL
CCC004*    , MAINFO_DAT_VON              TIMESTAMP NOT NULL
CCC004*    , MAINFO_LAST_UPD             TIMESTAMP NOT NULL
CCC004*    , MAINFO_DAT_BIS              DATE NOT NULL
CCC004*    , MAINFO_INFO                 CHAR(80) NOT NULL
CCC003*    ) END-EXEC.

      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVB0124                           *
      *****************************************************************

       01  VIVB0124-GESAMT.
        05 NI-VIVB0124-TAB.
           10 NI-VIVB0124-ELE      PIC S9(04)  BINARY OCCURS   7.
        05 NI-VIVB0124             REDEFINES   NI-VIVB0124-TAB.
           10 NI-MA-ST-NR                 PIC S9(04)       BINARY.
           10 NI-KTY-MAINFO-TYP           PIC S9(04)       BINARY.
           10 NI-KNR-MAINFO-TYP           PIC S9(04)       BINARY.
           10 NI-MAINFO-DAT-VON           PIC S9(04)       BINARY.
           10 NI-MAINFO-LAST-UPD          PIC S9(04)       BINARY.
           10 NI-MAINFO-DAT-BIS           PIC S9(04)       BINARY.
           10 NI-MAINFO-INFO              PIC S9(04)       BINARY.

        05 DCL-VIVB0124.
           10 MA-ST-NR                    PIC S9(09)       BINARY.
           10 KTY-MAINFO-TYP              PIC S9(04)       BINARY.
           10 KNR-MAINFO-TYP              PIC S9(04)       BINARY.
           10 MAINFO-DAT-VON              PIC  X(26).
           10 MAINFO-LAST-UPD             PIC  X(26).
           10 MAINFO-DAT-BIS              PIC  X(10).
           10 MAINFO-INFO                 PIC  X(80).

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :   7                    *
      *****************************************************************
