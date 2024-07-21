      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  18.06.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVBA049.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVBA049.CPv  $
      *
      *   Rev 2.1   Aug 05 2009 14:51:48   t13ex57
      *AVB01045: Pfändungskennzeichen bei Wiedereintritt / Kü-Rückn
      *ahme
      *
      *   Rev 2.0   Jun 18 2009 15:19:00   t13ex57
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    ERSTELLT: U. BOLL                                          *
      *    DATUM:    24.06.2009                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * AVB01045!24.06.09!2.1 ! NEU                       ! U. BOLL   *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVBA049) MAT                                    *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVBA049)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************

CCC005*    EXEC SQL DECLARE VIVBA049 TABLE
CCC004*    ( MA_ST_NR                    INTEGER NOT NULL
CCC004*    , MAT_DAT_VON                 TIMESTAMP NOT NULL
CCC004*    , MAT_LAST_UPDATE             TIMESTAMP NOT NULL
CCC004*    , MAT_DAT_BIS                 DATE NOT NULL
CCC004*    , MAT_KUE_DAT_VOM             DATE
CCC004*    , MAT_EING_KUE_DAT            DATE
CCC004*    , MAT_VORZFREI_DAT            DATE
CCC004*    , KTY_MAT_AUS_GRND            SMALLINT NOT NULL
CCC004*    , KNR_MAT_AUS_GRND            SMALLINT NOT NULL
CCC004*    , MAT_L_RCK_STZ               DECIMAL(05, 2) NOT NULL
CCC004*    , MAT_ED_PROV1                DECIMAL(05, 2)
CCC004*    , MAT_ED_PROV2                DECIMAL(05, 2)
CCC004*    , MAT_ED_PROV_DAT1            DATE
CCC004*    , MAT_ED_PROV_DAT2            DATE
CCC004*    , MAT_EINTR_KUEND             DATE
CCC004*    , MAT_RUECKN_KUEND            DATE
CCC003*    ) END-EXEC.

      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVBA049                           *
      *****************************************************************

       01  VIVBA049-GESAMT.
        05 NI-VIVBA049-TAB.
           10 NI-VIVBA049-ELE      PIC S9(04)  BINARY OCCURS  16.
        05 NI-VIVBA049             REDEFINES   NI-VIVBA049-TAB.
           10 NI-MA-ST-NR                 PIC S9(04)       BINARY.
           10 NI-MAT-DAT-VON              PIC S9(04)       BINARY.
           10 NI-MAT-LAST-UPDATE          PIC S9(04)       BINARY.
           10 NI-MAT-DAT-BIS              PIC S9(04)       BINARY.
           10 NI-MAT-KUE-DAT-VOM          PIC S9(04)       BINARY.
           10 NI-MAT-EING-KUE-DAT         PIC S9(04)       BINARY.
           10 NI-MAT-VORZFREI-DAT         PIC S9(04)       BINARY.
           10 NI-KTY-MAT-AUS-GRND         PIC S9(04)       BINARY.
           10 NI-KNR-MAT-AUS-GRND         PIC S9(04)       BINARY.
           10 NI-MAT-L-RCK-STZ            PIC S9(04)       BINARY.
           10 NI-MAT-ED-PROV1             PIC S9(04)       BINARY.
           10 NI-MAT-ED-PROV2             PIC S9(04)       BINARY.
           10 NI-MAT-ED-PROV-DAT1         PIC S9(04)       BINARY.
           10 NI-MAT-ED-PROV-DAT2         PIC S9(04)       BINARY.
           10 NI-MAT-EINTR-KUEND          PIC S9(04)       BINARY.
           10 NI-MAT-RUECKN-KUEND         PIC S9(04)       BINARY.

        05 DCL-VIVBA049.
           10 MA-ST-NR                    PIC S9(09)       BINARY.
           10 MAT-DAT-VON                 PIC  X(26).
           10 MAT-LAST-UPDATE             PIC  X(26).
           10 MAT-DAT-BIS                 PIC  X(10).
           10 MAT-KUE-DAT-VOM             PIC  X(10).
           10 MAT-EING-KUE-DAT            PIC  X(10).
           10 MAT-VORZFREI-DAT            PIC  X(10).
           10 KTY-MAT-AUS-GRND            PIC S9(04)       BINARY.
           10 KNR-MAT-AUS-GRND            PIC S9(04)       BINARY.
           10 MAT-L-RCK-STZ               PIC S9(03)V9(02) COMP-3.
           10 MAT-ED-PROV1                PIC S9(03)V9(02) COMP-3.
           10 MAT-ED-PROV2                PIC S9(03)V9(02) COMP-3.
           10 MAT-ED-PROV-DAT1            PIC  X(10).
           10 MAT-ED-PROV-DAT2            PIC  X(10).
           10 MAT-EINTR-KUEND             PIC  X(10).
           10 MAT-RUECKN-KUEND            PIC  X(10).

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  16                    *
      *****************************************************************
