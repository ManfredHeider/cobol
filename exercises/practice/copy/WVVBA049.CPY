      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  18.06.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBA049.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVBA049.CPv  $
      *
      *   Rev 2.1   Aug 05 2009 14:51:54   t13ex57
      *AVB01045: Pfändungskennzeichen bei Wiedereintritt / Kü-Rückn
      *ahme
      *
      *   Rev 2.0   Jun 18 2009 15:19:06   t13ex57
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVBA049                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVBA049 (MAT     )                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: U. BOLL                                          *
      *    DATUM:    24.06.2009                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AVB01045!24.06.09!2.1 ! NEU                       ! U. BOLL   *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVBA049                           *
      *****************************************************************

        05 WB-NI-VIVBA049-TAB.
           10 WB-NI-VIVBA049-ELE   PIC S9(04)  BINARY OCCURS  16.
        05 WB-NI-VIVBA049          REDEFINES   WB-NI-VIVBA049-TAB.
           10 WB-NI-MA-ST-NR              PIC S9(04)       BINARY.
           10 WB-NI-MAT-DAT-VON           PIC S9(04)       BINARY.
           10 WB-NI-MAT-LAST-UPDATE       PIC S9(04)       BINARY.
           10 WB-NI-MAT-DAT-BIS           PIC S9(04)       BINARY.
           10 WB-NI-MAT-KUE-DAT-VOM       PIC S9(04)       BINARY.
           10 WB-NI-MAT-EING-KUE-DAT      PIC S9(04)       BINARY.
           10 WB-NI-MAT-VORZFREI-DAT      PIC S9(04)       BINARY.
           10 WB-NI-KTY-MAT-AUS-GRND      PIC S9(04)       BINARY.
           10 WB-NI-KNR-MAT-AUS-GRND      PIC S9(04)       BINARY.
           10 WB-NI-MAT-L-RCK-STZ         PIC S9(04)       BINARY.
           10 WB-NI-MAT-ED-PROV1          PIC S9(04)       BINARY.
           10 WB-NI-MAT-ED-PROV2          PIC S9(04)       BINARY.
           10 WB-NI-MAT-ED-PROV-DAT1      PIC S9(04)       BINARY.
           10 WB-NI-MAT-ED-PROV-DAT2      PIC S9(04)       BINARY.
           10 WB-NI-MAT-EINTR-KUEND       PIC S9(04)       BINARY.
           10 WB-NI-MAT-RUECKN-KUEND      PIC S9(04)       BINARY.

        05 WB-VIVBA049.
           10 WB-MA-ST-NR                 PIC S9(09)       BINARY.
           10 WB-MAT-DAT-VON              PIC  X(26).
           10 WB-MAT-LAST-UPDATE          PIC  X(26).
           10 WB-MAT-DAT-BIS              PIC  X(10).
           10 WB-MAT-KUE-DAT-VOM          PIC  X(10).
           10 WB-MAT-EING-KUE-DAT         PIC  X(10).
           10 WB-MAT-VORZFREI-DAT         PIC  X(10).
           10 WB-KTY-MAT-AUS-GRND         PIC S9(04)       BINARY.
           10 WB-KNR-MAT-AUS-GRND         PIC S9(04)       BINARY.
           10 WB-MAT-L-RCK-STZ            PIC S9(03)V9(02) COMP-3.
           10 WB-MAT-ED-PROV1             PIC S9(03)V9(02) COMP-3.
           10 WB-MAT-ED-PROV2             PIC S9(03)V9(02) COMP-3.
           10 WB-MAT-ED-PROV-DAT1         PIC  X(10).
           10 WB-MAT-ED-PROV-DAT2         PIC  X(10).
           10 WB-MAT-EINTR-KUEND          PIC  X(10).
           10 WB-MAT-RUECKN-KUEND         PIC  X(10).

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  16                    *
      *****************************************************************
