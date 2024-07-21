      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.05.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBB211.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVBB211.CPv  $
      *
      *   Rev 2.1   Sep 07 2016 12:05:08   t13ae30
      *AVB01400: Projekt Lucy2 - Leistungsbonus Allfinanz - TWI
      *
      *   Rev 2.0   May 02 2016 12:04:36   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVBA211                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVBA211 (LZUAN   )                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: TH.WILLIG                                        *
      *    DATUM:    29.04.2016                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AVB01400!29.04.16!2.1 ! NEU                       ! TH.WILLIG *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVBB211                           *
      *****************************************************************

        05 WB-NI-VIVBB211-TAB.
           10 WB-NI-VIVBB211-ELE   PIC S9(04)  BINARY OCCURS  18.
        05 WB-NI-VIVBB211          REDEFINES   WB-NI-VIVBB211-TAB.
           10 WB-NI-LZUAN-ST-NR           PIC S9(04)       BINARY.
           10 WB-NI-LZUAN-ANSPR-VOM       PIC S9(04)       BINARY.
           10 WB-NI-KNR-NERF-GRUND        PIC S9(04)       BINARY.
           10 WB-NI-KTY-NERF-GRUND        PIC S9(04)       BINARY.
           10 WB-NI-LZUAN-EH              PIC S9(04)       BINARY.
           10 WB-NI-LZUAN-LAST-UPDATE     PIC S9(04)       BINARY.
           10 WB-NI-LZUAN-HH-VBA-GES      PIC S9(04)       BINARY.
           10 WB-NI-LZUAN-HH-VBA-ERF      PIC S9(04)       BINARY.
           10 WB-NI-LZUAN-HH-VBA-SOLL     PIC S9(04)       BINARY.
           10 WB-NI-LZUAN-HH-AL-GES       PIC S9(04)       BINARY.
           10 WB-NI-LZUAN-HH-AL-ERF       PIC S9(04)       BINARY.
           10 WB-NI-LZUAN-HH-AL-SOLL      PIC S9(04)       BINARY.
           10 WB-NI-LZUAN-BESHH-STEIG     PIC S9(04)       BINARY.
           10 WB-NI-LZUAN-ES-STEIG        PIC S9(04)       BINARY.
           10 WB-NI-LZUAN-ERT-GRUPPE      PIC S9(04)       BINARY.
           10 WB-NI-LZUAN-QUALI-KZ        PIC S9(04)       BINARY.
           10 WB-NI-LZUAN-HH-ERF-KZ       PIC S9(04)       BINARY.
           10 WB-NI-LZUAN-EH-UPFL         PIC S9(04)       BINARY.

        05 WB-VIVBB211.
           10 WB-LZUAN-ST-NR              PIC S9(09)       BINARY.
           10 WB-LZUAN-ANSPR-VOM          PIC  X(10).
           10 WB-KNR-NERF-GRUND           PIC S9(04)       BINARY.
           10 WB-KTY-NERF-GRUND           PIC S9(04)       BINARY.
           10 WB-LZUAN-EH                 PIC S9(09)V9(01) COMP-3.
           10 WB-LZUAN-LAST-UPDATE        PIC  X(26).
           10 WB-LZUAN-HH-VBA-GES         PIC S9(04)       BINARY.
           10 WB-LZUAN-HH-VBA-ERF         PIC S9(04)       BINARY.
           10 WB-LZUAN-HH-VBA-SOLL        PIC S9(04)       BINARY.
           10 WB-LZUAN-HH-AL-GES          PIC S9(04)       BINARY.
           10 WB-LZUAN-HH-AL-ERF          PIC S9(04)       BINARY.
           10 WB-LZUAN-HH-AL-SOLL         PIC S9(04)       BINARY.
           10 WB-LZUAN-BESHH-STEIG        PIC S9(09)V9(02) COMP-3.
           10 WB-LZUAN-BES-STEIG          PIC S9(09)V9(02) COMP-3.
           10 WB-LZUAN-WERT-GRUPPE        PIC S9(04)       BINARY.
           10 WB-LZUAN-QUALI-KZ           PIC  X(01).
           10 WB-LZUAN-HH-ERF-KZ          PIC  X(01).
           10 WB-LZUAN-EH-UPFL            PIC S9(09)V9(01) COMP-3.

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  18                    *
      *****************************************************************


