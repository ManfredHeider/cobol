      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  05.09.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBA211.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/WVVBA211.CPv  $
      *
      *   Rev 2.2   Jan 21 2016 13:46:06   t13ae30
      *ASL00552: Leistungszuschuss Allfinanz
      *
      *   Rev 2.1   Dec 16 2014 10:52:08   t13ex13
      *ASL00527: Leistungszuschuﬂ Allfinanz
      *
      *   Rev 2.2   Oct 20 2011 16:24:04   t13ex13
      *ASL00388: Leistungszuschuss - Allfinanz
      *
      *   Rev 2.1   Oct 20 2011 09:40:40   t13ex13
      *ASL00388: Leistungszuschuss - Allfinanz
      *
      *   Rev 2.0   Sep 05 2011 14:11:38   t13ex13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVBA211                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH F‹R VIVBA211 (LZUAN   )                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: TH.WILLIG                                        *
      *    DATUM:    16.12.2015                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00552!16.12.15!2.1 ! NEU                       ! TH.WILLIG *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION F‹R VIEW VIVBA211                           *
      *****************************************************************

        05 WB-NI-VIVBA211-TAB.
           10 WB-NI-VIVBA211-ELE   PIC S9(04)  BINARY OCCURS  14.
        05 WB-NI-VIVBA211          REDEFINES   WB-NI-VIVBA211-TAB.
           10 WB-NI-LZUAN-ST-NR           PIC S9(04)       BINARY.
           10 WB-NI-LZUAN-ANSPR-VOM       PIC S9(04)       BINARY.
           10 WB-NI-KNR-NERF-GRUND        PIC S9(04)       BINARY.
      *****************************************************************
      *    COPY-MEMBER: WVVBA211                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH F‹R VIVBA211 (LZUAN   )                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: TH.WILLIG                                        *
      *    DATUM:    16.12.2015                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00552!16.12.15!2.1 ! NEU                       ! TH.WILLIG *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION F‹R VIEW VIVBA211                           *
      *****************************************************************

        05 WB-NI-VIVBA211-TAB.
           10 WB-NI-VIVBA211-ELE   PIC S9(04)  BINARY OCCURS  17.
        05 WB-NI-VIVBA211          REDEFINES   WB-NI-VIVBA211-TAB.
           10 WB-NI-LZUAN-ST-NR           PIC S9(04)       BINARY.
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

        05 WB-VIVBA211.
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

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  17                    *
      *****************************************************************

