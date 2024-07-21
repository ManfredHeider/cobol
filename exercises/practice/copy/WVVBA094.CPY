      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  11.12.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBA094.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/WVVBA094.CPv  $
      *
      *   Rev 2.2   Jan 21 2016 13:45:56   t13ae30
      *ASL00552: Leistungszuschuss Allfinanz
      *
      *   Rev 2.1   Dec 16 2014 10:51:58   t13ex13
      *ASL00527: Leistungszuschuﬂ Allfinanz
      *
      *   Rev 2.0   Dec 11 2014 14:16:56   t13ex13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVBA094                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH F‹R VIVBA094 (LZUB    )                        *
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
      * COBOL DEKLARATION F‹R VIEW VIVBA094                           *
      *****************************************************************

        05 WB-NI-VIVBA094-TAB.
           10 WB-NI-VIVBA094-ELE   PIC S9(04)  BINARY OCCURS  23.
        05 WB-NI-VIVBA094          REDEFINES   WB-NI-VIVBA094-TAB.
           10 WB-NI-LZUB-ST-NR            PIC S9(04)       BINARY.
           10 WB-NI-KTY-OLZ-TYP           PIC S9(04)       BINARY.
           10 WB-NI-KNR-OLZ-TYP           PIC S9(04)       BINARY.
           10 WB-NI-LZUB-STICHTAG         PIC S9(04)       BINARY.
           10 WB-NI-LZUB-ANSPR-VOM        PIC S9(04)       BINARY.
           10 WB-NI-LZUB-LAST-UPDATE      PIC S9(04)       BINARY.
           10 WB-NI-LZUB-MS-NR            PIC S9(04)       BINARY.
           10 WB-NI-LZUB-VBP-NR           PIC S9(04)       BINARY.
           10 WB-NI-LZUB-12MON-UMSATZ     PIC S9(04)       BINARY.
           10 WB-NI-LZUB-03MON-UMSATZ     PIC S9(04)       BINARY.
           10 WB-NI-LZUB-FAKTOR           PIC S9(04)       BINARY.
           10 WB-NI-LZUB-ANZ-MONATE       PIC S9(04)       BINARY.
           10 WB-NI-LZUB-HH-VBA-GES       PIC S9(04)       BINARY.
           10 WB-NI-LZUB-HH-VBA-ERF       PIC S9(04)       BINARY.
           10 WB-NI-LZUB-HH-VBA-SOLL      PIC S9(04)       BINARY.
           10 WB-NI-LZUB-HH-AL-GES        PIC S9(04)       BINARY.
           10 WB-NI-LZUB-HH-AL-ERF        PIC S9(04)       BINARY.
           10 WB-NI-LZUB-HH-AL-SOLL       PIC S9(04)       BINARY.
           10 WB-NI-LZUB-BESHH-STEIG      PIC S9(04)       BINARY.
           10 WB-NI-LZUB-BES-STEIG        PIC S9(04)       BINARY.
           10 WB-NI-LZUB-WERT-GRUPPE      PIC S9(04)       BINARY.
           10 WB-NI-LZUB-QUALI-KZ         PIC S9(04)       BINARY.
           10 WB-NI-LZUB-HH-ERF-KZ        PIC S9(04)       BINARY.

        05 WB-VIVBA094.
           10 WB-LZUB-ST-NR               PIC S9(09)       BINARY.
           10 WB-KTY-OLZ-TYP              PIC S9(04)       BINARY.
           10 WB-KNR-OLZ-TYP              PIC S9(04)       BINARY.
           10 WB-LZUB-STICHTAG            PIC  X(10).
           10 WB-LZUB-ANSPR-VOM           PIC  X(10).
           10 WB-LZUB-LAST-UPDATE         PIC  X(26).
           10 WB-LZUB-MS-NR               PIC S9(04)       BINARY.
           10 WB-LZUB-VBP-NR              PIC S9(04)       BINARY.
           10 WB-LZUB-12MON-UMSATZ        PIC S9(09)V9(01) COMP-3.
           10 WB-LZUB-03MON-UMSATZ        PIC S9(09)V9(01) COMP-3.
           10 WB-LZUB-FAKTOR              PIC S9(03)V9(02) COMP-3.
           10 WB-LZUB-ANZ-MONATE          PIC S9(04)       BINARY.
           10 WB-LZUB-HH-VBA-GES          PIC S9(04)       BINARY.
           10 WB-LZUB-HH-VBA-ERF          PIC S9(04)       BINARY.
           10 WB-LZUB-HH-VBA-SOLL         PIC S9(04)       BINARY.
           10 WB-LZUB-HH-AL-GES           PIC S9(04)       BINARY.
           10 WB-LZUB-HH-AL-ERF           PIC S9(04)       BINARY.
           10 WB-LZUB-HH-AL-SOLL          PIC S9(04)       BINARY.
           10 WB-LZUB-BESHH-STEIG         PIC S9(09)V9(02) COMP-3.
           10 WB-LZUB-BES-STEIG           PIC S9(09)V9(02) COMP-3.
           10 WB-LZUB-WERT-GRUPPE         PIC S9(04)       BINARY.
           10 WB-LZUB-QUALI-KZ            PIC  X(01).
           10 WB-LZUB-HH-ERF-KZ           PIC  X(01).

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  23                    *
      *****************************************************************

