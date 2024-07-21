      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.01.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0111.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVB0111.CPv  $
      *
      *   Rev 2.1   25 Feb 2008 09:57:08   t13ex43
      *ASL00292: PGM - Direktions-Aufbau-Plan
      *
      *   Rev 2.0   28 Jan 2008 15:33:00   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVB0111                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVB0111 (DAPA)                            *
      *    --------------------------------------------------------   *
      *    ERSTELLT: BERND KLEEMEIER                                  *
      *    DATUM:    28.01.2008                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * ASL00292!28.01.08!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0111                          *
      *****************************************************************
        05  WB-NI-VIVB0111-TAB.
           10 WB-NI-VIVB0111-ELE   PIC S9(04)  BINARY OCCURS  18 TIMES.
        05  WB-NI-VIVB0111         REDEFINES   WB-NI-VIVB0111-TAB.
           10 WB-NI-MA-ST-NR              PIC S9(04)       BINARY.
           10 WB-NI-DAPA-ANSPR-VOM        PIC S9(04)       BINARY.
           10 WB-NI-DAPA-PART-VBNR        PIC S9(04)       BINARY.
           10 WB-NI-KNR-ANSPR-GRUND       PIC S9(04)       BINARY.
           10 WB-NI-KTY-ANSPR-GRUND       PIC S9(04)       BINARY.
           10 WB-NI-DAPA-LAST-UPDATE      PIC S9(04)       BINARY.
           10 WB-NI-KTY-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-KNR-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-DAPA-VBNR-TYP         PIC S9(04)       BINARY.
           10 WB-NI-DAPA-STUFE            PIC S9(04)       BINARY.
           10 WB-NI-DAPA-BEF-DAT          PIC S9(04)       BINARY.
           10 WB-NI-DAPA-EH-GESAMT        PIC S9(04)       BINARY.
           10 WB-NI-DAPA-EH-EIGEN         PIC S9(04)       BINARY.
           10 WB-NI-DAPA-EQF-GESAMT       PIC S9(04)       BINARY.
           10 WB-NI-DAPA-RATE             PIC S9(04)       BINARY.
           10 WB-NI-DAPA-ANSPR-BETRAG     PIC S9(04)       BINARY.
           10 WB-NI-KTY-ANSPR-STATUS      PIC S9(04)       BINARY.
           10 WB-NI-KNR-ANSPR-STATUS      PIC S9(04)       BINARY.

        05  WB-VIVB0111.
           10 WB-MA-ST-NR                 PIC S9(09)       BINARY.
           10 WB-DAPA-ANSPR-VOM           PIC  X(10).
           10 WB-DAPA-PART-VBNR           PIC S9(09)       BINARY.
           10 WB-KNR-ANSPR-GRUND          PIC S9(04)       BINARY.
           10 WB-KTY-ANSPR-GRUND          PIC S9(04)       BINARY.
           10 WB-DAPA-LAST-UPDATE         PIC  X(26).
           10 WB-KTY-WAEHRUNG             PIC S9(04)       BINARY.
           10 WB-KNR-WAEHRUNG             PIC S9(04)       BINARY.
           10 WB-DAPA-VBNR-TYP            PIC  X(01).
           10 WB-DAPA-STUFE               PIC  X(04).
           10 WB-DAPA-BEF-DAT             PIC  X(10).
           10 WB-DAPA-EH-GESAMT           PIC S9(09)V9(01) COMP-3.
           10 WB-DAPA-EH-EIGEN            PIC S9(09)V9(01) COMP-3.
           10 WB-DAPA-EQF-GESAMT          PIC S9(03)V9(02) COMP-3.
           10 WB-DAPA-RATE                PIC  X(01).
           10 WB-DAPA-ANSPR-BETRAG        PIC S9(09)V9(02) COMP-3.
           10 WB-KTY-ANSPR-STATUS         PIC S9(04)       BINARY.
           10 WB-KNR-ANSPR-STATUS         PIC S9(04)       BINARY.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 18      *
      ******************************************************************
