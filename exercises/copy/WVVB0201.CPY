      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01.06.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0201.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVB0201.CPv  $
      *
      *   Rev 2.1   Jun 11 2010 08:50:36   t13ex43
      *ASL00353: PGM - Nichterfüller Columbus-Bonus
      *
      *   Rev 2.0   Jun 01 2010 10:25:22   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVB0201                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVB0201 (CBAN)                            *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    01.06.2010                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00353!01.06.10!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVB0201                           *
      *****************************************************************

        05 WB-NI-VIVB0201-TAB.
           10 WB-NI-VIVB0201-ELE   PIC S9(04)  BINARY OCCURS  13.
        05 WB-NI-VIVB0201          REDEFINES   WB-NI-VIVB0201-TAB.
           10 WB-NI-MA-ST-NR              PIC S9(04)       BINARY.
           10 WB-NI-CBAN-ANSPR-VOM        PIC S9(04)       BINARY.
           10 WB-NI-CBAN-PART-VBNR        PIC S9(04)       BINARY.
           10 WB-NI-KNR-NERF-GRUND        PIC S9(04)       BINARY.
           10 WB-NI-KTY-NERF-GRUND        PIC S9(04)       BINARY.
           10 WB-NI-CBAN-LAST-UPDATE      PIC S9(04)       BINARY.
           10 WB-NI-KTY-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-KNR-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-CBAN-PART-BETRAG      PIC S9(04)       BINARY.
           10 WB-NI-CBAN-PART-EH          PIC S9(04)       BINARY.
           10 WB-NI-CBAN-PART-SEIT        PIC S9(04)       BINARY.
           10 WB-NI-KTY-NERF-STATUS       PIC S9(04)       BINARY.
           10 WB-NI-KNR-NERF-STATUS       PIC S9(04)       BINARY.

        05 WB-VIVB0201.
           10 WB-MA-ST-NR                 PIC S9(09)       BINARY.
           10 WB-CBAN-ANSPR-VOM           PIC  X(10).
           10 WB-CBAN-PART-VBNR           PIC S9(09)       BINARY.
           10 WB-KNR-NERF-GRUND           PIC S9(04)       BINARY.
           10 WB-KTY-NERF-GRUND           PIC S9(04)       BINARY.
           10 WB-CBAN-LAST-UPDATE         PIC  X(26).
           10 WB-KTY-WAEHRUNG             PIC S9(04)       BINARY.
           10 WB-KNR-WAEHRUNG             PIC S9(04)       BINARY.
           10 WB-CBAN-PART-BETRAG         PIC S9(09)V9(02) COMP-3.
           10 WB-CBAN-PART-EH             PIC S9(09)V9(01) COMP-3.
           10 WB-CBAN-PART-SEIT           PIC  X(10).
           10 WB-KTY-NERF-STATUS          PIC S9(04)       BINARY.
           10 WB-KNR-NERF-STATUS          PIC S9(04)       BINARY.

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  13                    *
      *****************************************************************
