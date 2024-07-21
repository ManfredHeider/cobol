      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  05.09.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0211.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/WVVB0211.CPv  $
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
      *    COPY-MEMBER: WVVB0211                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVB0211 (LZUAN)                           *
      *    --------------------------------------------------------   *
      *    ERSTELLT: RALF GUTFLEISCH                                  *
      *    DATUM:    05.09.2011                                       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00388!05.09.11! 2.1! NEU                       !GUTFLEISCH*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0211                           *
      ******************************************************************
        05  WB-NI-VIVB0211-TAB.
           10 WB-NI-VIVB0211-ELE   PIC S9(04)  BINARY OCCURS  6 TIMES.
        05  WB-NI-VIVB0211         REDEFINES   WB-NI-VIVB0211-TAB.
           10 WB-NI-LZUAN-ST-NR           PIC S9(04)       BINARY.
           10 WB-NI-LZUAN-ANSPR-VOM       PIC S9(04)       BINARY.
           10 WB-NI-KNR-NERF-GRUND        PIC S9(04)       BINARY.
           10 WB-NI-KTY-NERF-GRUND        PIC S9(04)       BINARY.
           10 WB-NI-LZUAN-EH              PIC S9(04)       BINARY.
           10 WB-NI-LZUAN-LAST-UPDATE     PIC S9(04)       BINARY.

        05  WB-VIVB0211.
           10 WB-LZUAN-ST-NR              PIC S9(09)       BINARY.
           10 WB-LZUAN-ANSPR-VOM          PIC  X(10).
           10 WB-KNR-NERF-GRUND           PIC S9(04)       BINARY.
           10 WB-KTY-NERF-GRUND           PIC S9(04)       BINARY.
           10 WB-LZUAN-EH                 PIC S9(09)V9     COMP-3.
           10 WB-LZUAN-LAST-UPDATE        PIC  X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 6       *
      ******************************************************************
