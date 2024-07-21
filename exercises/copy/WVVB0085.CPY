      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0085.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/MF/CPY/WVVB0085.CPv  $
      *
      *   Rev 2.2   Jul 10 2002 15:58:52   t13ae30
      *AVB00587 - VB-EMAIL / VB-HOMEPAGE
      *
      *   Rev 2.1   Jul 10 2002 14:33:14   t13ae30
      *AVB00587 - VB-EMAIL / VB-HOMEPAGE
      *
      *   Rev 2.0   20 Feb 2001 13:29:36   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0085                           *
      ******************************************************************
        05  WB-NI-VIVB0085-TAB.
           10 WB-NI-VIVB0085-ELE    PIC S9(04) BINARY OCCURS  7  TIMES.
        05  WB-NI-VIVB0085            REDEFINES   WB-NI-VIVB0085-TAB.
           10 WB-NI-KTY-ANW-NR              PIC S9(04)  BINARY.
           10 WB-NI-KNR-ANW-NR              PIC S9(04)  BINARY.
           10 WB-NI-KTY-AUSN-TYP            PIC S9(04)  BINARY.
           10 WB-NI-KNR-AUSN-TYP            PIC S9(04)  BINARY.
           10 WB-NI-AUSN-LAST-UPDATE        PIC S9(04)  BINARY.
           10 WB-NI-AUSN-STRING-ALT         PIC S9(04)  BINARY.
           10 WB-NI-AUSN-STRING-NEU         PIC S9(04)  BINARY.
        05 WB-VIVB0085.
           10 WB-KTY-ANW-NR                 PIC S9(4)   USAGE COMP.
           10 WB-KNR-ANW-NR                 PIC S9(4)   USAGE COMP.
           10 WB-KTY-AUSN-TYP               PIC S9(4)   USAGE COMP.
           10 WB-KNR-AUSN-TYP               PIC S9(4)   USAGE COMP.
           10 WB-AUSN-LAST-UPDATE           PIC X(26).
           10 WB-AUSN-STRING-ALT            PIC X(70).
           10 WB-AUSN-STRING-NEU            PIC X(70).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 7       *
      ******************************************************************
