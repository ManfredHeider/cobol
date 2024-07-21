      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0077.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WVVB0077.CPv  $
      *
      *   Rev 2.1   Apr 18 2000 12:43:06   T13AE26
      *ASL00046 Einführung Förderbonus (Dimension 7)
      *
      *   Rev 2.0   Mar 30 2000 09:38:14   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0075                           *
      ******************************************************************
        05  WB-NI-VIVB0077-TAB.
           10 WB-NI-VIVB0077-ELE   PIC S9(04)  BINARY OCCURS  11 TIMES.
        05  WB-NI-VIVB0077         REDEFINES   WB-NI-VIVB0077-TAB.
           10 WB-NI-ST-NR                 PIC S9(04)       BINARY.
           10 WB-NI-FBAZ-ANSPR-VOM        PIC S9(04)       BINARY.
           10 WB-NI-FBAZ-UNTST-NR         PIC S9(04)       BINARY.
           10 WB-NI-KNR-ANSPR-FBONUS      PIC S9(04)       BINARY.
           10 WB-NI-FBAZ-LAST-UPDATE      PIC S9(04)       BINARY.
           10 WB-NI-KTY-ANSPR-FBONUS      PIC S9(04)       BINARY.
           10 WB-NI-KTY-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-KNR-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-FBAZ-ANSPR-BETRAG     PIC S9(04)       BINARY.
           10 WB-NI-KTY-ANSPR-STATUS      PIC S9(04)       BINARY.
           10 WB-NI-KNR-ANSPR-STATUS      PIC S9(04)       BINARY.

        05  WB-VIVB0077.
           10 WB-ST-NR                    PIC S9(09)       BINARY.
           10 WB-FBAZ-ANSPR-VOM           PIC X(10).
           10 WB-FBAZ-UNTST-NR            PIC S9(09)       BINARY.
           10 WB-KNR-ANSPR-FBONUS         PIC S9(04)       BINARY.
           10 WB-FBAZ-LAST-UPDATE         PIC X(26).
           10 WB-KTY-ANSPR-FBONUS         PIC S9(04)       BINARY.
           10 WB-KTY-WAEHRUNG             PIC S9(04)       BINARY.
           10 WB-KNR-WAEHRUNG             PIC S9(04)       BINARY.
           10 WB-FBAZ-ANSPR-BETRAG        PIC S9(09)V9(02) USAGE COMP-3.
           10 WB-KTY-ANSPR-STATUS         PIC S9(04)       BINARY.
           10 WB-KNR-ANSPR-STATUS         PIC S9(04)       BINARY.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 11      *
      ******************************************************************