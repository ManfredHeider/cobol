      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0082.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:\MF\CPY\WVVB0082.CPv  $
      *
      *   Rev 2.3   31 May 2001 10:41:24   t13ae27
      *neu
      *
      *   Rev 2.2   18 May 2001 15:49:50   t13ae30
      * 
      *
      *   Rev 2.1   17 May 2001 10:56:00   t13ae30
      *AAS00274 - PIN für VB-Portal
      *
      *   Rev 2.0   20 Feb 2001 13:29:34   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0082                           *
      ******************************************************************
        05  WB-NI-VIVB0082-TAB.
           10 WB-NI-VIVB0082-ELE    PIC S9(04) BINARY OCCURS  6  TIMES.
        05  WB-NI-VIVB0082            REDEFINES   WB-NI-VIVB0082-TAB.
           10 WB-NI-LIST-EH-1               PIC S9(04)  BINARY.
           10 WB-NI-LIST-EH-2               PIC S9(04)  BINARY.
           10 WB-NI-LIST-EH-3               PIC S9(04)  BINARY.
           10 WB-NI-LIST-EH-4               PIC S9(04)  BINARY.
           10 WB-NI-LIST-EH-5               PIC S9(04)  BINARY.
           10 WB-NI-LIST-SUM-EH             PIC S9(04)  BINARY.
        05 WB-VIVB0082.
           10 WB-MA-ST-NR                   PIC S9(9)   USAGE COMP.
           10 WB-LIST-MONAT                 PIC X(10).
           10 WB-KTY-WB-TYP                 PIC S9(4)   USAGE COMP.
           10 WB-KNR-WB-TYP                 PIC S9(4)   USAGE COMP.
           10 WB-KTY-WERTUNGS-TYP           PIC S9(4)   USAGE COMP.
           10 WB-KNR-WERTUNGS-TYP           PIC S9(4)   USAGE COMP.
           10 WB-LIST-STUF-BEZ              PIC X(10).
           10 WB-LIST-LAST-UPDATE           PIC X(26).
           10 WB-DIR-NR                     PIC S9(4)   USAGE COMP.
           10 WB-LIST-NAME                  PIC X(25).
           10 WB-LIST-VORNAME               PIC X(20).
           10 WB-LIST-RANG                  PIC S9(9)   USAGE COMP.
           10 WB-LIST-GEWINNER-KZ           PIC X(01).
           10 WB-LIST-EH-1                  PIC S9(9)V9 COMP-3.
           10 WB-LIST-EH-2                  PIC S9(9)V9 COMP-3.
           10 WB-LIST-EH-3                  PIC S9(9)V9 COMP-3.
           10 WB-LIST-EH-4                  PIC S9(9)V9 COMP-3.
           10 WB-LIST-EH-5                  PIC S9(9)V9 COMP-3.
           10 WB-LIST-SUM-EH                PIC S9(9)V9 COMP-3.
           10 WB-KTY-REGION                 PIC S9(4)   USAGE COMP.
           10 WB-KNR-REGION                 PIC S9(4)   USAGE COMP.

      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 21      *
      ******************************************************************
