      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0086.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/WVVB0086.CPv  $
      *
      *   Rev 2.1   Jan 15 2003 10:24:24   t13ae27
      *neu
      *
      *   Rev 2.0   Nov 11 2002 11:25:36   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0086                           *
      ******************************************************************
        05  WB-NI-VIVB0086-TAB.
           10 WB-NI-VIVB0086-ELE    PIC S9(04) BINARY OCCURS 15  TIMES.
        05  WB-NI-VIVB0086            REDEFINES   WB-NI-VIVB0086-TAB.
           10 WB-NI-BBON-BETR-NR            PIC S9(04)  BINARY.
           10 WB-NI-BBON-UNTST-NR           PIC S9(04)  BINARY.
           10 WB-NI-BBON-DAT-ANSPRUCH       PIC S9(04)  BINARY.
           10 WB-NI-BBON-DAT-VON            PIC S9(04)  BINARY.
           10 WB-NI-BBON-DAT-BIS            PIC S9(04)  BINARY.
           10 WB-NI-BBON-DAT-VON-BETR       PIC S9(04)  BINARY.
           10 WB-NI-BBON-PROV-BETR          PIC S9(04)  BINARY.
           10 WB-NI-BBON-DAT-VON-UNTST      PIC S9(04)  BINARY.
           10 WB-NI-BBON-GRUND              PIC S9(04)  BINARY.
           10 WB-NI-BBON-PROV-UNTST         PIC S9(04)  BINARY.
           10 WB-NI-BBON-BETRAG             PIC S9(04)  BINARY.
           10 WB-NI-BBON-MONATE             PIC S9(04)  BINARY.
           10 WB-NI-BBON-LAST-UPDATE        PIC S9(04)  BINARY.
           10 WB-NI-BBON-BES-GRUND          PIC S9(04)  BINARY.
           10 WB-NI-BBON-CREATOR            PIC S9(04)  BINARY.
        05 WB-VIVB0086.
           10 WB-BBON-BETR-NR           PIC S9(009)     USAGE COMP.
           10 WB-BBON-UNTST-NR          PIC S9(009)     USAGE COMP.
           10 WB-BBON-DAT-ANSPRUCH      PIC X(010).
           10 WB-BBON-DAT-VON           PIC X(026).
           10 WB-BBON-DAT-BIS           PIC X(010).
           10 WB-BBON-DAT-VON-BETR      PIC X(026).
           10 WB-BBON-PROV-BETR         PIC S9(003)V99  USAGE COMP-3.
           10 WB-BBON-DAT-VON-UNTST     PIC X(026).
           10 WB-BBON-GRUND             PIC X(004).
           10 WB-BBON-PROV-UNTST        PIC S9(003)V99  USAGE COMP-3.
           10 WB-BBON-BETRAG            PIC S9(009)V9(2) USAGE COMP-3.
           10 WB-BBON-MONATE            PIC S9(004)     COMP.
           10 WB-BBON-LAST-UPDATE       PIC X(026).
           10 WB-BBON-BES-GRUND         PIC S9(004)     COMP.
           10 WB-BBON-CREATOR           PIC X(008).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 7       *
      ******************************************************************
