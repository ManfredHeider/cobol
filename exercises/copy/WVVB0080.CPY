      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0080.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:\MF\CPY\WVVB0080.CPv  $
      *
      *   Rev 2.3   25 Jun 2001 11:56:22   t13ae13
      *AVB00534: Einführung PIN für VB-Portal
      *
      *   Rev 2.2   18 May 2001 15:49:40   t13ae30
      * 
      *AAS00274 - PIN für VB-Portal
      *   Rev 2.1   09 May 2001 14:43:28   t13ae30
      * 
      *   Rev 2.0   20 Feb 2001 13:29:34   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0080                           *
      ******************************************************************
        05 WB-VIVB0080.
           10 WB-MPW                        PIC X(10).
           10 WB-MPW-LAST-UPDATE            PIC X(26).
           10 WB-KTY-MPW-STATUS             PIC S9(4)  USAGE COMP.
           10 WB-KNR-MPW-STATUS             PIC S9(4)  USAGE COMP.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 4       *
      ******************************************************************
