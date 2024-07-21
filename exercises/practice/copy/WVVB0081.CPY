      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0081.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:\MF\CPY\WVVB0081.CPv  $
      *
      *   Rev 2.3   19 Jun 2001 10:13:42   t13ae13
      *AVB00534: Einführung Masterpaßwort für VB-Portal
      *
      *   Rev 2.2   18 May 2001 15:49:44   t13ae30
      * 
      *
      *   Rev 2.1   09 May 2001 14:43:32   t13ae30
      * 
      *
      *   Rev 2.0   20 Feb 2001 13:29:34   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0081                           *
      ******************************************************************
        05 WB-VIVB0081.
           10 WB-MA-ST-NR                     PIC S9(9) USAGE COMP.
180601*    10 WB-MPW                          PIC S9(4) USAGE COMP.
180601     10 WB-MPW                          PIC X(10).
           10 WB-MMPZ-DAT-VON                 PIC X(26).
           10 WB-MMPZ-LAST-UPDATE             PIC X(26).
           10 WB-MMPZ-DAT-BIS                 PIC X(10).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 5       *
      ******************************************************************
