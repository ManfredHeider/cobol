      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUAN0001.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WUAN0001.CPv  $
      *
      *   Rev 2.1   13 Sep 2001 17:05:02   t13ex33
      *AAN00320 - EURO-Umstellung - euro-vorbereitet
      *
      *   Rev 2.0   30 Aug 2001 15:44:28   T13EX31
      *AAN00312 - Euro-Umstellung
      *
      *   Rev 1.1   30 Aug 2001 15:40:42   T13EX31
      *AAN00312 - Euro-Umstellung
      *
      *   Rev 1.0   30 May 2001 08:26:30   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBAN0001                           *
      ******************************************************************
        05  WUAN0001.
           10 VVNR-PIN             PIC S9(9) USAGE COMP.
           10 PGEB-GES             PIC S9(4) USAGE COMP.
           10 VVNR-VOR-VNR         PIC X(15).
           10 KNR-PGRP             PIC S9(4) USAGE COMP.
           10 KTY-PGRP             PIC S9(4) USAGE COMP.
           10 VVNR-KD-BETREUER     PIC S9(9) USAGE COMP.
           10 VVNR-AUTOMATISCH     PIC X(1).
           10 VVNR-HINWEIS         PIC X(4).
           10 VVNR-BEITRAG         PIC S9(9)V9(2) USAGE COMP-3.
           10 KNR-WAEHRUNG         PIC S9(4) USAGE COMP.
           10 KTY-WAEHRUNG         PIC S9(4) USAGE COMP.
           10 VVNR-ERSTELLUNG      PIC X(26).
           10 VVNR-LAST-UPDATE     PIC X(26).
           10 VVNR-KENNUNG         PIC X(8).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 13      *
      ******************************************************************
