      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUPA0104.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WUPA0104.CPv  $
      *
      *   Rev 2.1   20 Jul 2001 10:29:00   T13EX31
      *APA00143 - DB2-Datenmigration Euro-Umstellung
      *
      *   Rev 2.0   08 Jun 2001 08:35:00   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      * TBPA0104                                                       *
      ******************************************************************
         05  TBPA0104.
           10 PGEB-GES             PIC S9(4) USAGE COMP.
           10 PDTA-ID              PIC S9(15)V USAGE COMP-3.
           10 PDIF-PGV             PIC S9(4) USAGE COMP.
           10 PDIF-VSTAT           PIC X(4).
           10 PDIF-ANZ-BEWEGUNG    PIC S9(9) USAGE COMP.
           10 PDIF-GUTSCHRIFTEN    PIC S9(13)V9(2) USAGE COMP-3.
           10 PDIF-BELASTUNGEN     PIC S9(13)V9(2) USAGE COMP-3.
           10 PDIF-LAST-UPDATE     PIC X(26).
           10 PDIF-KENNUNG         PIC X(8).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8       *
      ******************************************************************
