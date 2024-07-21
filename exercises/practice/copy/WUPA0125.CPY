      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUPA0125.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WUPA0125.CPv  $
      *
      *   Rev 2.1   20 Jul 2001 10:30:38   T13EX31
      *APA00143 - DB2-Datenmigration Euro-Umstellung
      *
      *   Rev 2.0   08 Jun 2001 08:35:06   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      * TBPA0125                                                       *
      ******************************************************************
         05  TBPA0125.
           10 PDTA-ID              PIC S9(15)V USAGE COMP-3.
           10 PGEB-GES             PIC S9(4) USAGE COMP.
           10 KNR-FKT-BEREICH      PIC S9(4) USAGE COMP.
           10 KNR-WAEHRUNG         PIC S9(4) USAGE COMP.
           10 KOF-GSMT-PROV        PIC S9(13)V9(2) USAGE COMP-3.
           10 KOF-ANZ-BEWEGUNG     PIC S9(9) USAGE COMP.
           10 KOF-ABSCHLUSS-DAT    PIC X(10).
           10 KOF-LAST-UPDATE      PIC X(26).
           10 KOF-KENNUNG          PIC X(8).
