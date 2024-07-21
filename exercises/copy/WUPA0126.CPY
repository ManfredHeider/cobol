      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUPA0126.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WUPA0126.CPv  $
      *
      *   Rev 2.1   20 Jul 2001 10:30:54   T13EX31
      *APA00143 - DB2-Datenmigration Euro-Umstellung
      *
      *   Rev 2.0   08 Jun 2001 08:35:06   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      * TBPA0126                                                       *
      ******************************************************************
         05  TBPA0126.
           10 PDTA-ID              PIC S9(15)V USAGE COMP-3.
           10 PGEB-GES             PIC S9(4) USAGE COMP.
           10 KNR-FKT-BEREICH-AB   PIC S9(4) USAGE COMP.
           10 KNR-FKT-BEREICH-ZU   PIC S9(4) USAGE COMP.
           10 KNR-WAEHRUNG         PIC S9(4) USAGE COMP.
           10 KOFV-GSMT-PROV       PIC S9(13)V9(2) USAGE COMP-3.
           10 KOFV-ANZ-BEW-AB      PIC S9(9) USAGE COMP.
           10 KOFV-ANZ-BEW-ZU      PIC S9(9) USAGE COMP.
           10 KOFV-ABSCHLUSS-DAT   PIC X(10).
           10 KOFV-LAST-UPDATE     PIC X(26).
           10 KOFV-KENNUNG         PIC X(8).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 11      *
      ******************************************************************
