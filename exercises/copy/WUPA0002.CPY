      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUPA0002.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WUPA0002.CPv  $
      *
      *   Rev 2.1   30 Aug 2001 15:56:56   T13EX31
      *APA00143 - Euro-Umstellung
      *
      *   Rev 2.0   30 Aug 2001 15:54:36   T13EX31
      *APA00143 - Euro-Umstellung
      *
      *   Rev 1.1   20 Jul 2001 10:27:22   T13EX31
      *APA00143 - DB2-Datenmigration Euro-Umstellung
      *
      *   Rev 1.0   30 May 2001 08:29:06   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      * TBPA0002                                                       *
      ******************************************************************
         05  WU-TBPA0002.
           10 PEWA-ID              PIC S9(15)V USAGE COMP-3.
           10 PGEB-GES             PIC S9(4) USAGE COMP.
           10 PROD-PROD-SCHL       PIC S9(4) USAGE COMP.
           10 PEWP-TARIF           PIC X(12).
           10 PEWP-LAUFZEIT        PIC S9(4) USAGE COMP.
           10 PEWP-ABLAUF          PIC X(10).
           10 PRKO-KOND-NR-GSP     PIC S9(4) USAGE COMP.
           10 PRKO-KOND-NR-GRP     PIC S9(4) USAGE COMP.
           10 PRKO-KOND-NR-EINH    PIC S9(4) USAGE COMP.
           10 PEWP-BEITRAG         PIC S9(9)V9(2) USAGE COMP-3.
           10 PEWP-SUMME           PIC S9(9)V9(2) USAGE COMP-3.
           10 PEWP-AVERM           PIC S9(9) USAGE COMP.
           10 PEWP-BETR            PIC S9(9)V9(2) USAGE COMP-3.
           10 PEWP-BETR-AVERM      PIC S9(9)V9(2) USAGE COMP-3.
           10 PEWP-EINH            PIC S9(6)V9(2) USAGE COMP-3.
           10 PEWP-EINH-AVERM      PIC S9(6)V9(2) USAGE COMP-3.
           10 PEWP-RMELD-DAT       PIC X(10).
           10 PEWP-BEWERT-BETRAG   PIC S9(9)V9(2) USAGE COMP-3.
           10 KNR-WAEHRUNG         PIC S9(4) USAGE COMP.
           10 PEWP-BTO-BEITRAG     PIC S9(9)V9(2) USAGE COMP-3.
           10 PEWP-VSTAT           PIC X(4).
           10 PEWP-VSTAT-UNR       PIC S9(4) USAGE COMP.
           10 PEWP-LAST-UPDATE     PIC X(26).
           10 PEWP-KENNUNG         PIC X(8).
      ******************************************************************
