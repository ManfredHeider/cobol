      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUPA0117.CPY  $
      *    $Revision:   1.1  $
      *
      *    $Log:   L:\MF\CPY\WUPA0117.CPv  $
      *
      *   Rev 1.1   20 Jul 2001 10:30:06   T13EX31
      *APA00143 - DB2-Datenmigration Euro-Umstellung
      *
      *   Rev 1.0   30 May 2001 08:29:14   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      * TBPA0117                                                       *
      ******************************************************************
         05  DCL-TBPA0117.
           10 PMPT-ID              PIC S9(15)V USAGE COMP-3.
           10 PMPT-VERSION         PIC S9(4) USAGE COMP.
           10 PMPT-V-GUELTIG-AB    PIC X(26).
           10 PMPT-V-GUELTIG-BIS   PIC X(10).
           10 PMPT-V-KENNUNG       PIC X(8).
           10 PVPT-ID              PIC S9(15)V USAGE COMP-3.
           10 PVPT-VERSION         PIC S9(4) USAGE COMP.
           10 PMPR-ID              PIC S9(15)V USAGE COMP-3.
           10 PMPR-VERSION         PIC S9(4) USAGE COMP.
           10 PMBE-ID              PIC S9(15)V USAGE COMP-3.
           10 PRINTE               PIC S9(15)V USAGE COMP-3.
           10 PGEB-GES             PIC S9(4) USAGE COMP.
           10 PROD-PROD-SCHL       PIC S9(4) USAGE COMP.
           10 KNR-PMPT-ART         PIC S9(4) USAGE COMP.
           10 PMPT-VSTAT           PIC X(4).
           10 PMPT-FAELLIG-VON     PIC X(10).
           10 PMPT-FAELLIG-BIS     PIC X(10).
           10 PMPT-PPFL-BEITRAG    PIC S9(9)V9(2) USAGE COMP-3.
           10 PMPT-PPFL-SUMME      PIC S9(9)V9(2) USAGE COMP-3.
           10 PMPT-GSMT-PROV       PIC S9(9)V9(2) USAGE COMP-3.
           10 PMPT-GRUND-PROV      PIC S9(9)V9(2) USAGE COMP-3.
           10 PMPT-EINH            PIC S9(6)V9(2) USAGE COMP-3.
           10 PRKO-KOND-NR-GRP     PIC S9(4) USAGE COMP.
           10 PRKO-KOND-NR-EINH    PIC S9(4) USAGE COMP.
           10 PRKO-KOND-NR-KDB     PIC S9(4) USAGE COMP.
           10 PMPT-LAST-UPDATE     PIC X(26).
           10 PMPT-KENNUNG         PIC X(8).
      ******************************************************************
