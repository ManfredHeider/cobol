      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUPA0111.CPY  $
      *    $Revision:   1.1  $
      *
      *    $Log:   L:\MF\CPY\WUPA0111.CPv  $
      *
      *   Rev 1.1   20 Jul 2001 10:29:32   T13EX31
      *APA00143 - DB2-Datenmigration Euro-Umstellung
      *
      *   Rev 1.0   30 May 2001 08:29:10   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      * TBPA0111                                                       *
      ******************************************************************
         05  WU-TBPA0111.
           10 PVPT-ID              PIC S9(15)V USAGE COMP-3.
           10 PVPT-VERSION         PIC S9(4) USAGE COMP.
           10 PVPT-F-GUELTIG-AB    PIC X(10).
           10 PVPT-F-GUELTIG-BIS   PIC X(10).
           10 PVPT-V-GUELTIG-AB    PIC X(26).
           10 PVPT-V-GUELTIG-BIS   PIC X(10).
           10 PVPT-V-KENNUNG       PIC X(8).
           10 KNR-PVPT-ART         PIC S9(4) USAGE COMP.
           10 PRINTE               PIC S9(15)V USAGE COMP-3.
           10 PVER-VERSION         PIC S9(4) USAGE COMP.
           10 PGEB-GES             PIC S9(4) USAGE COMP.
           10 PROD-PROD-SCHL       PIC S9(4) USAGE COMP.
           10 PVPT-VT-PRIO         PIC S9(4) USAGE COMP.
           10 PVPT-PPFL-BEGINN     PIC X(10).
           10 PVPT-PPFL-ENDE       PIC X(10).
           10 PVPT-BEWERT-BETRAG   PIC S9(9)V9(2) USAGE COMP-3.
           10 PVPT-PTEIL-BEGINN    PIC X(10).
           10 PVPT-PTEIL-ABLAUF    PIC X(10).
           10 PVPT-LAUFZEIT        PIC S9(4) USAGE COMP.
           10 PRKO-KOND-NR-EE      PIC S9(4) USAGE COMP.
           10 PVPT-EE-ENTSCHEID    PIC X(1).
           10 KNR-EVS-GRUND        PIC S9(4) USAGE COMP.
           10 KNR-WAEHRUNG         PIC S9(4) USAGE COMP.
           10 PEWA-ID              PIC S9(15)V USAGE COMP-3.
           10 PVPT-PIN             PIC S9(9) USAGE COMP.
           10 PEST-ID              PIC S9(15)V USAGE COMP-3.
           10 PVPT-LAST-UPDATE     PIC X(26).
           10 PVPT-KENNUNG         PIC X(8).
      ******************************************************************
