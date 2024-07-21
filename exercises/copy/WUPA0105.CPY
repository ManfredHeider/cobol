      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUPA0105.CPY  $
      *    $Revision:   1.1  $
      *
      *    $Log:   L:\MF\CPY\WUPA0105.CPv  $
      *
      *   Rev 1.1   20 Jul 2001 10:29:16   T13EX31
      *APA00143 - DB2-Datenmigration Euro-Umstellung
      *
      *   Rev 1.0   30 May 2001 08:29:10   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      * TBPA0105                                                       *
      ******************************************************************
         05  WU-TBPA0105.
           10 PNAP-ID              PIC S9(15)V USAGE COMP-3.
           10 PNAP-VERSION         PIC S9(4) USAGE COMP.
           10 KNR-OBJEKT-HKF-TAB   PIC S9(4) USAGE COMP.
           10 KNR-PNAP-OBJEKT      PIC S9(4) USAGE COMP.
           10 KNR-PNAP-ZUSTAND     PIC S9(4) USAGE COMP.
           10 KTY-PNAP-ZUSTAND     PIC S9(4) USAGE COMP.
           10 PNAP-FAELLIG-VON     PIC X(10).
           10 PRINTE               PIC S9(15)V USAGE COMP-3.
           10 PGEB-GES             PIC S9(4) USAGE COMP.
           10 PNAP-VNR             PIC X(15).
           10 PNAP-PIN             PIC S9(9) USAGE COMP.
           10 PVER-KUNDE-NAME      PIC X(25).
           10 PNAP-GSMT-PROV       PIC S9(9)V9(2) USAGE COMP-3.
           10 PNAP-PG-ABR-DAT      PIC X(10).
           10 PNAP-PROD-SCHL-1     PIC S9(4) USAGE COMP.
           10 PNAP-PROD-SCHL-2     PIC S9(4) USAGE COMP.
           10 PNAP-PROD-SCHL-3     PIC S9(4) USAGE COMP.
           10 PNAP-PROD-SCHL-KZ    PIC X(1).
           10 PNAP-VSTAT           PIC X(4).
           10 PNAP-VSTAT-UNR       PIC S9(4) USAGE COMP.
           10 PNAP-LAST-UPDATE     PIC X(26).
           10 PNAP-KENNUNG         PIC X(8).
      ******************************************************************
