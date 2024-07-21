      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0334.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/WUKI0334.CPv  $
      *
      *   Rev 2.2   Jan 31 2011 09:59:28   t13ex39
      *AKI00979: Neue Depotansicht  AGI-FondsdepotBank
      *
      *   Rev 2.1   Jan 21 2011 10:14:48   t13ex39
      *AKI00979: Neue Depotansicht  AGI-FondsdepotBank
      *
      *   Rev 1.2   09 May 1998 14:13:58   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 10:08:54   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      * 'AKI00001 V006.000 19980325'
      ******************************************************************
        05  WU-RUECKNAHMEPREIS.
           10 RNPFONDID                     PIC S9(4) COMP.
           10 RNPAUSGABEBETRAG              PIC S9(5)V9(2) COMP-3.
           10 NIL-RNPAUSGABEBETRAG          PIC  X(01).
           10 RNPDATUMFONDSTAND             PIC  X(010).
           10 NIL-RNPDATUMFONDSTAND         PIC  X(01).
           10 RNPRUECKNAHMEBETR             PIC S9(5)V9(2) COMP-3.
           10 NIL-RNPRUECKNAHMEBETR         PIC  X(01).
           10 RNPDATUMWERTENTW              PIC  X(010).
           10 NIL-RNPDATUMWERTENTW          PIC  X(01).
           10 RNPWERTENTWICKLUNG            PIC S9(5)V9(2) COMP-3.
           10 NIL-RNPWERTENTWICKLUNG        PIC  X(01).
           10 LETZTEAENDERUNG               PIC  X(026).
           10 NIL-LETZTEAENDERUNG           PIC  X(01).
