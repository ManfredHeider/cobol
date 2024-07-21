      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.06.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0403.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/WUKI0403.CPv  $
      *
      *   Rev 2.4   Mar 15 2017 14:31:10   t13ex27
      *AKI01492: AML-Änderungen zum RT 03/2017OZPs
      *
      *   Rev 2.3   16 Dec 2008 11:29:50   t13ex27
      *AKI00773: DB-Änderung für Rel. 4.44
      *
      *   Rev 2.2   Oct 17 2005 10:08:20   t13ex27
      *AKI00521: DB-Änderung für Fix 80 PMKI0079 und WUKI's
      *
      *   Rev 2.1   Jul 21 2005 10:28:18   t13ex27
      *AKI00505: DB-Änderung für Fix 77 EA-Programme
      *
      *   Rev 2.0   Jun 27 2005 16:22:22   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-WUKI0403.
           10 RZLVERTRAGSID                 PIC  X(0013).
           10 RZLZULAGENJAHR                PIC S9(4) COMP.
           10 RZLDATUMZULAGE                PIC  X(010).
           10 NIL-RZLDATUMZULAGE            PIC  X(01).
           10 RZLZULAGEBETRAG               PIC S9(9)V9(2) COMP-3.
           10 RZLZULAGEBETRAGWKZ            PIC  X(0001).
           10 RZLKUERZUNG-KTY               PIC S9(4) COMP.
           10 NIL-RZLKUERZUNG-KTY           PIC  X(01).
           10 RZLKUERZUNG-KNR               PIC S9(15)V COMP-3.
           10 NIL-RZLKUERZUNG-KNR           PIC  X(01).
           10 RZLANSPRUCH-KTY               PIC S9(4) COMP.
           10 NIL-RZLANSPRUCH-KTY           PIC  X(01).
           10 RZLANSPRUCH-KNR               PIC S9(15)V COMP-3.
           10 NIL-RZLANSPRUCH-KNR           PIC  X(01).
           10 LETZTEAENDERUNG               PIC  X(0026).

