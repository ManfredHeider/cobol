      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0067.CPY  $
      *    $Revision:   2.9  $
      *
      *    $Log:   L:/mf/CPY/WUKI0067.CPv  $
      *
      *   Rev 2.9   Sep 18 2012 15:14:46   t13ex27
      *AKI00886: Host - neues Design Kundenaktualisierung-KVB
      *
      *   Rev 2.8   May 24 2004 11:43:48   t13ex27
      *DB-Änderung für Fix 61 PMKI0079 und WUKI's
      *
      *   Rev 2.7   Feb 16 2004 09:50:54   t13ex27
      *AKI00403 - DB-Änderung für Fix 57
      *
      *   Rev 2.6   Jun 11 2003 13:07:50   t13ex27
      *AKI00351 - DB-Änderung für Fix 41
      *
      *   Rev 2.5   25 Jul 2001 10:33:06   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-KUNDENINTVBBEZ.
           10 KVBVBPERSONENID               PIC  X(0013).
           10 KVBVBPERSONENROLLE            PIC  X(0001).
           10 KVBKIPERSONENID               PIC  X(0013).
           10 LETZTEAENDERUNG               PIC  X(026).
           10 NIL-LETZTEAENDERUNG           PIC  X(01).
           10 KVBLOESCHKZ                   PIC  X(0001).
           10 NIL-KVBLOESCHKZ               PIC  X(01).
           10 KVBKUNDEBETREUUNG             PIC  X(0001).
           10 NIL-KVBKUNDEBETREUUNG         PIC  X(01).
