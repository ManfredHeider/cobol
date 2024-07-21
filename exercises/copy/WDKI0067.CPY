      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0067.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/WDKI0067.CPv  $
      *
      *   Rev 2.4   May 24 2004 10:52:42   t13ex27
      *DB-Änderung für Fix 61 EA-Programme und WDKI's
      *
      *   Rev 2.3   Feb 16 2004 09:49:10   t13ex27
      *AKI00403 - DB-Änderung für Fix 57
      *
      *   Rev 2.2   22 Oct 2001 14:48:46   t13ex27
      *AKI00240 - DB-Änderung Fixpack 6 
      *
      *   Rev 2.1   Apr 16 1999 10:06:44   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:32:20   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-KUNDENINTVBBEZ-GESAMT.
         10 EA-NI-KUNDENINTVBBEZ-TAB.
           20 EA-NI-KUNDENINTVBBEZ-ELE PIC X OCCURS 10 TIMES.
         10 EA-NI-KUNDENINTVBBEZ
                 REDEFINES EA-NI-KUNDENINTVBBEZ-TAB.
           20 EA-NI-KVBVBPERSONENID          PIC X.
           20 EA-NI-KVBVBPERSONENROLLE       PIC X.
           20 EA-NI-KVBKIPERSONENID          PIC X.
           20 EA-NI-KVBKIPERSONENROLLE       PIC X.
           20 EA-NI-KVBVERMITTLERID          PIC X.
           20 EA-NI-KVBVERMITTLERROLLE       PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
           20 EA-NI-KVBLOESCHKZ              PIC X.
           20 EA-NI-KVBKUNDEDUZENKZ          PIC X.
           20 EA-NI-KVBKUNDEBETREUUNG        PIC X.
         10 EA-DCL-KUNDENINTVBBEZ.
           20 EA-KVBVBPERSONENID             PIC X(13).
           20 EA-KVBVBPERSONENROLLE          PIC X(1).
           20 EA-KVBKIPERSONENID             PIC X(13).
           20 EA-KVBKIPERSONENROLLE          PIC X(1).
           20 EA-KVBVERMITTLERID             PIC X(13).
           20 EA-KVBVERMITTLERROLLE          PIC X(1).
           20 EA-LETZTEAENDERUNG             PIC X(026).
           20 EA-KVBLOESCHKZ                 PIC X(1).
           20 EA-KVBKUNDEDUZENKZ             PIC X(1).
           20 EA-KVBKUNDEBETREUUNG           PIC X(1).
      *****************************************************************
