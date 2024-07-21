      *****************************************************************
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0002.CPY  $
      *    $Revision:   2.9  $
      *
      *    $Log:   L:/mf/CPY/WUKI0002.CPv  $
      *
      *   Rev 2.9   Feb 16 2004 09:50:16   t13ex27
      *AKI00403 - DB-Änderung für Fix 57
      *
      *   Rev 2.8   Sep 08 2003 16:27:14   t13ex27
      *AKI00268 - Neues Adressverfahren
      *
      *   Rev 2.7   Jun 11 2003 13:05:22   t13ex27
      *AKI00351 - DB-Änderung für Fix 41
      *
      *   Rev 2.6   23 Oct 2001 16:10:36   t13ex48
      *AKI00216
      *Aufbau Test-DB(RP und XDB)
      *
      *   Rev 2.5   25 Jul 2001 10:32:54   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-ADRESSE.
           10 ADRESSEID                   PIC X(13).
           10 ADRESSEBDSLDID              PIC X(3).
           10 NIL-ADRESSEBDSLDID          PIC X(01).
           10 ADRESSELANDID               PIC X(3).
           10 ADRESSEORT                  PIC X(30).
           10 NIL-ADRESSEORT              PIC X(01).
           10 ADRESSEPLZ                  PIC X(15).
           10 NIL-ADRESSEPLZ              PIC X(01).
           10 ADRESSESTRHAUSNUM           PIC X(45).
           10 NIL-ADRESSESTRHAUSNUM       PIC X(01).
           10 ADRESSEISTVTADR             PIC X(1).
           10 LETZTEAENDERUNG             PIC X(026).
           10 NIL-LETZTEAENDERUNG         PIC X(01).
           10 ADRESSESUCHSTRASSE          PIC X(45).
           10 NIL-ADRESSESUCHSTRASSE      PIC X(01).
           10 ADRESSEGEPRUEFT             PIC X(1).
           10 NIL-ADRESSEGEPRUEFT         PIC X(01).
           10 ADRESSEORTSTEIL             PIC X(30).
           10 ADRESSEPOSTPRUEF            PIC X(25).
           10 ADRESSEPOSTPRUEFID          PIC S9(4) COMP.
      *****************************************************************
