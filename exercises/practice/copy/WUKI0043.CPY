      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.09.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0043.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WUKI0043.CPv  $
      *
      *   Rev 2.1   Sep 02 2009 16:05:34   t13ex27
      *AKI00843: DB-Änderungen für Rel. 4.60-Kundenaktualisierung
      *
      *   Rev 2.0   Sep 02 2009 15:55:18   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-TCKI0043.
           10 PERSONENID                  PIC  X(13).
           10 PERSONANREDEID              PIC  S9(4) COMP.
           10 NIL-PERSONANREDEID          PIC X(01).
           10 PERSONANZNAME               PIC  X(30).
           10 NIL-PERSONANZNAME           PIC X(01).
           10 PERSONANZVORNAME            PIC  X(30).
           10 NIL-PERSONANZVORNAME        PIC X(01).
           10 PERSONSUCHNAME              PIC  X(30).
           10 NIL-PERSONSUCHNAME          PIC X(01).
           10 PERSONSUCHVORNAME           PIC  X(30).
           10 NIL-PERSONSUCHVORNAME       PIC X(01).
           10 PERSONGEBGRUENDDAT          PIC  X(010).
           10 NIL-PERSONGEBGRUENDDAT      PIC X(01).
           10 LETZTEAENDERUNG             PIC  X(026).
           10 NIL-LETZTEAENDERUNG         PIC X(01).
