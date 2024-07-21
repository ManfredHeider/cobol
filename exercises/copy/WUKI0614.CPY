      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0614.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/WUKI0614.CPv  $
      *
      *   Rev 2.6   Jun 11 2003 13:14:20   t13ex27
      *AKI00351 - DB-Änderung für Fix 41
      *
      *   Rev 2.5   25 Jul 2001 10:33:40   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-PROGRAMMUPDATE.
           10 PU-PC-NR                    PIC S9(9) COMP.
           10 NUMMERFREIGABE              PIC S9(9) COMP.
           10 ARCHIV                      PIC X(12).
           10 BESCHREIBUNG.
              49 BESCHREIBUNG-LEN              PIC S9(4) COMP.
              49 BESCHREIBUNG-TEXT             PIC X(60).
           10 NIL-BESCHREIBUNG            PIC X(01).
           10 MEDIUM                      PIC X(8).
           10 NIL-MEDIUM                  PIC X(01).
           10 ARCHIVBYTES                 PIC S9(9) COMP.
           10 NIL-ARCHIVBYTES             PIC X(01).
           10 DATUMFREI                   PIC X(010).
           10 NIL-DATUMFREI               PIC X(01).
           10 DATUMBIS                    PIC X(010).
           10 NIL-DATUMBIS                PIC X(01).
           10 REPSPERRE                   PIC X(1).
           10 FLGSPERRE                   PIC X(1).
           10 FREIGABEART                 PIC X(1).
           10 LAUFENDER-STATUS            PIC S9(4) COMP.
           10 NIL-LAUFENDER-STATUS        PIC X(01).
           10 LETZTEAENDERUNG             PIC X(026).
           10 NIL-LETZTEAENDERUNG         PIC X(01).
           10 AKTBYTECOUNT                PIC S9(9) COMP.
           10 NIL-AKTBYTECOUNT            PIC X(01).
           10 REBOOTFLAG                  PIC X(1).
           10 AKTEINARBTIMESTAMP          PIC X(026).
           10 NIL-AKTEINARBTIMESTAMP      PIC X(01).
           10 TMPFILENAME.
              49 TMPFILENAME-LEN               PIC S9(4) COMP.
              49 TMPFILENAME-TEXT              PIC X(254).
           10 NIL-TMPFILENAME             PIC X(01).
      *****************************************************************
