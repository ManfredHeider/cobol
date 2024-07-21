      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0614.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/MF/CPY/WDKI0614.CPv  $
      *
      *   Rev 2.2   May 27 2003 11:39:26   t13ex27
      *AKI00351 - DB-Änderung für Fix 41
      *
      *   Rev 2.1   Apr 16 1999 10:13:44   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:33:06   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-PROGRAMMUPDATE-GESAMT.
         10 EA-NI-PROGRAMMUPDATE-TAB.
           20 EA-NI-PROGRAMMUPDATE-ELE PIC X OCCURS 17 TIMES.
         10 EA-NI-PROGRAMMUPDATE
                 REDEFINES EA-NI-PROGRAMMUPDATE-TAB.
           20 EA-NI-PU-PC-NR                 PIC X.
           20 EA-NI-NUMMERFREIGABE           PIC X.
           20 EA-NI-ARCHIV                   PIC X.
           20 EA-NI-BESCHREIBUNG             PIC X.
           20 EA-NI-MEDIUM                   PIC X.
           20 EA-NI-ARCHIVBYTES              PIC X.
           20 EA-NI-DATUMFREI                PIC X.
           20 EA-NI-DATUMBIS                 PIC X.
           20 EA-NI-REPSPERRE                PIC X.
           20 EA-NI-FLGSPERRE                PIC X.
           20 EA-NI-FREIGABEART              PIC X.
           20 EA-NI-LAUFENDER-STATUS         PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
           20 EA-NI-AKTBYTECOUNT             PIC X.
           20 EA-NI-REBOOTFLAG               PIC X.
           20 EA-NI-AKTEINARBTIMESTAMP       PIC X.
           20 EA-NI-TMPFILENAME              PIC X.
         10 EA-DCL-PROGRAMMUPDATE.
           20 EA-PU-PC-NR                    PIC ----------9.
           20 EA-NUMMERFREIGABE              PIC ----------9.
           20 EA-ARCHIV                      PIC X(12).
           20 EA-BESCHREIBUNG.
              49 EA-BESCHREIBUNG-LEN         PIC -----9.
              49 EA-BESCHREIBUNG-TEXT        PIC X(60).
           20 EA-MEDIUM                      PIC X(8).
           20 EA-ARCHIVBYTES                 PIC ----------9.
           20 EA-DATUMFREI                   PIC X(010).
           20 EA-DATUMBIS                    PIC X(010).
           20 EA-REPSPERRE                   PIC X(1).
           20 EA-FLGSPERRE                   PIC X(1).
           20 EA-FREIGABEART                 PIC X(1).
           20 EA-LAUFENDER-STATUS            PIC -----9.
           20 EA-LETZTEAENDERUNG             PIC X(026).
           20 EA-AKTBYTECOUNT                PIC ----------9.
           20 EA-REBOOTFLAG                  PIC X(1).
           20 EA-AKTEINARBTIMESTAMP          PIC X(026).
           20 EA-TMPFILENAME.
              49 EA-TMPFILENAME-LEN          PIC -----9.
              49 EA-TMPFILENAME-TEXT         PIC X(254).
      *****************************************************************
