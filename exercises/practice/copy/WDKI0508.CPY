      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0508.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0508.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:13:08   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:32:58   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-PARTNERDOKUMENT-GESAMT.
         10 EA-NI-PARTNERDOKUMENT-TAB.
           20 EA-NI-PARTNERDOKUMENT-ELE PIC X
                  OCCURS 7 TIMES.
         10 EA-NI-PARTNERDOKUMENT
                 REDEFINES EA-NI-PARTNERDOKUMENT-TAB.
           20 EA-NI-PDOKID                   PIC X.
           20 EA-NI-INTERNERSERVICEID        PIC X.
           20 EA-NI-ERSTELLUHRZEIT           PIC X.
           20 EA-NI-DRUCKKZ                  PIC X.
           20 EA-NI-ANZAHLAUSDRUCKE          PIC X.
           20 EA-NI-PDTEXT                   PIC X.
           20 EA-NI-PDSTATISTIKDATUM         PIC X.
         10 EA-DCL-PARTNERDOKUMENT.
           20 EA-PDOKID                      PIC X(13).
           20 EA-INTERNERSERVICEID           PIC -----9    .
           20 EA-ERSTELLUHRZEIT              PIC X(08).
           20 EA-DRUCKKZ                     PIC X(01).
           20 EA-ANZAHLAUSDRUCKE             PIC -----9    .
           20 EA-PDTEXT.
              49 EA-PDTEXT-LEN               PIC -----9    .
              49 EA-PDTEXT-TEXT              PIC X(80).
           20 EA-PDSTATISTIKDATUM            PIC X(10).
      ******************************************************************
