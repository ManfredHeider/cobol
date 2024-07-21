      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0060.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/WDKI0060.CPv  $
      *
      *   Rev 2.3   May 24 2004 10:52:30   t13ex27
      *DB-Änderung für Fix 61 EA-Programme und WDKI's
      *
      *   Rev 2.2   Apr 15 2002 08:37:12   T13EX27
      *AKI00278 DB-Änderung für Fixpack 15
      *
      *   Rev 2.1   Apr 16 1999 10:06:38   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:32:16   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-AUSSENDIENSTMA-GESAMT.
         10 EA-NI-AUSSENDIENSTMA-TAB.
           20 EA-NI-AUSSENDIENSTMA-ELE PIC X OCCURS 15 TIMES.
         10 EA-NI-AUSSENDIENSTMA
                 REDEFINES EA-NI-AUSSENDIENSTMA-TAB.
           20 EA-NI-ADMAPERSONENID           PIC X.
           20 EA-NI-ADMAPERSONENROLLE        PIC X.
           20 EA-NI-ADMASTRUKTURSTUFE        PIC X.
           20 EA-NI-ADMAAUSTRITTSDATUM       PIC X.
           20 EA-NI-ADMADIREKTION            PIC X.
           20 EA-NI-ADMARUECKSTUFDATUM       PIC X.
           20 EA-NI-ADMAUMSTUFUNGSDAT        PIC X.
           20 EA-NI-ADMAINPROBEKZ            PIC X.
           20 EA-NI-ADMAPRAXISPROVSATZ       PIC X.
           20 EA-NI-ADMAARBSTATIONID         PIC X.
           20 EA-NI-ADMASPERRE               PIC X.
           20 EA-NI-ADMARECHNUNGSART         PIC X.
           20 EA-NI-ADMAPCZAHLUNGSART        PIC X.
           20 EA-NI-ADMAABAKUSDATUM          PIC X.
           20 EA-NI-ADMABESTELLABOKZ         PIC X.
         10 EA-DCL-AUSSENDIENSTMA.
           20 EA-ADMAPERSONENID              PIC X(13).
           20 EA-ADMAPERSONENROLLE           PIC X(1).
           20 EA-ADMASTRUKTURSTUFE           PIC ----------9.
           20 EA-ADMAAUSTRITTSDATUM          PIC X(010).
           20 EA-ADMADIREKTION               PIC -----9.
           20 EA-ADMARUECKSTUFDATUM          PIC X(010).
           20 EA-ADMAUMSTUFUNGSDAT           PIC X(010).
           20 EA-ADMAINPROBEKZ               PIC X(1).
           20 EA-ADMAPRAXISPROVSATZ          PIC ---9.99.
           20 EA-ADMAARBSTATIONID            PIC ----------9.
           20 EA-ADMASPERRE                  PIC X(1).
           20 EA-ADMARECHNUNGSART            PIC -----9.
           20 EA-ADMAPCZAHLUNGSART           PIC -----9.
           20 EA-ADMAABAKUSDATUM             PIC X(010).
           20 EA-ADMABESTELLABOKZ            PIC X(1).
      *****************************************************************
