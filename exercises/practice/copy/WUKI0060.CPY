      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0060.CPY  $
      *    $Revision:   2.8  $
      *
      *    $Log:   L:/mf/CPY/WUKI0060.CPv  $
      *
      *   Rev 2.8   May 12 2015 09:19:38   t13ex73
      *AKI01360: §34f VB-SST Umsetzung
      *
      *   Rev 2.7   May 24 2004 11:43:38   t13ex27
      *DB-Änderung für Fix 61 PMKI0079 und WUKI's
      *
      *   Rev 2.6   Jun 11 2003 13:07:40   t13ex27
      *AKI00351 - DB-Änderung für Fix 41
      *
      *   Rev 2.5   25 Jul 2001 10:33:04   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-AUSSENDIENSTMA.
           10 ADMAPERSONENID              PIC X(13).
           10 ADMAPERSONENROLLE           PIC X(1).
           10 ADMASTRUKTURSTUFE           PIC S9(9) COMP.
           10 NIL-ADMASTRUKTURSTUFE       PIC X(01).
           10 ADMAAUSTRITTSDATUM          PIC X(010).
           10 NIL-ADMAAUSTRITTSDATUM      PIC X(01).
           10 ADMADIREKTION               PIC S9(4) COMP.
           10 NIL-ADMADIREKTION           PIC X(01).
           10 ADMARUECKSTUFDATUM          PIC X(010).
           10 NIL-ADMARUECKSTUFDATUM      PIC X(01).
           10 ADMAUMSTUFUNGSDAT           PIC X(010).
           10 NIL-ADMAUMSTUFUNGSDAT       PIC X(01).
           10 ADMAINPROBEKZ               PIC X(1).
           10 ADMAPRAXISPROVSATZ          PIC S9(3)V9(2) COMP-3.
           10 NIL-ADMAPRAXISPROVSATZ      PIC X(01).
           10 ADMAARBSTATIONID            PIC S9(9) COMP.
           10 NIL-ADMAARBSTATIONID        PIC X(01).
           10 ADMASPERRE                  PIC X(1).
           10 ADMARECHNUNGSART            PIC S9(4) COMP.
           10 NIL-ADMARECHNUNGSART        PIC X(01).
           10 ADMAPCZAHLUNGSART           PIC S9(4) COMP.
           10 NIL-ADMAPCZAHLUNGSART       PIC X(01).
           10 ADMAABAKUSDATUM             PIC X(010).
           10 NIL-ADMAABAKUSDATUM         PIC X(01).
           10 ADMABESTELLABOKZ            PIC X(1).
           10 NIL-ADMABESTELLABOKZ        PIC X(01).
           10 ADMA34FDAT                  PIC X(10).
           10 NIL-ADMA34FDAT              PIC X(01).
      *****************************************************************

