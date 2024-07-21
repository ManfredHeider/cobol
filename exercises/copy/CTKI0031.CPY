      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0031.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/CTKI0031.CPv  $
      *
      *   Rev 2.6   Feb 17 2011 14:26:40   t13ex27
      *AKI01022: DB-Änderungen für R. 5.28
      *
      *   Rev 2.5   19 Nov 2007 09:56:18   t13ex27
      *AKL00664: Allfinanz: Anpassung Kundenaktualisierung
      *
      *   Rev 2.4   18 Nov 2007 15:27:58   t13ex27
      *Reparatur
      *
      *   Rev 2.3   13 Nov 2007 12:23:14   t13ex27
      *AKL00664: Allfinanz: Anpassung Kundensuche
      *
      *   Rev 2.2   16 Oct 2007 14:26:24   t13ex27
      *AKL00664: Allfinanz: Anpassung Kundensuche
      *
      *   Rev 2.1   Jun 10 2002 09:50:10   T13EX27
      *AKI00289 -  DB-Änderun für neue 'VP-Analyse'
      *
      *   Rev 2.0   Feb 27 1999 12:49:34   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   09 May 1998 13:27:12   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 09:53:40   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AKI00001 V006.000 19980325'
      ******************************************************************
CCC005*    EXEC SQL DECLARE KUNDENINTERESSENT TABLE
CCC004*    ( KIPERSONENID                   CHAR(13) NOT NULL,
CCC004*      KIPERSONENROLLE                CHAR(1) NOT NULL,
CCC004*      KIKUNDENNUMMER                 INTEGER,
CCC004*      KIVKUNDEDUZENKZ                CHAR(1) NOT NULL,
CCC004*      KIKUNDENSTATUSID               CHAR(1) NOT NULL,
CCC004*      KIVSABERECHDATUM               DATE,
CCC004*      KIINFOWUNSCH                   CHAR(1) NOT NULL,
CCC004*      KIORTDERARBSTAETT              CHAR(3),
CCC004*      KIANZAHLBETREUER               SMALLINT,
CCC004*      KIKUNDENAKTEVER                CHAR(1) NOT NULL,
CCC004*      KIMANDANTNR                    INTEGER NOT NULL,
CCC004*      KIBDSGSPERRDATUM               DATE
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE KUNDENINTERESSENT           *
      ******************************************************************
       01  KUNDENINTERESSENT-GESAMT.
        05 NI-KUNDENINTERESSENT-TAB.
           10 NI-KUNDENINTERESSENT-ELE PIC S9(04) COMP OCCURS 12 TIMES.
        05 NI-KUNDENINTERESSENT REDEFINES NI-KUNDENINTERESSENT-TAB.
           10 NI-KIPERSONENID       PIC S9(4) COMP.
           10 NI-KIPERSONENROLLE    PIC S9(4) COMP.
           10 NI-KIKUNDENNUMMER     PIC S9(4) COMP.
           10 NI-KIVKUNDEDUZENKZ    PIC S9(4) COMP.
           10 NI-KIKUNDENSTATUSID   PIC S9(4) COMP.
           10 NI-KIVSABERECHDATUM   PIC S9(4) COMP.
           10 NI-KIINFOWUNSCH       PIC S9(4) COMP.
           10 NI-KIORTDERARBSTAETT  PIC S9(4) COMP.
           10 NI-KIANZAHLBETREUER   PIC S9(4) COMP.
           10 NI-KIKUNDENAKTEVER    PIC S9(4) COMP.
           10 NI-KIMANDANTNR        PIC S9(4) COMP.
           10 NI-KIBDSGSPERRDATUM   PIC S9(4) COMP.
        05 DCL-KUNDENINTERESSENT.
           10 KIPERSONENID         PIC X(13).
           10 KIPERSONENROLLE      PIC X(1).
           10 KIKUNDENNUMMER       PIC S9(9) USAGE COMP.
           10 KIVKUNDEDUZENKZ      PIC X(1).
           10 KIKUNDENSTATUSID     PIC X(1).
           10 KIVSABERECHDATUM     PIC X(10).
           10 KIINFOWUNSCH         PIC X(1).
           10 KIORTDERARBSTAETT    PIC X(3).
           10 KIANZAHLBETREUER     PIC S9(4) USAGE COMP.
           10 KIKUNDENAKTEVER      PIC X(1).
           10 KIMANDANTNR          PIC S9(9) USAGE COMP.
           10 KIBDSGSPERRDATUM     PIC X(10).
      ************************** ENDE-DCLGEN *************************
