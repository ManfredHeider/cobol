      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0245.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/MF/CPY/DRKI0245.CPv  $
      *
      *   Rev 2.2   Jul 29 2002 16:26:24   T13ex48
      *FKI00626
      *Zaehlerfelder vergroessert
      *
      *   Rev 2.1   Mar 25 1999 12:44:24   T13EX41
      *AKI00007
      *- UPGM-NAME hinzugefügt
      *
      *   Rev 2.0   Mar 09 1999 15:57:54   T13EX41
      *NT-Umstellung
      *
      *   Rev 1.0   23 Jun 1998 13:50:54   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *    COPY fuer PC-Aktivitaeten-Statistik        Datei 2          *
160702*                                               Satzlaenge 140   *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                          !NAME     !VER-*
      *AUFTRAG !                                        !         !SION*
      *--------!----------------------------------------!---------!----*
160702*16.07.02!Logbuch eingefuegt, PC-Nr erweitert auf !SEBBACH  ! 01 *
      *FKI00626! 7 Stellen, ANZ-xx-Felder vergroessert  !         !    *
      *--------!----------------------------------------!---------!----*
       01  DT2SATZ.
           05 DATUM-AUSW-ANF   PIC X(010).
           05 DATUM-AUSW-END   PIC X(010).
160702*    05 PC-NR            PIC 9(005).
160702     05 PC-NR            PIC 9(007).
           05 VB-NR            PIC 9(007).
           05 KD-NR            PIC 9(009).
           05 PGM-NAME         PIC X(040).
990319     05 UPGM-NAME        PIC X(040).
           05 D-KZ             PIC X(001).
160702*    05 ANZ-PGM-AUFR     PIC 9(002).
160702     05 ANZ-PGM-AUFR     PIC 9(005).
160702*    05 ANZ-AUSD         PIC 9(002).
160702     05 ANZ-AUSD         PIC 9(005).
           05 SA               PIC X(002).
160702*    05     FILLER       PIC X(002).
160702     05     FILLER       PIC X(004).

