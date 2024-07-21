      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0246.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/MF/CPY/DRKI0246.CPv  $
      *
      *   Rev 2.2   Jul 29 2002 16:26:26   T13ex48
      *FKI00626
      *Zaehlerfelder vergroessert
      *
      *   Rev 2.1   Mar 25 1999 12:45:34   T13EX41
      *AKI00007
      *- BERECH-ERST-KZ hinzugefügt
      *
      *   Rev 2.0   Mar 09 1999 15:58:00   T13EX41
      *NT-Umstellung
      *
      *   Rev 1.0   23 Jun 1998 13:50:58   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *    COPY fuer PC-Aktivitaeten-Statistik        Datei 3          *
160702*                                               Satzlaenge 050   *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                          !NAME     !VER-*
      *AUFTRAG !                                        !         !SION*
      *--------!----------------------------------------!---------!----*
160702*16.07.02!Logbuch eingefuegt, PC-Nr erweitert auf !SEBBACH  ! 01 *
      *FKI00626! 7 Stellen                              !         !    *
      *--------!----------------------------------------!---------!----*
       01  DT3SATZ.
           05 DATUM-AUSW-ANF   PIC X(010).
           05 DATUM-AUSW-END   PIC X(010).
160702*    05 PC-NR            PIC 9(005).
160702     05 PC-NR            PIC 9(007).
           05 VB-NR            PIC 9(007).
           05 KD-NR            PIC 9(009).
990319     05 BERECH-ERST-KZ   PIC 9(001).
990319*    05     FILLER       PIC X(039).
160702*    05     FILLER       PIC X(038).
160702     05     FILLER       PIC X(006).

