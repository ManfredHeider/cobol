      * ---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0244.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/MF/CPY/DRKI0244.CPv  $
      *
      *   Rev 2.2   Jul 29 2002 16:26:22   T13ex48
      *FKI00626
      *Zaehlerfelder vergroessert
      *
      *   Rev 2.1   Apr 19 1999 15:48:06   T13EX41
      *AKI00007
      *- keine Aenderung
      *
      *   Rev 2.0   Mar 09 1999 15:57:18   T13EX41
      *NT-Umstellung
      *
      *   Rev 1.0   23 Jun 1998 13:50:50   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *    COPY fuer PC-Aktivitaeten-Statistik        Datei 1          *
      *                                               Satzlaenge  082  *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                          !NAME     !VER-*
      *AUFTRAG !                                        !         !SION*
      *--------!----------------------------------------!---------!----*
160702*16.07.02!Logbuch eingefuegt, PC-Nr erweitert auf !SEBBACH  ! 01 *
      *FKI00626! 7 Stellen                              !         !    *
      *--------!----------------------------------------!---------!----*
       01  DT1SATZ.
           05 DATUM-AUSW-ANF   PIC X(010).
           05 DATUM-AUSW-END   PIC X(010).
160702*    05 PC-NR            PIC 9(005).
160702     05 PC-NR            PIC 9(007).
           05 PGM-NAME         PIC X(040).
160702*    05 ANZ-PGM-AUFR     PIC 9(002).
160702     05 ANZ-PGM-AUFR     PIC 9(005).
160702*    05 ANZ-RKS-ERL      PIC 9(003).
160702     05 ANZ-RKS-ERL      PIC 9(005).
160702*    05 ANZ-RKS-OFF      PIC 9(003).
160702     05 ANZ-RKS-OFF      PIC 9(005).
160702*    05     FILLER       PIC X(007).
