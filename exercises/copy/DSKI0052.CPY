      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DSKI0052.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/DSKI0052.CPv  $
      *
      *   Rev 2.1   Mar 01 2004 10:37:56   t13ex27
      *AKI00114 - Korrigierte TITEL
      *
      *   Rev 2.0   Feb 27 1999 12:59:00   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   29 Oct 1998 17:52:10   T13EX19
      *FKI00010 - Korrektur
      *
      *   Rev 1.1   12 Aug 1998 16:56:02   T13EX19
      *VB-SCHNITTSTELLE
      *
      *   Rev 1.0   30 Jun 1998 10:23:18   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DSKI0052                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS TITEL Supporttabelle                  *
      *    --------------------------------------------------------   *
      *    ERSTELLT: ZGUR-VUJIC                                       *
      *    DATUM:    30.06.1998                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00001!30.06.98! 01 ! NEUERSTELLUNG             !ZGUR-VUJIC*
      *         !27.10.98! 02 ! 99  wird -1               !ZGUR-VUJIC*
      * AKI00114!16.02.04!2.1 ! Titelbereinigung im       !I. Frenzel*
      *         !        !    ! VB-System                 !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
          05 K-dr                       PIC  s9(004) binary value 1.
          05 K-prof                     PIC  s9(004) binary value 2.
          05 K-prof-dr                  PIC  s9(004) binary value 3.
          05 K-dr-hc                    PIC  s9(004) binary value 4.
          05 K-magister                 PIC  s9(004) binary value 5.
          05 K-diplom-kaufmann          PIC  s9(004) binary value 6.
          05 K-ingenieur-mag            PIC  s9(004) binary value 7.
          05 K-ingenieur                PIC  s9(004) binary value 8.
          05 K-di                       PIC  s9(004) binary value 9.
          05 K-diplom-jur               PIC  s9(004) binary value 10.
          05 K-dr-ing                   PIC  s9(004) binary value 11.
          05 K-dipl-ing                 PIC  s9(004) binary value 12.
          05 K-dr-med                   PIC  s9(004) binary value 13.
          05 K-dr-jur                   PIC  s9(004) binary value 14.
160204    05 K-BAKK                     PIC  S9(004) BINARY VALUE 15.
160204    05 K-DR-DR                    PIC  S9(004) BINARY VALUE 16.
271098*   05 K-titel-unbekannt          PIC  s9(004) binary value 99.
271098    05 K-titel-unbekannt          PIC  s9(004) binary value -1.
      *
      ***** ENDE COPY-MEMBER DSKI0052 *****************************
