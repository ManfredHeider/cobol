      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0081.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAT0081.CPv  $
      *
      *   Rev 2.1   May 03 2010 08:02:48   t13ae13
      *AVB01031: ED- flexible Provisionszuordnung 
      *
      *   Rev 2.0   Feb 27 1999 11:57:16   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   23 Jun 1998 09:56:02   T13AE26
      *AVB00391 - Anpassung BD-Struktur
      *
      *   Rev 1.0   Nov 04 1995 08:49:22   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0081   KNR-KONST. FUER EDD-URSACHE        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SCHÖLLHAMMER                                     *
      *    DATUM:    22.06.1994                                       *
      *****************************************************************
      *    VERSION   1    18.07.94 17:06:02
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFVB0117!22.06.94!    ! NEU                       ! SCHÖLLH. *
      *         !18.07.94! 01 ! PROD.UEBERGABE            ! SCHÖLLH. *
      * AFVB0371!20.08.97!    ! K-KNR-BD-STRUKTUR neu u.  !          *
      *         !        ! 02 ! Spalten >72 geloescht     ! T. Moll  *
      * AVB01031!04.02.10!2.1 ! K-KNR-ED-WEICHE neu       ! S. Walb  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *   SCHLÜSSELWERTE: URSACHE FÜR EDD-EINTRAG  (KTY 46)
      *
       01  FILLER.
           05  K-KNR-D-HERAUSGEWACHSEN       PIC S9(004) COMP VALUE 001.
           05  K-KNR-STRUKTURDIR             PIC S9(004) COMP VALUE 002.
           05  K-KNR-ED-WIEDERAUFGELEBT      PIC S9(004) COMP VALUE 003.
           05  K-KNR-ED-EINGEFUEGT           PIC S9(004) COMP VALUE 004.
           05  K-KNR-ED-GEKUENDIGT           PIC S9(004) COMP VALUE 005.
200897     05  K-KNR-BD-STRUKTUR             PIC S9(004) COMP VALUE 006.
040210     05  K-KNR-ED-WEICHE               PIC S9(004) COMP VALUE 007.
           05  K-KNR-KORR-D-HERAUSGEWACHSEN  PIC S9(004) COMP VALUE 091.
           05  K-KNR-EDD-URSACHE-ALT         PIC S9(004) COMP VALUE 999.
      *
      ***** ENDE COPY-MEMBER DRAT0081  KNR-KONST. EDD-URSACHE     ******
