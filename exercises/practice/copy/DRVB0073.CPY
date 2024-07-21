      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0073.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0073.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:57:58   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Apr 11 1997 10:12:44   T13AE24
      *AFVB0347, Neue Bedingungen im AS-Club
      *
      *   Rev 1.0   Nov 04 1995 09:28:22   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0073                                      *
      *    --------------------------------------------------------   * 00002300
      *    SATZBESCHREIBUNG PMVB0146 / PMVB0147                       *
      *    ERSTELLEN DER LISTE:                                       *
      *    D-LEITER MIT STATUS A UND ADMIRALS-CLUB                    *
      *    --------------------------------------------------------   * 00002300
      *    ERSTELLT: P. SCHMIDT                                       *
      *    DATUM:    16.01.1995                                       *
      *****************************************************************
      *    VERSION   1    23.01.95 16:43:17
      ****************************************************************  00002800
      * AENDERUNGEN:                                                 *  00002900
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00003000
      * -------------------------------------------------------------*  00003100
      * AFVB0168!16.01.95! 01 ! NEU                       ! SCHMIDT  *  00004000
      * AFVB0312!25.11.96!    ! NEUE MITGLIEDER IM ADMIRAL!          *  00005000
      *         !        !    ! CLUB MIT * MARKIEREN      ! SCHMIDT  *  00006000
      * AFVB0347!04.04.97! 02 ! Produktions-Übergabe      ! Schmidt  *  00007000
      *         !        !    !                           !          *  00008000
      *                                                              *  00009000
      * ENDE-LOGBUCH                                                 *  00010000
      ****************************************************************  00020000
       01  DRVB0073-SATZ.
           05 GRUWE.
              10 GRUWE1            PIC 9(01).
              10 GRUWE2            PIC 9(03).
              10 GRUWE3            PIC 9(03).
           05 FILLER               PIC X(01).
           05 DIR                  PIC X(03).
           05 FILLER               PIC X(01).
           05 VBNR                 PIC X(07).
           05 FILLER               PIC X(01).
           05 NAME                 PIC X(20).
           05 FILLER               PIC X(01).
           05 VORNAME              PIC X(20).
           05 FILLER               PIC X(01).
           05 REGION               PIC X(10).
           05 FILLER               PIC X(01).
           05 STUFE                PIC X(04).
           05 FILLER               PIC X(01).
251196     05 ADM-CLUB-ERST        PIC X(01).
           05 FILLER               PIC X(01).
           05 ANZ-ED               PIC X(01).
           05 FILLER               PIC X(01).
           05 ADM-CLUB             PIC X(10).
           05 FILLER               PIC X(01).
           05 ADM-CLUB-AKT         PIC X(10).
           05 FILLER               PIC X(01).
           05 ADM-CLUB-ERNANNT     PIC X(01).
           05 FILLER               PIC X(01).
           05 SONOMA               PIC X(01).
           05 FILLER               PIC X(13).
      *
      ******** ENDE COPY-MEMBER (DRVB0073) *******************

