      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drvb0072.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drvb0072.cpv  $
      *
      *   Rev 2.1   Jun 29 1999 12:34:40   T13AE42
      *AVB00448 - Einbau Pruefziffer
      *
      *   Rev 2.0   Feb 27 1999 15:57:58   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Apr 11 1997 10:11:54   T13AE24
      *AFVB0351, Neue Bedingungen im AS-Club
      *
      *   Rev 1.0   Nov 04 1995 09:26:30   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0072                                      *
      *    --------------------------------------------------------   * 00002300
      *    SATZBESCHREIBUNG DATEI VB......                            *
      *    ERSTELLEN DER LISTE:                                       *
      *    REPRÄSENTANTEN DER STUFE VB-P7 / EIGENGESCHÄFT             *
      *    --------------------------------------------------------   * 00002300
      *    ERSTELLT: P. SCHMIDT                                       *
      *    DATUM:    10.01.1995                                       *
      *****************************************************************
      *    VERSION   2    18.04.95 10:47:49
      ****************************************************************  00002800
      * AENDERUNGEN:                                                 *  00002900
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00003000
      * -------------------------------------------------------------*  00003100
      * AFVB0168!10.01.95! 01 ! NEU                       ! SCHMIDT  *  00004000
      * AFVB0204!05.04.95! 02 ! BQF ABGESCHAFFT           ! BERGHOFER*  00005000
      * AFVB0312!25.11.96!    ! NEUE FELDER AS-CLUB-ERST  !          *  00006000
      *         !        !    ! UND AS-CLUB-MONATE        ! SCHMIDT  *  00007000
      * AFVB0347!04.04.97! 03 ! Produktions-Übergabe      ! Schmidt  *
      * AFVB0448!29.06.99! 04 ! EINBAU PRUEFZIFFER        ! SCHMIDT  *
      *         !        !    !                           !          *  00008000
      *                                                              *  00009000
      * ENDE-LOGBUCH                                                 *  00010000
      ****************************************************************  00020000
       01  DRVB0072-SATZ.
           05 GRUWE.
              10 GRUWE1            PIC 9(01).
              10 GRUWE2            PIC 9(03).
              10 GRUWE3            PIC 9(03).
           05 FILLER               PIC X(01).
           05 REGION               PIC X(10).
           05 FILLER               PIC X(01).
           05 DIR                  PIC X(03).
           05 FILLER               PIC X(01).
           05 VBNR                 PIC X(07).
           05 FILLER               PIC X(01).
           05 NAME                 PIC X(20).
           05 FILLER               PIC X(01).
           05 VORNAME              PIC X(20).
           05 FILLER               PIC X(01).
           05 STUFE                PIC X(04).
           05 FILLER               PIC X(01).
           05 AS-CLUB-ERST         PIC X(01).
           05 FILLER               PIC X(01).
           05 AS-CLUB              PIC X(10).
           05 FILLER               PIC X(01).
           05 AS-CLUB-ERNANNT      PIC X(01).
           05 FILLER               PIC X(01).
           05 NEUGESCHAEFT         PIC ---------9,9.
           05 FILLER               PIC X(01).
           05 KUNDEN               PIC Z(05)9.
           05 FILLER               PIC X(01).
           05 KUNDEN2              PIC Z(05)9.
           05 FILLER               PIC X(01).
           05 BEITRAG              PIC --------9,99.
           05 FILLER               PIC X(01).
           05 EQF                  PIC ZZ9,99.
           05 FILLER               PIC X(01).
           05 SONSTIGES            PIC X(10).
           05 FILLER               PIC X(01).
251196     05 AS-CLUB-MONATE       PIC 999.
290699     05 FILLER               PIC X(01).
290699     05 PRUEFZIFFER          PIC X(01).
290699*    05 FILLER               PIC X(06).
290699     05 FILLER               PIC X(04).
      *
      ******** ENDE COPY-MEMBER (DRVB0072) *******************

