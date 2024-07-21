      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drki0545.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drki0545.cpv  $
      *
      *   Rev 2.1   Sep 17 1999 08:24:24   T13EX19
      *aki00048 - neuerstellung. Copy-strecke
      *                  für Prodtokol aus VBSST
      *
      *   Rev 2.0   Feb 27 1999 12:57:50   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   07 Aug 1998 14:46:50   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0545                                      *
      *    --------------------------------------------------------   *
      *    COPY-STRECKE FUER DIE PROTOKOLLDATEI AUS DER PMKI0092
      *    --------------------------------------------------------   *
      *    LAENGE = 161
      *    --------------------------------------------------------   *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *AKI00048 !06.09.00!    !neuerstellung              !zgur-vujic*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************

           05  pr-vb-personenid        PIC x(13).
           05  pr-vb-personenid-disp   PIC x(26).
           05  pr-vb-nummer            PIC 9(07).
           05  pr-direktion-neu        PIC 9(04).
           05  pr-direktion-alt        PIC 9(04).
           05  pr-ist-direktion-neu    PIC x(01).
      ***     'J' wenn direktion neu ist sonst SPACE
           05  pr-betreuer-neu         PIC x(13).
           05  pr-betreuer-neu-disp    PIC x(26).
           05  pr-betreuer-alt         PIC x(13).
           05  pr-betreuer-alt-disp    PIC x(26).
           05  pr-pc-nr-neu            PIC 9(09).
           05  pr-pc-nr-alt            PIC 9(09).
           05  pr-datum                PIC x(10).
      ***     datum der verarbeitung

