      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DSKI0321.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DSKI0321.CPv  $
      *
      *   Rev 2.1   Mar 10 1999 12:07:06   I13PC48
      *Vorbereitung NTPut
      *
      *   Rev 2.0   Mar 03 1999 13:20:42   I13PC48
      *Version 2.0 nach Umstellung auf NT
      *
      *   Rev 1.0   30 Jun 1998 10:24:48   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *                                                              *
      *    COPY-MEMBER: DSKI0321                                     *
      *                                                              *
      *    Konstanten für die Ausprägungen der Supporttabelle        *
      *                                                              *
      *    KFZ-Leistungsartid - TCKI0321                             *
      *                                                              *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * AKI00001!18.08.98!01.01! Neuerstellung            !K-H Müller*
      * --------!--------!-----!--------------------------!----------*
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************

       01  FILLER.
           05 K-KFZ-LEISTUNG-IN-KW          PIC S9(004) BINARY VALUE  1.
           05 K-KFZ-LEISTUNG-IN-PS          PIC S9(004) BINARY VALUE  2.
           05 K-KFZ-LEISTUNG-IN-CCM         PIC S9(004) BINARY VALUE  3.
           05 K-KFZ-LEISTUNG-IN-T           PIC S9(004) BINARY VALUE  4.
           05 K-KFZ-LEISTUNG-IN-PLAETZEN    PIC S9(004) BINARY VALUE  5.
           05 K-KFZ-LEISTUNG-UNBEKANNT      PIC S9(004) BINARY VALUE -2.

      ******************************************************************
      *          E n d e    Copy-Buch   DSKI0321                       *
      ******************************************************************
