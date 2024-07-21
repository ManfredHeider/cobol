      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0233.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0233.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:38   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   07 Aug 1998 11:27:18   T13AE34
      *AAN00167 - Vorvertragsnummer
      *
      *   Rev 1.0   03 Dec 1997 15:48:32   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0233  KONSTANTEN FÜR ZAHLWEISE ANTRAG    *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: HEIDER                                          *
      *    DATUM:    12.02.1998                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00167!12.02.98! 01 ! NEUERSTELLUNG             ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  FILLER.
           05 K-ZAHLW-AN-JAHR       PIC S9(004) BINARY SYNC
                                                 VALUE  1.
           05 K-ZAHLW-AN-HALB-JAHR  PIC S9(004) BINARY SYNC
                                                 VALUE  2.
           05 K-ZAHLW-AN-QUARTAL    PIC S9(004) BINARY SYNC
                                                 VALUE  4.
           05 K-ZAHLW-AN-MONAT      PIC S9(004) BINARY SYNC
                                                 VALUE  6.
           05 K-ZAHLW-AN-EINMAL     PIC S9(004) BINARY SYNC
                                                 VALUE  8.
           05 K-ZAHLW-AN-DEPOT      PIC S9(004) BINARY SYNC
                                                 VALUE  9.
