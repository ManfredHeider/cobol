      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0266.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0266.CPv  $
      *
      *   Rev 2.0   26 Jul 1999 10:49:26   T01JM2S
      *APA00012 - NT-Umstellung
      *
      *   Rev 1.0   01 Apr 1998 08:38:52   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0266  KONSTANTEN FÜR BEREINIGUNG EVS     *
      *                           KTY-NR 170                         *
      *    -------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                           *
      *    DATUM:    16.06.1998                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !16.06.98! 01 ! NEUERSTELLUNG             ! WOLFF    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  FILLER.
           05 K-KNR-EVS-BER-PT-BEENDET PIC S9(004) BINARY SYNC
                                                   VALUE  1.
           05 K-KNR-EVS-BER-PT-GESAMT  PIC S9(004) BINARY SYNC
                                                   VALUE  2.
           05 K-KNR-EVS-BER-PT-GLEICH  PIC S9(004) BINARY SYNC
                                                   VALUE  3.
           05 K-KNR-EVS-BER-PT-REST    PIC S9(004) BINARY SYNC
                                                   VALUE  4.
