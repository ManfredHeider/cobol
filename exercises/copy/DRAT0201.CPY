      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0201.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0201.CPv  $
      *
      *   Rev 2.0   May 28 1999 18:23:42   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   05 Sep 1997 09:22:46   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0201  KONSTANTEN FÜR ÄNDERUNGSKENNZEICHEN*
      *                           KTY-NR 150                         *
      *    -------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                           *
      *    DATUM:    03.12.1997                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !03.12.97! 01 ! NEUERSTELLUNG             ! WOLFF    *
      * APA00012!14.11.98! 02 ! NEU K-KNR-AKZ-PNAP-EINTRAG! MUND     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  FILLER.
           05 K-KNR-AKZ-NEUERSTELL     PIC S9(004) BINARY SYNC
                                                   VALUE  1.
           05 K-KNR-AKZ-KEINE-AEND     PIC S9(004) BINARY SYNC
                                                   VALUE  2.
           05 K-KNR-AKZ-AEND-MHIST     PIC S9(004) BINARY SYNC
                                                   VALUE  3.
           05 K-KNR-AKZ-AEND-OHIST     PIC S9(004) BINARY SYNC
                                                   VALUE  4.
           05 K-KNR-AKZ-PNAP-EINTRAG   PIC S9(004) BINARY SYNC
                                                   VALUE  5.
