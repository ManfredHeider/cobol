      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0224.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRAT0224.CPv  $
      *
      *   Rev 2.2   May 04 1999 15:53:16   T01JM2S
      *APA00012 - NEUE FUNKTION DRPA1210
      *
      *   Rev 2.1   May 04 1999 15:43:10   T01JM2S
      *APA00012 - NEUE FUNKTION DRPA1210
      *
      *   Rev 2.0   Apr 26 1999 15:10:20   T01JM2S
      *NT-Umstellung
      *
      *   Rev 1.0   03 Dec 1997 15:47:54   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0224  KONSTANTEN FÜR FUNKTIONSBEREICH    *
      *                           KTY-NR 153                         *
      *    -------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                           *
      *    DATUM:    03.02.1998                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !03.02.98! 01 ! NEUERSTELLUNG             ! WOLFF    *
      *         !        !    !                           !          *
      * AFPA0012!04.11.98! 02 ! NEU: FKT-BER-SST-MA-ABR   ! WOLFF    *
      *         !        !    !      FKT-BER-SST-DTA      !          *
      *         !        !    !                           !          *
      * APA00012!26.04.99!2.1 ! NEU: FKT-BER-PVW-DIALF    ! WOLFF    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  FILLER.
           05 K-KNR-FKT-BER-BDTA-ORIG  PIC S9(004) BINARY SYNC
                                                   VALUE  1.
           05 K-KNR-FKT-BER-BDTA-AEND  PIC S9(004) BINARY SYNC
                                                   VALUE  2.
           05 K-KNR-FKT-BER-MDTA-ORIG  PIC S9(004) BINARY SYNC
                                                   VALUE  3.
           05 K-KNR-FKT-BER-MDTA-AEND  PIC S9(004) BINARY SYNC
                                                   VALUE  4.
           05 K-KNR-FKT-BER-PEG-ERFOLG PIC S9(004) BINARY SYNC
                                                   VALUE  5.
           05 K-KNR-FKT-BER-PEG-FEHLER PIC S9(004) BINARY SYNC
                                                   VALUE  6.
           05 K-KNR-FKT-BER-PVW-ERFOLG PIC S9(004) BINARY SYNC
                                                   VALUE  7.
           05 K-KNR-FKT-BER-PVW-FEHIFS PIC S9(004) BINARY SYNC
                                                   VALUE  8.
           05 K-KNR-FKT-BER-PVW-FEHK   PIC S9(004) BINARY SYNC
                                                   VALUE  9.
           05 K-KNR-FKT-BER-PVW-ZUK    PIC S9(004) BINARY SYNC
                                                   VALUE  10.
           05 K-KNR-FKT-BER-SST-VNEUTR PIC S9(004) BINARY SYNC
                                                   VALUE  11.
           05 K-KNR-FKT-BER-SST-NEUTR  PIC S9(004) BINARY SYNC
                                                   VALUE  12.
           05 K-KNR-FKT-BER-SST-NNEUTR PIC S9(004) BINARY SYNC
                                                   VALUE  13.
           05 K-KNR-FKT-BER-SST-MA-ABR PIC S9(004) BINARY SYNC
                                                   VALUE  14.
           05 K-KNR-FKT-BER-SST-DTA    PIC S9(004) BINARY SYNC
                                                   VALUE  15.
           05 K-KNR-FKT-BER-PVW-DIALF  PIC S9(004) BINARY SYNC
                                                   VALUE  16.
