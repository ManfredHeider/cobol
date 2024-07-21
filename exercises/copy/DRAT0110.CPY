      *---------------------------------------------------------------*
      *    STANDARD FÜR ALLE OBJEKTE UNTER PVCS
      *            DATUM   :  NOVEMBER 1995
      *            KOPF AUS:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    COPYRIGHT 1995 ATLAS DIENSTLEISTUNGS GMBH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0110.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:\MF\CPY\DRAT0110.CPv  $
      *
      *   Rev 2.3   19 Dec 2000 14:55:40   T13EX43
      *APA00111 : Wiederholung NTPUT (identisch mit Vorversion)
      *
      *   Rev 2.2   14 Nov 2000 09:51:16   T13EX43
      *APA00112 : DIT-PEG umsetzen und einführen (Nachtrag)
      *
      *   Rev 2.1   02 Nov 2000 13:26:50   T13EX43
      *APA00112 : DIT-PEG umsetzen und einführen
      *
      *   Rev 2.0   Feb 27 1999 11:57:22   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Mar 07 1997 09:17:34   P13PVCS
      *Neuerstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0110  KONSTANTEN FÜR ZAHLWEISE, KTY = 100*
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Priggen                                         *
      *    DATUM:    22.08.1996                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !22.08.96! 01 ! NEUERSTELLUNG             ! PRIGGEN  *
      * APA00112!24.10.00! 2.1! ZAHLW-ZWEI-MONAT NEU (DIT)! KLEEMEIER*
      * APA00112!10.11.00! 2.2! ZAHLW-VIER-MONAT NEU (DIT)! KLEEMEIER*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  FILLER.
           05 K-KNR-ZAHLW-UNGUELTIG  PIC S9(004) BINARY SYNC
                                                 VALUE  0.
           05 K-KNR-ZAHLW-JAHR       PIC S9(004) BINARY SYNC
                                                 VALUE  1.
           05 K-KNR-ZAHLW-HALB-JAHR  PIC S9(004) BINARY SYNC
                                                 VALUE  2.
101100     05 K-KNR-ZAHLW-VIER-MONAT PIC S9(004) BINARY SYNC
101100                                           VALUE  3.
           05 K-KNR-ZAHLW-QUARTAL    PIC S9(004) BINARY SYNC
                                                 VALUE  4.
241000     05 K-KNR-ZAHLW-ZWEI-MONAT PIC S9(004) BINARY SYNC
241000                                           VALUE  6.
           05 K-KNR-ZAHLW-MONAT      PIC S9(004) BINARY SYNC
                                                 VALUE  12.
           05 K-KNR-ZAHLW-EINMAL     PIC S9(004) BINARY SYNC
                                                 VALUE  99.
      *
