      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drat0333.cpy  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\drat0333.cpv  $
      *
      *   Rev 2.2   22 May 2000 11:00:06   T13EX23
      *APA00090 - Doppelter Schluesselname
      *
      *   Rev 2.1   17 May 2000 14:49:42   T13EX23
      *APA00090 - AKZ Werte fuer PINZ-DATEN im Programm UPPA0049 
      *
      *   Rev 2.0   Mar 16 1999 09:57:28   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0333  KONSTANTEN FÜR ÄNDERUNGSKENNZEICHEN*
      *                           KTY-NR 188                         *
      *    -------------------------------------------------------   *
      *    ERSTELLT: GRAY                                            *
      *    DATUM:    17.05.2000                                      *
      ****************************************************************
      *    VERSION   2.1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00090!17.05.00! 2.1! NEUERSTELLUNG             ! GRAY     *
      * APA00090!19.05.00! 2.2! DOPPELTER SCHLUESSELNAME ! GRAY     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  FILLER.
           05 K-KNR-AKZ-PINZ-PVER-NEU  PIC S9(004) BINARY SYNC
                                                   VALUE  1.
           05 K-KNR-AKZ-PINZ-M-PVER-AKT
                                       PIC S9(004) BINARY SYNC
                                                   VALUE  2.
           05 K-KNR-AKZ-PINZ-O-PVER-AKT
                                       PIC S9(004) BINARY SYNC
                                                   VALUE  3.
           05 K-KNR-AKZ-PINZ-KEINE-AEND
                                       PIC S9(004) BINARY SYNC
                                                   VALUE  4.
