      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0323.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:\MF\CPY\DRAT0323.CPv  $
      *
      *   Rev 2.5   19 Dec 2000 14:56:10   T13EX43
      *APA00111 : Wiederholung NTPUT (identisch mit Vorversion)
      *
      *   Rev 2.4   13 Nov 2000 13:17:56   T13EX43
      *APA00112 : DIT-PEG umsetzen und einführen (Nachtrag)
      *
      *   Rev 2.3   02 Nov 2000 13:30:14   T13EX43
      *APA00112 : DIT-PEG umsetzen und einführen
      *
      *   Rev 2.2   May 04 2000 11:58:38   T13EX23
      *APA00067 - Neue Fremdsysteme definiert
      *
      *   Rev 2.1   Apr 13 2000 14:56:20   T13EX21
      *APA00067 - Neuerstellung
      *
      *   Rev 2.0   Mar 16 1999 09:57:26   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0323  KONSTANTEN FÜR KTY = 184           *
      *                           (FREMDSYSTEM)                      *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    12.04.2000                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME    *
      * -------------------------------------------------------------*
      * APA00067!12.04.00! 01  ! NEUERSTELLUNG             ! STORCH  *
      * APA00067!19.04.00! 02  ! ZWEI NEUE FREMD-SYSTEME   ! GRAY    *
      * APA00112!31.10.00! 2.3 ! DREI NEUE FREMD-SYSTEME   !KLEEMEIER*
      * APA00112!10.11.00! 2.4 ! EIN NEUES FREMD-SYSTEM    !KLEEMEIER*
      *         !        !     !                           !         *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  FILLER.
           05 K-KNR-FREMDSYS-PG         PIC S9(004) BINARY VALUE 1.
101100     05 K-KNR-FREMDSYS-DAD        PIC S9(004) BINARY VALUE 2.
190400     05 K-KNR-FREMDSYS-OP         PIC S9(004) BINARY VALUE 3.
190400     05 K-KNR-FREMDSYS-OPD        PIC S9(004) BINARY VALUE 4.
311000     05 K-KNR-FREMDSYS-AN         PIC S9(004) BINARY VALUE 5.
311000     05 K-KNR-FREMDSYS-AND        PIC S9(004) BINARY VALUE 6.
311000     05 K-KNR-FREMDSYS-KI         PIC S9(004) BINARY VALUE 7.
      *
