      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPAT0066.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DPAT0066.CPv  $
      *
      *   Rev 2.1   Jan 25 2017 10:11:28   t13ex27
      *AAT00055: Überarbeitung Fehlerverfahren
      *
      *   Rev 2.0   Feb 27 1999 11:57:00   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   11 Mar 1998 17:03:24   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *    COPY-MEMBER: DPAT0066
      *    --------------------------------------------------------
      *    Procedure-Copy für Fehlerverfahren
      *    --------------------------------------------------------
      ******************************************************************
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME
      * -------------------------------------------------------------
      * AAT00055!03.01.17!2.1 ! neu                       ! IFR
      *         !        !    !                           !
      *
      * ENDE-LOGBUCH
      ******************************************************************
      *
      *    Dieses Copy enthält die Anweisungen zur Durchführung
      *    eines Programmabbruchs falls er durch eine Fehleraktion
      *    erforderlich ist.
      *
      *    Voraussetzung: Copy DPAT0066
      *
      ******************************************************************
      *
           IF S-VOR-PROGRAMMENDE-ABBR-J
              MOVE 'Abbruch durch Fehlerhandling'
                                            TO ERR-VAR-ZEILE01
              MOVE K-1                      TO ERR-ORT-LFD
              MOVE 'S'                      TO ERR-ORT-SEC
              PERFORM BR93-PROG-FEHLER
              PERFORM U01-ABBRUCH

           END-IF


