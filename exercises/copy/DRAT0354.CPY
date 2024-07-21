      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0354.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRAT0354.CPv  $
      *
      *   Rev 2.1   02 May 2001 08:59:10   t13ex49
      *AKI00161 - erstmalige Prod.-Übergabe
      *
      *   Rev 2.0   01 Mar 2001 13:03:14   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *   Dieses Copy beschreibt den Aufbau für die Tabelle
      *   zur internen Ermittlung der Fehleranzahl in einem Batch-
      *   programm
      *
      *
      *--------------------------------------------------------------*
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00161!14.03.01!    ! Neuerstellung             !Peter Pohl*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *--------------------------------------------------------------*
      *
      * Tabelle Anzahl Fehler pro Feld
       01 TB-FEHLER-STATISTIK.
      *    Achtung - wer den OCCURS-Wert ändert muss auch den
      *              MAX-Wert ändern!
           05  TB-Fehler-ID OCCURS 100.
              15 TB-Fehler-IDX.
                 20  TB-Fehler-Nummer      PIC 9(4).
                 20  TB-Fehler-Feldname    PIC X(15).
              15 TB-Job-Name               PIC X(08).
              15 TB-Fehler-Relevanz        PIC X(01).
              15 TB-Fehler-Reaktion        PIC X(01).
              15 TB-Fehler-Anzahl          PIC 9(4).
      *
       01  TB-Fehler-MAX                   PIC 9(03) COMP-3 VALUE 100.
      *

