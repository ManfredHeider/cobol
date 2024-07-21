      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0380.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRAT0380.CPv  $
      *
      *   Rev 2.1   13 Jul 2001 12:17:12   t13ae34
      *AKI00202 - Pflege Batch-Fehlerhandling
      *
      *   Rev 2.0   19 Apr 2001 14:26:52   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0380  KONSTANTEN FÜR KTY = 202           *
      *                           (FEHLERREAKTION)                   *
      *                 ZUGRIFF ÜBER KNR_BEZ_KURZ !                  *
      *    -------------------------------------------------------   *
      *    ERSTELLT: HEIDER                                          *
      *    DATUM:    JULI 2001                                       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME    *
      * -------------------------------------------------------------*
      * AKI00202!05.07.01!2.1  ! NEUERSTELLUNG             ! HEIDER  *
      *         !        !     !                           !         *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  FILLER.
      *       KEINE REAKTION
           05 K-KNR-FEHLERREA-KEINE     PIC  X(001) VALUE ' '.
      *       FELD INITIALISIEREN
           05 K-KNR-FEHLERREA-INIT      PIC  X(001) VALUE 'I'.
      *       SATZ NICHT WEITERVERARBEITEN
           05 K-KNR-FEHLERREA-SATZ      PIC  X(001) VALUE 'S'.
      *       VERTRAG NICHT WEITERVERARBEITEN
           05 K-KNR-FEHLERREA-VERTRAG   PIC  X(001) VALUE 'V'.
      *       PROGRAMM ABBRECHEN
           05 K-KNR-FEHLERREA-ABBRUCH   PIC  X(001) VALUE 'A'.
