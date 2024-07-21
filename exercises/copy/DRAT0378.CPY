      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0378.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRAT0378.CPv  $
      *
      *   Rev 2.1   13 Jul 2001 12:11:32   t13ae34
      *AKI00202 - Pflege Batch-Fehlerhandling
      *
      *   Rev 2.0   19 Apr 2001 14:26:50   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0378  KONSTANTEN FÜR KTY = 201           *
      *                           (FEHLERRELEVANZ)                   *
      *                 ZUGRIFF ÜBER KNR_BEZ_KURZ !                  *
      *                                                              *
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
           05 K-KNR-FEHLERREL-HINWEIS   PIC  X(001) VALUE 'H'.
           05 K-KNR-FEHLERREL-WARNUNG   PIC  X(001) VALUE 'W'.
           05 K-KNR-FEHLERREL-FEHLER    PIC  X(001) VALUE 'F'.
