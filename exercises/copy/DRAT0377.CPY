      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0377.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRAT0377.CPv  $
      *
      *   Rev 2.1   13 Jul 2001 12:08:46   t13ae34
      *AKI00202 - Pflege Batch-Fehlerhandling
      *
      *   Rev 2.0   19 Apr 2001 14:26:50   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0377                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRAT0377                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: HEIDER                                           *
      *    DATUM:    05.07.2001                                       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00202!05.07.01!2.1 ! NEU                       ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRAT0377
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION DRAT0377 DES BUFFER-OZP ZYAT0003 ERMITTELT
      *    DIE ERGEBNISMENGE FÜR DIALOG SF26 'BATCH-FEHLERHANDLING'
      *
      ****************************************************************
      *
       01  PI-DRAT0377.
           05  PI-DRAT0377-UFKT                    PIC  9(002).
           05  PI-DRAT0377-NAME-ZW-SPEICHER        PIC  X(008).
           05  PI-DRAT0377-LOG-SATZ-NR             PIC  9(005).
           05  PI-DRAT0377-AUSWAHL-KRITERIEN.
               10 PI-DRAT0377-FR-SYSCODE           PIC X(2).
               10 PI-DRAT0377-FR-JOBNAME           PIC X(8).
               10 PI-DRAT0377-FR-PROGRAMMNAME      PIC X(8).
      *
       01  PO-DRAT0377.
           05  PO-DRAT0377-RC                      PIC  X(03).
               88 PO-DRAT0377-RC-OK                     VALUE 'OK '.
               88 PO-DRAT0377-RC-EOF                    VALUE 'EOF'.
               88 PO-DRAT0377-RC-LEER                   VALUE 'LER'.

           05  PO-DRAT0377-ANZAHL-ERG-ZEILEN       PIC  9(05).

           05  PO-DRAT0377-ERGEBNISZEILE.
               COPY DRAT0379.
      *
      ********** ENDE COPY-MEMBER **************************************
