      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  10.09.2003
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0100.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAN0100.CPv  $
      *
      *   Rev 2.2   Mar 10 2014 16:44:32   t13ae23
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g) Migration
      *
      *   Rev 2.1   Sep 11 2003 15:14:06   t13ae34
      *
      *   Rev 2.0   Sep 10 2003 15:12:40   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0100  FUNKTION ERMITTELN KONDITION       *
      *                           FüR GRUNDPROVISION EING. GESCHÄFT  *
      *    -------------------------------------------------------   *
      *    ERSTELLT: HEIDER                                          *
      *    DATUM:    10.09.2003                                      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * AAN00369!10.09.03!2.1  !NEU                       !HEIDER    *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0100                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRAN0100.
          05 PI-DRAN0100-PZVP-VERSION           PIC S9(004)    BINARY.
          05 PI-DRAN0100-DATEN.
             10 PI-DRAN0100-PGEB-GES            PIC S9(004) BINARY.
             10 PI-DRAN0100-PROD-PROD-SCHL      PIC S9(004) BINARY.
             10 PI-DRAN0100-STICHTAG            PIC  X(010).
             10 PI-DRAN0100-PRODART             PIC S9(004) BINARY.
             10 PI-DRAN0100-LAUFZEIT            PIC S9(004) BINARY.
      *
       01 PO-DRAN0100.
          05 PO-DRAN0100-RC                     PIC  X(003).
             88 PO-DRAN0100-RC-OK                    VALUE 'OK '.
             88 PO-DRAN0100-RC-NOT-FOUND             VALUE 'NFD'.
             88 PO-DRAN0100-RC-SEL-KRIT-NOK          VALUE 'SKN'.
             88 PO-DRAN0100-RC-KOND-DEF-NOK          VALUE 'KDN'.
         05 PO-DRAN0100-DATEN.
             COPY DRPA0547.
      ******** Ende Copy-Member **************************************
