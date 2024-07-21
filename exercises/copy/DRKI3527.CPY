      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  30.11.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3527.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3527.CPv  $
      *
      *   Rev 2.1   Dec 11 2018 17:09:34   t13ae52
      *AKI01649: Generali - Löschung Kunden- und Vertragsdaten aus 
      *Temporal Tables - Sourcen/COPY
      *
      *   Rev 2.0   Nov 30 2018 11:22:40   t13ae52
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3527
      *    --------------------------------------------------------
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3527
      *    --------------------------------------------------------
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME
      * -------------------------------------------------------------
      * AKI01649!30.11.18!2.1 ! NEUERSTELLUNG             !SCHUMACHER
      *         !        !    !                           !
      *         !        !    !                           !
      *
      * ENDE-LOGBUCH
      *****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3527
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION: LÖSCHEN VERTRÄGE AUS TEMPORAL TABLE
      *
      *****************************************************************
      *
       01  PI-DRKI3527.
           05 PI-DRKI3527-VNR                     PIC  X(015).
      *
       01  PO-DRKI3527.
           05 PO-DRKI3527-RC                      PIC  X(003).
              88 PO-DRKI3527-RC-OK                VALUE 'OK '.
      *
           05 PO-DRKI3527-DATEN.
              10 PO-DRKI3527-COUNTER-507          PIC S9(9) BINARY.
              10 PO-DRKI3527-COUNTER-508          PIC S9(9) BINARY.
              10 PO-DRKI3527-COUNTER-511          PIC S9(9) BINARY.
              10 PO-DRKI3527-COUNTER-512          PIC S9(9) BINARY.
              10 PO-DRKI3527-COUNTER-513          PIC S9(9) BINARY.
              10 PO-DRKI3527-COUNTER-515          PIC S9(9) BINARY.
              10 PO-DRKI3527-COUNTER-516          PIC S9(9) BINARY.
      *
      ******* ENDE COPY-MEMBER ****************************************

