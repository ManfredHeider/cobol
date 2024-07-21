      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  13.09.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3467.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3467.CPv  $
      *
      *   Rev 2.1   Nov 27 2018 11:38:20   t13ex27
      *AKI01600: M_9000_2a - Prio A - Sperren & Löschen von Verträg
      *en in KI, DBO & VBI Temporal Tables
      *
      *   Rev 2.0   Sep 13 2018 18:34:16   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3467
      *    --------------------------------------------------------
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3467
      *    --------------------------------------------------------
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME
      * -------------------------------------------------------------
      * AKI01600!12.09.18!2.1 ! NEUERSTELLUNG             ! IFR
      *         !        !    !                           !
      *         !        !    !                           !
      *
      * ENDE-LOGBUCH
      *****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3467
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION: LÖSCHEN VERTRÄGE AUS TEMPORAL TABLE
      *
      *****************************************************************
      *
       01  PI-DRKI3467.
           05 PI-DRKI3467-GES                     PIC  X(002).
           05 PI-DRKI3467-VNR                     PIC  X(015).
      *
       01  PO-DRKI3467.
           05 PO-DRKI3467-RC                      PIC  X(003).
              88 PO-DRKI3467-RC-OK                       VALUE 'OK '.
      *
           05 PO-DRKI3467-DATEN.
              10 PO-DRKI3467-COUNTER-468          PIC S9(9) BINARY.
              10 PO-DRKI3467-COUNTER-487          PIC S9(9) BINARY.
              10 PO-DRKI3467-COUNTER-489          PIC S9(9) BINARY.
              10 PO-DRKI3467-COUNTER-301          PIC S9(9) BINARY.
      *
      ******* ENDE COPY-MEMBER ****************************************

