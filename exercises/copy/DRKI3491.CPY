      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  12.09.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3491.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI3491.CPv  $
      *
      *   Rev 2.4   Dec 10 2020 09:45:04   u130684
      *AKI01773: Löschen aus Temporal Table
      *
      *   Rev 2.3   Dec 04 2020 14:18:46   u130684
      *AKI01820: Nacharbeiten DVM Compeon
      *
      *   Rev 2.2   Nov 10 2020 12:03:28   u130684
      *AKI01773: Löschen aus Temporal Table
      *
      *   Rev 2.1   Nov 27 2018 11:43:56   t13ex27
      *AKI01600: M_9000_2a - Prio A - Sperren & Löschen von Verträg
      *en in KI, DBO & VBI Temporal Tables
      *
      *   Rev 2.0   Sep 12 2018 16:05:36   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3491
      *    --------------------------------------------------------
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3491
      *    --------------------------------------------------------
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME
      * -------------------------------------------------------------
      * AKI01600!12.09.18!2.1 ! NEUERSTELLUNG             ! IFR
      * --------!--------!----!---------------------------!----------
      * AKI01773!12.10.20!2.2 ! ERWEITERUNG UM COUNTER    ! U130684
      *         !        !    ! 540 (G90CVERTRAG) UND     !
      *         !        !    ! 541 (G90EVERTRAG)         !
      *         !        !    !                           !
      *
      * ENDE-LOGBUCH
      *****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3491
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION: LÖSCHEN VERTRÄGE AUS TEMPORAL TABLE
      *
      *****************************************************************
      *
       01  PI-DRKI3491.
           05 PI-DRKI3491-GES                     PIC  X(002).
           05 PI-DRKI3491-VNR                     PIC  X(015).
      *
       01  PO-DRKI3491.
           05 PO-DRKI3491-RC                      PIC  X(003).
              88 PO-DRKI3491-RC-OK                       VALUE 'OK '.
      *
           05 PO-DRKI3491-DATEN.
              10 PO-DRKI3491-COUNTER-475          PIC S9(9) BINARY.
              10 PO-DRKI3491-COUNTER-476          PIC S9(9) BINARY.
              10 PO-DRKI3491-COUNTER-540          PIC S9(9) BINARY.
              10 PO-DRKI3491-COUNTER-541          PIC S9(9) BINARY.
      *
      ******* ENDE COPY-MEMBER ****************************************




