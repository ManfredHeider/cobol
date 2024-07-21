      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  15.03.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3331.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3331.CPv  $
      *
      *   Rev 2.1   Mar 27 2018 16:02:56   t13ac20
      *AKI01579: LENA-Erweiterung SRT04: Vertragslöschung
      *
      *   Rev 2.0   Mar 15 2018 18:18:32   t13ac20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3331
      *    --------------------------------------------------------
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3331
      *    --------------------------------------------------------
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME
      * -------------------------------------------------------------
      * AKI01579!15.03.18!2.1 ! NEUERSTELLUNG             ! SWO
      *         !        !    !                           !
      *         !        !    !                           !
      *
      * ENDE-LOGBUCH
      *****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3331
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION: LÖSCHEN VERTRÄGE AUS TEMPORAL TABLE
      *
      *****************************************************************
      *
       01  PI-DRKI3331.
           05 PI-DRKI3331-VNR                     PIC  X(015).
      *
       01  PO-DRKI3331.
           05 PO-DRKI3331-RC                      PIC  X(003).
              88 PO-DRKI3331-RC-OK                       VALUE 'OK '.
      *
           05 PO-DRKI3331-DATEN.
              10 PO-DRKI3331-COUNTER-484          PIC S9(9) BINARY.
              10 PO-DRKI3331-COUNTER-485          PIC S9(9) BINARY.
              10 PO-DRKI3331-COUNTER-486          PIC S9(9) BINARY.
      *
      ******* ENDE COPY-MEMBER ****************************************

