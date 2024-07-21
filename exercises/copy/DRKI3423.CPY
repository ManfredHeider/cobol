      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  06.09.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3423.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI3423.CPv  $
      *
      *   Rev 2.3   Jan 22 2019 15:25:22   t13ex78
      *AKI01641: Temp. table nach KD-Löschung bereinigen
      *
      *   Rev 2.2   Sep 11 2018 11:29:28   t13ac23
      *AKI01629: Frischer Wind - Nacharbeiten löschen der gelöschte
      *n Kunden aus den KSDSen für Basisdaten - Sourcen
      *
      *   Rev 2.1   Sep 11 2018 10:40:56   t13ac23
      *AKI01629: Frischer Wind - Nacharbeiten löschen der gelöschte
      *n Kunden aus den KSDSen für Basisdaten - Sourcen
      *
      *   Rev 2.0   Sep 06 2018 10:51:56   t13ac23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3423
      *    --------------------------------------------------------
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3423
      *    --------------------------------------------------------
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME
      * -------------------------------------------------------------
      * AKI01629!06.09.18!2.1 ! NEUERSTELLUNG             ! DETMAR
      * -------------------------------------------------------------
      * AKI01641!22.01.19!2.3 ! Jetzt auch in TCKI0517 und! tfl
      *         !        !    ! TCKI0518 löschen          !
      *         !        !    !                           !
      *
      * ENDE-LOGBUCH
      *****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3423
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION: LÖSCHEN VERTRÄGE AUS TEMPORAL TABLE
      *
      *****************************************************************
      *
       01  PI-DRKI3423.
           05 PI-DRKI3423-GES                     PIC  X(002).
           05 PI-DRKI3423-VNR                     PIC  X(015).
      *
       01  PO-DRKI3423.
           05 PO-DRKI3423-RC                      PIC  X(003).
              88 PO-DRKI3423-RC-OK                       VALUE 'OK '.
      *
           05 PO-DRKI3423-DATEN.
              10 PO-DRKI3423-COUNTER-490          PIC S9(9) BINARY.
              10 PO-DRKI3423-COUNTER-491          PIC S9(9) BINARY.
              10 PO-DRKI3423-COUNTER-517          PIC S9(9) BINARY.
              10 PO-DRKI3423-COUNTER-518          PIC S9(9) BINARY.
      *
      ******* ENDE COPY-MEMBER ****************************************
 

