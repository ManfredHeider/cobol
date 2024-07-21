      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  25.01.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1127.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1127.CPv  $
      *
      *   Rev 2.1   Jan 26 2016 07:40:36   t13ex67
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System
      *
      *   Rev 2.0   Jan 25 2016 08:11:30   t13ex67
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1127                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<ERM. ABR-DAT ZU DAT >>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: STORCH                                           *
      *    DATUM:    25.01.2016                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA01932!25.01.16!2.01! NEU                       ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB1127
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01 PI-DRVB1127.
          05 PI-DRVB1127-DATEN.
             10 PI-DRVB1127-KNR-STEUER-INFO         PIC S9(004) BINARY.
             10 PI-DRVB1127-STICHTAG                PIC  X(010).
      *
       01 PO-DRVB1127.
          05 PO-DRVB1127-RC                         PIC  X(003).
             88 PO-DRVB1127-RC-OK                        VALUE 'OK '.
             88 PO-DRVB1127-RC-NOT-FOUND                 VALUE 'NFD'.
          05 PO-DRVB1127-DATEN.
             10 PO-DRVB1127-LET-MA-ABR-DAT          PIC  X(010).
             10 PO-DRVB1127-NAE-MA-ABR-DAT          PIC  X(010).
      *
      ******* ENDE COPY-MEMBER **************************************

