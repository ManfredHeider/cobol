      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  08.03.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0844.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0844.CPv  $
      *
      *   Rev 2.1   13 Apr 2007 17:32:20   t13ae13
      *AVB00907: Beförderungsbedingungen Österreich 2007
      *
      *   Rev 2.0   08 Mar 2007 09:32:00   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0844                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<ERM. ABR-DAT ZU DAT >>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    22.10.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!22.10.98! 01 ! NEU                       ! WOLFF    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0844
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01 PI-DRVB0844.
          05 PI-DRVB0844-DATEN.
             10 PI-DRVB0844-KNR-STEUER-INFO         PIC S9(004) BINARY.
             10 PI-DRVB0844-STICHTAG                PIC  X(010).
      *
       01 PO-DRVB0844.
          05 PO-DRVB0844-RC                         PIC  X(003).
             88 PO-DRVB0844-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0844-RC-NOT-FOUND                 VALUE 'NFD'.
          05 PO-DRVB0844-DATEN.
             10 PO-DRVB0844-ZEITPUNKT-STAT          PIC  X(010).
      *
      ******* ENDE COPY-MEMBER **************************************
