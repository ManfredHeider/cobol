      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01.07.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0109.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRAN0109.CPv  $
      *
      *   Rev 2.6   Jul 15 2005 11:45:24   t13ae23
      *AAN00305: DIT-Aufbauplan
      *
      *   Rev 2.5   Jul 15 2005 10:18:26   t13ae23
      *AAN00305: DIT-Aufbauplan
      *
      *   Rev 2.4   Jul 01 2005 17:14:22   t13ae34
      *AAN00305: DIT-Aufbauplan
      *
      *   Rev 2.3   Jul 01 2005 17:05:30   t13ae34
      *AAN00305: DIT-Aufbauplan
      *
      *   Rev 2.2   Jul 01 2005 16:56:48   t13ae34
      *AAN00305: DIT-Aufbauplan
      *
      *   Rev 2.1   Jul 01 2005 16:35:54   t13ae34
      *AAN00305: DIT-Aufbauplan
      *
      *   Rev 2.0   Jul 01 2005 13:00:00   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0109                                     *
      *    FUNKTION   : IST DAS PRODUKT LAUFZEITABHÄNGIG ?           *
      *    -------------------------------------------------------   *
      *    ERSTELLT: HEIDER                                          *
      *    DATUM:    01.07.2005                                      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00582!01.07.05!2.1 !NEU                        !HEIDER    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0109                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRAN0109.
          05 PI-DRAN0109-DATEN.
             10 PI-DRAN0109-PGEB-GES       PIC S9(004) COMP.
             10 PI-DRAN0109-PROD-PROD-SCHL PIC S9(004) COMP.
             10 PI-DRAN0109-STICHTAG       PIC  X(010).
      *
       01 PO-DRAN0109.
          05 PO-DRAN0109-RC              PIC  X(003).
             88 PO-DRAN0109-RC-OK             VALUE 'OK '.
             88 PO-DRAN0109-RC-NOK            VALUE 'NOK'.
      ******** ENDE COPY-MEMBER **************************************
