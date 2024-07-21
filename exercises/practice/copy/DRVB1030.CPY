      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  07.09.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1030.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1030.CPv  $
      *
      *   Rev 2.1   Sep 30 2011 08:27:56   t13ae13
      *AVB01185: Maßnahmenpaket - Teil 1
      *
      *   Rev 2.0   Sep 07 2011 09:42:18   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ***************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB01185!07.09.11!2.1 ! NEU                       !Walb      *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB1030                                 *
      *    ERMITTELN MA-BEFBED-NEU                                   *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB1030.
          05 PI-DRVB1030-DATEN.
             10 PI-DRVB1030-MA-ST-NR         PIC S9(009) COMP.

      *
       01 PO-DRVB1030.
          05 PO-DRVB1030-RC                  PIC  X(003).
             88 PO-DRVB1030-RC-OK            VALUE 'OK '.
             88 PO-DRVB1030-RC-NFD           VALUE 'NFD'.
          05 PO-DRVB1030-DATEN.
      *      DATUM IM ISO FORMAT
             10 PO-DRVB1030-MA-BEFBED-2011   PIC X(10).
      ******** ENDE COPY-MEMBER **************************************
