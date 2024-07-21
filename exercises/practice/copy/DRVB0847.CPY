      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.03.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0847.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0847.CPv  $
      *
      *   Rev 2.1   16 May 2007 11:22:36   t13ae13
      *AVB00908: Neuer VB-Vertrag 2007 - UPVB0128
      *
      *   Rev 2.0   15 Mar 2007 12:08:40   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ***************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00908!14.05.07!2.1 ! NEU                       !Walb      *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0847                                 *
      *    ERMITTELN MA-BEFBED-NEU                                   *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0847.
          05 PI-DRVB0847-DATEN.
             10 PI-DRVB0847-MA-ST-NR         PIC S9(009) COMP.

      *
       01 PO-DRVB0847.
          05 PO-DRVB0847-RC                  PIC  X(003).
             88 PO-DRVB0847-RC-OK            VALUE 'OK '.
             88 PO-DRVB0847-RC-NFD           VALUE 'NFD'.
          05 PO-DRVB0847-DATEN.
      *      DATUM IM ISO FORMAT
             10 PO-DRVB0847-MA-BEFBED-WIEN-07 PIC X(10).
      ******** ENDE COPY-MEMBER **************************************
