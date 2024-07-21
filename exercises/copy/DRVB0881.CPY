      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  10.12.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0881.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0881.CPv  $
      *
      *   Rev 2.1   10 Dec 2007 15:17:16   t13ae30
      *AVB00905: Allfinanz Stufe 1 im VB-System
      *
      *   Rev 2.0   10 Dec 2007 11:44:24   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0881  FUNKTION                           *
      *                           ERMITTELN DES AM-AGENTUR-NR        *
      *    --------------------------------------------------------- *
      *    ERSTELLT: WILLIG                                          *
      *    DATUM:    05.07.2007                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00905!05.07.07! 01 ! EINFÜHRUNG ALLFINANZ      ! WILLIG   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0881                                  *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0881.
          05 PI-DRVB0881-DATEN.
             10 PI-DRVB0881-KL-ST-NR         PIC S9(009) BINARY.
      *
       01 PO-DRVB0881.
          05 PO-DRVB0881-RC                  PIC  X(003).
             88 PO-DRVB0881-RC-OK            VALUE 'OK '.
             88 PO-DRVB0881-RC-NFD           VALUE 'NFD'.

          05 PO-DRVB0881-DATEN.
             10 PO-DRVB0881-KL-AM-AGENT-NR   PIC S9(009) COMP.
      *
      ******** Ende Copy-Member **************************************
