      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Mai 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1578.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1578.CPv  $
      *
      *   Rev 2.1   Jul 16 2004 10:25:04   t13ex44
      *APA00308: Wiederverwendung Vertrags-Nr. bei der DeuBa
      *
      *   Rev 2.0   Jul 04 2002 15:31:24   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1578                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<UPDATE PVER-VNR>>                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SCHEIDEL                                         *
      *    DATUM:    28.06.2004                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00308!28.06.04!2.0 ! NEU                       ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1578                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1578.
          05 PI-DRPA1578-DATEN.
             10 PI-DRPA1578-PGEB-GES             PIC S9(004) COMP.
             10 PI-DRPA1578-PVER-VNR             PIC  X(015).
             10 PI-DRPA1578-PVER-VNR-MOD         PIC  X(015).
      *
       01 PO-DRPA1578.
          05 PO-DRPA1578-RC                         PIC  X(003).
             88 PO-DRPA1578-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1578-RC-NOT-FOUND                  VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER **************************************
