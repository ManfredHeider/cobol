      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Mai 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1577.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1577.CPv  $
      *
      *   Rev 2.1   Jul 16 2004 10:24:56   t13ex44
      *APA00308: Wiederverwendung Vertrags-Nr. bei der DeuBa
      *
      *   Rev 2.0   Jul 04 2002 15:31:20   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1577                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<UPDATE WIEDERVERWENDETE VNR>>            *
      *    --------------------------------------------------------   *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00308!28.06.04!2.0 ! NEU                       ! SCHEIDEL *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1577                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
        01 PI-DRPA1577.
           05 PI-DRPA1577-DATEN.
              10 PI-DRPA1577-PGEB-GES         PIC S9(004)  BINARY.
              10 PI-DRPA1577-VNR              PIC  X(015).
      *
        01 PO-DRPA1577.
           05 PO-DRPA1577-RC                  PIC  X(003).
              88 PO-DRPA1577-RC-OK            VALUES 'OKM'
                                                   , 'OKO'.
              88 PO-DRPA1577-RC-OKM           VALUE  'OKM'.
              88 PO-DRPA1577-RC-OKO           VALUE  'OKO'.
              88 PO-DRPA1577-RC-NFD           VALUE  'NFD'.
      *
      ******* ENDE COPY-MEMBER **************************************
