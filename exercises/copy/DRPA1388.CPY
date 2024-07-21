      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1388.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1388.CPv  $
      *
      *   Rev 2.1   25 May 2000 17:38:04   T13EX44
      *Neu
      *
      *   Rev 2.0   Apr 11 2000 16:58:04   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1388 FUNKTION PRINTE ERMITTELN           *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: SCHEIDEL                                        *
      *    DATUM:    25.05.2000                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00090!25.05.00!2.1 !NEU                        ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1388                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1388.
             10 PI-DRPA1388-PGEB-GES        PIC S9(004) BINARY.
             10 PI-DRPA1388-VNR             PIC  X(015).
       01 PO-DRPA1388.
          05 PO-DRPA1388-RC                 PIC  X(003).
             88 PO-DRPA1388-RC-OK                VALUE 'OK '.
             88 PO-DRPA1388-RC-NOT-FOUND         VALUE 'NFD'.
          05 PO-DRPA1388-DATEN.
             10 PO-DRPA1388-PRINTE          PIC S9(015)  PACKED-DECIMAL.
      ******** ENDE COPY-MEMBER **************************************
