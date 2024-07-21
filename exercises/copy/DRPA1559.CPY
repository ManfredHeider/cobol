      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Mai 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1559.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/DRPA1559.CPv  $
      *
      *   Rev 2.1   Apr 22 2003 08:36:34   t13ae46
      *APA00228: Neu
      *
      *   Rev 2.0   Jul 04 2002 15:29:52   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1559 ERMITTELN PNAP-VSTAT                 *
      *    -------------------------------------------------------    *
      *    ERSTELLT: J. WANG                                          *
      *    DATUM:    26.03.2003                                       *
      *****************************************************************
      *    VERSION   01                                               *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME      *
      * ------------------------------------------------------------- *
      * APA00228!26.03.03!01.01!NEU                       ! WANG      *
      *         !        !     !                          !           *
      *         !        !     !                          !           *
      *         !        !     !                          !           *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER          *
      *    FÜR DIE FUNKTION DRPA1559                                  *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
       01 PI-DRPA1559.
          05 PI-DRPA1559-DATEN.
             10 PI-DRPA1559-PNAP-VNR       PIC  X(015).
      *
       01 PO-DRPA1559.
          05 PO-DRPA1559-RC                PIC  X(003).
             88 PO-DRPA1559-RC-OK               VALUE 'OK '.
             88 PO-DRPA1559-RC-NFD              VALUE 'NFD'.
      *
      ******** ENDE COPY-MEMBER ***************************************
