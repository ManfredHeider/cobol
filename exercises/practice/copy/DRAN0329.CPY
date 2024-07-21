      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  09.03.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0329.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAN0329.CPv  $
      *
      *   Rev 2.2   Mar 12 2018 14:25:48   t13ae34
      *AAN01351: M_9000_2b - Löschung der Antragsdaten nach Widersp
      *ruch
      *
      *   Rev 2.1   Mar 12 2018 09:39:42   t13ae34
      *AAN01351: M_9000_2b - Löschung der Antragsdaten nach Widersp
      *ruch
      *
      *   Rev 2.0   Mar 09 2018 17:59:48   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0329  FUNKTION DELETE STANDARD-OZP       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN01351!12.03.18!2.1 ! NEUERSTELLUNG             ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0329                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  DELETE  <<<       TABELLE VIAB TBAN0007              *
      *                                                              *
      ****************************************************************
       01  PI-DRAN0329.
           05 PI-DRAN0329-VIA-KEY.
               10 PI-DRAN0329-PGEB-GES        PIC S9(4) COMP.
               10 PI-DRAN0329-VIA-HVANTNUM    PIC S9(17) COMP-3.
      *
       01  PO-DRAN0329.
           05 PO-DRAN0329-RC                  PIC  X(003).
             88 PO-DRAN0329-RC-OK                   VALUE 'OK '.
             88 PO-DRAN0329-RC-NOT-FOUND            VALUE 'NFD'.
      ****** ENDE COPY-MEMBER ****************************************


