      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  13.03.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0330.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAN0330.CPv  $
      *
      *   Rev 2.1   Mar 13 2018 09:14:46   t13ae34
      *AAN01351: M_9000_2b - Löschung der Antragsdaten nach Widersp
      *ruch
      *
      *   Rev 2.0   Mar 13 2018 08:43:58   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0330  FUNKTION DELETE STANDARD-OZP       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN01351!12.03.18!2.1 ! NEU                       ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0330                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  DELETE TABELLE DVM  TBAN0020                         *
      *                                                              *
      ****************************************************************
       01  PI-DRAN0330.
           05 PI-DRAN0330-DVM-KEY.
               10 PI-DRAN0330-PGEB-GES          PIC S9(4) COMP.
               10 PI-DRAN0330-DVM-HVANTNUM      PIC X(9).
      *
       01  PO-DRAN0330.
           05 PO-DRAN0330-RC                    PIC  X(003).
             88 PO-DRAN0330-RC-OK               VALUE 'OK '.
             88 PO-DRAN0330-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ****** ENDE COPY-MEMBER ****************************************

