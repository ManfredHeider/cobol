      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  13.03.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0331.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAN0331.CPv  $
      *
      *   Rev 2.2   May 29 2018 11:34:42   t13ae34
      *AAN01368: AM-Leben: Neue ungezillmerte bAV-Tarife. Fehlende 
      *copys.
      *
      *   Rev 2.1   Mar 13 2018 17:29:14   T13AE34
      *AAN01351: M_9000_2b - Löschung der Antragsdaten nach Widersp
      *ruch
      *
      *   Rev 2.0   Mar 13 2018 17:08:22   T13AE34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0331  FUNKTION DELETE STANDARD-OZP       *
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
      *    FÜR DIE FUNKTION DRAN0331                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  DELETE TABELLE PZM  TBAN0007                         *
      *                                                              *
      ****************************************************************
       01  PI-DRAN0331.
           05 PI-DRAN0331-PZM-KEY.
               10 PI-DRAN0331-PGEB-GES          PIC S9(4) COMP.
               10 PI-DRAN0331-PZM-HVANTNUM      PIC X(9).
      *
       01  PO-DRAN0331.
           05 PO-DRAN0331-RC                    PIC  X(003).
             88 PO-DRAN0331-RC-OK               VALUE 'OK '.
             88 PO-DRAN0331-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ****** ENDE COPY-MEMBER ****************************************


