      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  30.07.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1902.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1902.CPv  $
      *
      *   Rev 2.1   Sep 21 2018 12:19:58   t13ex44
      *APA02548: Nachfolgeregelung Direktion Familie
      *
      *   Rev 2.0   Jul 30 2018 11:20:32   t13ex44
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1902 FUNKTION UWPA0021                    *
      *    -------------------------------------------------------    *
      *    PRUEFEN UND EVTL. EINBUCHEN EINES NRDF                     *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * APA02548!26.07.18!2.0 ! NEU                       ! SCHEIDEL  *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER          *
      *    FÜR DIE FUNKTION DRPA1902                                  *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *****************************************************************
      *
       01 PI-DRPA1902.
          05 PI-DRPA1902-DATEN.
             10 PI-DRPA1902-TERMIN     PIC  X(010).
      *
       01 PO-DRPA1902.
          05 PO-DRPA1902-RC            PIC  X(003).
             88 PO-DRPA1902-RC-OK           VALUE 'OK '.
             88 PO-DRPA1902-RC-VSG          VALUE 'VSG'.
             88 PO-DRPA1902-RC-NOT-OK       VALUE 'NOK'.
          05 PO-DRPA1902-DATEN.
             10 PO-DRPA1902-STRK-AEND-KZ
                                       PIC  X(001).
      *
      ******* ENDE COPY-MEMBER **************************************
 

