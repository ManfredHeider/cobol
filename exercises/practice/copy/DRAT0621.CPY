      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  25.10.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0621.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAT0621.CPv  $
      *
      *   Rev 2.2   Nov 06 2018 11:24:12   t13ac23
      *AKI01640: Erweiterung des Bestands-DTAs um Timestamp - Nacha
      *rbeiten EVWS und PSLife - Source
      *
      *   Rev 2.1   Nov 06 2018 10:55:58   t13ac23
      *AKI01640: Erweiterung des Bestands-DTAs um Timestamp - Nacha
      *rbeiten EVWS und PSLife - Source
      *
      *   Rev 2.0   Oct 25 2018 08:55:58   t13ac23
      *Archiverstellung
      *
      *****************************************************************
      *    COPY-MEMBER: DRAT0621                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRAT0621                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01640!25.10.18!2.1 ! NEUERSTELLUNG             ! DETMAR   *
      *         !        !2.2 !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRAT0621
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRAT0621.
           05  PI-DRAT0621-KEY.
 
             10 PI-DRAT0621-TIMESTAMP             PIC  X(026).
      *
 
      *
       01  PO-DRAT0621.
           05  PO-DRAT0621-RC                     PIC  X(003).
               88 PO-DRAT0621-RC-OK               VALUE 'OK '.
               88 PO-DRAT0621-RC-NOT-FOUND        VALUE 'NFD'.
           05  PO-DRAT0621-FEHLERMELDUNG          PIC  X(080).
      *
      ******* ENDE COPY-MEMBER ***************************************
 

