      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  22.06.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1191.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1191.CPv  $
      *
      *   Rev 2.1   25 Jun 2018 15:28:58   t13ae50
      *AVB01479: Einführung Allfinanz AG im VB-System - Statistikpa
      *ket - Programmen(sla)
      *
      *   Rev 2.0   22 Jun 2018 11:01:32   t13ae50
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1191                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB1191                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: S.LAS                                            *
      *    DATUM:    22.06.2018                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * --------+--------+----+---------------------------+----------*
      * AVB14795!22.06.18!2.1 ! NEU                       ! S.LAS    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB1191
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB1191.
           05  PI-DRVB1191-DIR-KEY.
               10 PI-DRVB1191-DIR-NR        PIC S9(004) BINARY.
      *
       01  PO-DRVB1191.
           05  PO-DRVB1191-RC               PIC  X(003).
               88 PO-DRVB1191-RC-OK                      VALUE 'OK '.
               88 PO-DRVB1191-RC-NOT-FOUND               VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER **************************************
      *

