      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1466.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1466.CPv  $
      *
      *   Rev 2.1   05 Feb 2001 10:22:08   T13EX22
      *APA00118: Anpassungen für PEG-BFG
      *
      *   Rev 2.0   09 Jan 2001 10:12:16   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1466  FUNKTION WÄHRUNG NORMIEREN         *
      *                                    ERMITTELN (UPPA0067)      *
      *    --------------------------------------------------------- *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00118!23.01.01. 02 ! NEUERSTELLUNG             ! MUND     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1466                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1466.
          05 PI-DRPA1466-DATEN.
             10 PI-DRPA1466-VERTRAG-WAEHRUNG  PIC  X(003).
             10 PI-DRPA1466-PGEB-GES          PIC S9(004) BINARY.
      *
       01 PO-DRPA1466.
          05 PO-DRPA1466-RC                  PIC  X(003).
             88 PO-DRPA1466-RC-OK                 VALUE 'OK '.
             88 PO-DRPA1466-RC-UNZ                VALUE 'UNZ'.
          05 PO-DRPA1466-DATEN.
             10 PO-DRPA1466-KNR-WAEHRUNG     PIC S9(004)  BINARY.
      *
      ******** ENDE COPY-MEMBER **************************************
