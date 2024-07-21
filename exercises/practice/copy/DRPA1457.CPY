      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1457.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1457.CPv  $
      *
      *   Rev 2.1   05 Feb 2001 10:21:50   T13EX22
      *APA00118: Anpassungen für PEG-BFG
      *
      *   Rev 2.0   04 Jan 2001 11:48:18   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1457                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<ZAHLWEISE FACHLICH PRUEFEN >>            *
      *    --------------------------------------------------------   *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00118!11.01.01! 01 ! NEU                       ! MUND     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1457                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1457.
          05 PI-DRPA1457-DATEN.
             10 PI-DRPA1457-PGEB-GES         PIC S9(004)  BINARY.
             10 PI-DRPA1457-KNR-ZAHLWEISE    PIC S9(004)  BINARY.
      *
       01 PO-DRPA1457.
          05 PO-DRPA1457-RC                  PIC  X(003).
             88 PO-DRPA1457-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1457-RC-UNZULAESSIG                VALUE 'UNZ'.
      *
      ******* ENDE COPY-MEMBER **************************************
