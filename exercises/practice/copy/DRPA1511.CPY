      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1511.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/DRPA1511.CPv  $
      *
      *   Rev 2.1   Nov 28 2001 16:31:16   t13ex22
      *APA00167: Neuerstellung für DIT-Österreich (44)
      *
      *   Rev 2.0   04 Jul 2001 11:10:52   p13sp13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1511 PIN DAISY-INTERN FACHLICH PRÜFEN    *
      *                 FÜR DIT-ÖSTERREICH                           *
      *    -------------------------------------------------------   *
      *    ERSTELLT: MUND                                            *
      *    DATUM:    05.04.2001                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00167!22.11.01!2.0 ! NEU                       ! MUND     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1511                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1511.
             10 PI-DRPA1511-ZVP-VERSION     PIC S9(004) BINARY.
             10 PI-DRPA1511-PGEB-GES        PIC S9(004) BINARY.
             10 PI-DRPA1511-PIN             PIC S9(009) BINARY.
             10 PI-DRPA1511-VERTR-BEGINN    PIC  X(010).

       01 PO-DRPA1511.
          05 PO-DRPA1511-RC                 PIC  X(003).
             88 PO-DRPA1511-RC-OK                VALUE 'OK '.
             88 PO-DRPA1511-RC-VSTAT-GEFUELLT    VALUE 'VSG'.
      ******** ENDE COPY-MEMBER **************************************

