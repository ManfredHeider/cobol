      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1488.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRPA1488.CPv  $
      *
      *   Rev 2.2   06 Apr 2001 08:34:56   T13EX43
      *APA00131 : Formatänderung PI-DRPA1488-VNR
      *
      *   Rev 2.1   05 Apr 2001 16:17:48   T13EX43
      *APA00131 : PEG Commerzbank - neue Funktion
      *
      *   Rev 2.0   09 Jan 2001 10:22:30   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1488 VNR DAISY-INTERN FACHLICH PRÜFEN    *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                       *
      *    DATUM:    05.04.2001                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00131!05.04.01!2.1 ! NEU                       ! KLEEMEIER*
      * APA00131!06.04.01!2.2 ! FORMATÄNDERUNG VNR        ! KLEEMEIER*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1488                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1488.
             10 PI-DRPA1488-PGEB-GES        PIC S9(004) BINARY.
060401****   10 PI-DRPA1488-VNR             PIC  9(015).
060401       10 PI-DRPA1488-VNR             PIC  X(015).

       01 PO-DRPA1488.
          05 PO-DRPA1488-RC                 PIC  X(003).
             88 PO-DRPA1488-RC-OK                VALUE 'OK '.
             88 PO-DRPA1488-RC-VSTAT-GEFUELLT    VALUE 'VSG'.
      ******** ENDE COPY-MEMBER **************************************
