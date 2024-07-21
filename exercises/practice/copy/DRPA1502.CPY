      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1502.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRPA1502.CPv  $
      *
      *   Rev 2.4   Jun 10 2008 10:33:40   t13ex21
      *APA00712: FVD Stufe 1: Tabellenerweiterungen
      *
      *   Rev 2.3   24 Jul 2001 10:12:50   t13ae46
      * 
      *
      *   Rev 2.2   20 Jul 2001 09:38:00   t13ae46
      *APA00133
      *
      *   Rev 2.1   16 Jul 2001 11:26:44   t13ae46
      *APA00133
      *
      *   Rev 2.0   04 Jul 2001 11:10:24   p13sp13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *   COPY-MEMBER: DRPA1502 FUNKTION UWPA0046                    *
      *                DREBA-CH-PM FORMAL PRUEFEN UND NORMIEREN      *
      *   -------------------------------------------------------    *
      ****************************************************************
      *    ERSTELLT: WANG                                            *
      *    DATUM:    16.07.01                                        *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00133!16.07.01!2.1 ! NEUERSTELLUNG             ! WANG     *
      * APA00712!08.04.08!2.2 ! PI-ERWEITERUNG UM MANDANT ! STORCH   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1502                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA1502.
             10 PI-DRPA1502-PZVP-VERSION    PIC S9(004) BINARY.
             10 PI-DRPA1502-PEG-DATEN-DREBA-CH.
                COPY DRPA1501.
080408       10 PI-DRPA1502-KNR-MANDANT     PIC S9(004) BINARY.
      *
       01 PO-DRPA1502.
          05 PO-DRPA1502-RC                 PIC  X(003).
             88 PO-DRPA1502-RC-OK                VALUE 'OK '.
             88 PO-DRPA1502-RC-VSTAT-GEFUELLT    VALUE 'VSG'.
      *
      ******** ENDE COPY-MEMBER **************************************