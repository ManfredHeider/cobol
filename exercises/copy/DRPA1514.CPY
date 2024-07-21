      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1514.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA1514.CPv  $
      *
      *   Rev 2.3   Nov 02 2006 11:26:36   t13ex25
      *APA00452: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.2   Jan 17 2002 10:35:28   t13ex43
      *APA00167 : DRPA1514-VERTR-BEGINN hinzu
      *
      *   Rev 2.1   Nov 30 2001 08:54:38   t13ex43
      *APA00167 : PIN prüfen Sonstige Banken Österreich
      *
      *   Rev 2.0   04 Jul 2001 11:10:56   p13sp13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1514 FUNKTION PIN PRÜFEN FÜR             *
      *                                   SONSTIGE BANKEN ÖSTERREICH *
      *    --------------------------------------------------------- *
      *    ERSTELLT: KLEEMEIER                                       *
      *    DATUM:    28.11.2001                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00167!28.11.01!2.1 ! NEU                       ! KLEEMEIER*
      * APA00167!17.01.02!2.2 ! VERTR-BEGINN HINZU        ! KLEEMEIER*
      * APA00452!02.11.06!2.3 ! Umstellung 3-st. Produkte ! Jäger    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1514                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1514.
             10 PI-DRPA1514-ZVP-VERSION     PIC S9(004) BINARY.
             10 PI-DRPA1514-PGEB-GES        PIC S9(004) BINARY.
             10 PI-DRPA1514-PIN             PIC S9(009) BINARY.
021106*      10 PI-DRPA1514-PROD-SCHL       PIC S9(004) BINARY.
021106       10 PI-DRPA1514-PROD-PROD-SCHL  PIC S9(004) BINARY.
             10 PI-DRPA1514-VERTR-BEGINN    PIC  X(010).

       01 PO-DRPA1514.
          05 PO-DRPA1514-RC                 PIC  X(003).
             88 PO-DRPA1514-RC-OK                VALUE 'OK '.
             88 PO-DRPA1514-RC-VSTAT-GEFUELLT    VALUE 'VSG'.
      ******** ENDE COPY-MEMBER **************************************
