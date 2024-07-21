      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1515.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRPA1515.CPv  $
      *
      *   Rev 2.5   Nov 18 2008 13:18:58   t13ae37
      *APA00828: Anpassung Provisionsschnittstelle Ges 45 
      *
      *   Rev 2.4   Jun 10 2008 10:34:26   t13ex21
      *APA00712: FVD Stufe 1: Tabellenerweiterungen
      *
      *   Rev 2.3   Jun 05 2002 16:35:02   t13ex44
      *Anpassung Summenfelder
      *
      *   Rev 2.2   May 15 2002 13:43:08   t13ex44
      *Korrektur Laenge der Felder Provision, provisionspflichtige 
      *Summe, Anlagebetrag
      *
      *   Rev 2.1   Dec 04 2001 10:37:30   t13ex25
      *APA00167; Anpassungen für Bank Austria Invest
      *
      *   Rev 2.0   04 Jul 2001 11:10:58   p13sp13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1515 FUNKTION BANK AUSTRIA INVEST        *
      *                 FORMAL PRÜFEN UND NORMIEREN                  *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Jäger-Sokoluk                                   *
      *    DATUM:    30.11.2001                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00167!30.11.01!2.1 ! NEU                       ! JÄGER    *
      * APA00712!08.04.08!2.2 ! PI-ERWEITERUNG UM MANDANT ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1515                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1515.
          05 PI-DRPA1515-PZVP-VERSION       PIC S9(004) BINARY.
          05 PI-DRPA1515-PEG-DATEN-AUST-INV.
             COPY DRPA1516.
080408    05 PI-DRPA1515-KNR-MANDANT        PIC S9(004) BINARY.
      *
       01 PO-DRPA1515.
          05 PO-DRPA1515-RC                 PIC  X(003).
             88 PO-DRPA1515-RC-OK                VALUE 'OK '.
             88 PO-DRPA1515-RC-VSTAT-GEFUELLT    VALUE 'VSG'.
      ******** ENDE COPY-MEMBER **************************************
