      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0807.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRPA0807.CPv  $
      *
      *   Rev 2.4   Jan 29 2015 10:46:18   t13ex44
      *APA01963: Ges 20- Änderung der Basistabelle und Vorbewegungs
      *ermittlung
      *
      *   Rev 2.3   Jan 10 2014 13:59:08   t13ex72
      *APA01542: Erstellung Gevo Neutral
      *
      *   Rev 2.2   16 May 2007 06:44:36   t13ex44
      *APA00604: Anpassungen in Daisy zur Ablösung PEW
      *
      *   Rev 2.1   25 May 2000 11:17:16   T13EX21
      *
      *
      *   Rev 2.0   May 27 1999 12:05:26   T01JM2S
      *APA00042
      *
      *   Rev 1.0   14 Jan 1998 12:43:22   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0807                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<PVW GEVO NEUTR. MELD>>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    26.01.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!26.01.98! 01 ! NEU                       ! WOLFF    *
      * APA00012!07.12.98! 02 ! BEARBEITUNGSMODUS ENTFERNT! MUND     *
      * APA00090!25.05.00! 2.1! PI-DRPA0807-PIN-VNR-AKZ   ! STORCH   *
      *         !        !    ! EINGEFÜGT                 !          *
      * APA00604!12.04.07! 2.2! ABSCHALTUNG PEW           ! SCHEIDEL *
      * APA01542!25.03.12!2.18! KALYPSO                   ! WEINMANN *
      * APA01963!05.01.15! 2.4! AML-VORBEW.-ERM           ! SCHEIDEL *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0807                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0807.
          05 PI-DRPA0807-DATEN.
             10 PI-DRPA0807-PVER-ZUSTAND            PIC 9(002).
             10 PI-DRPA0807-ANTRAG-ZUSTAND          PIC 9(002).
             10 PI-DRPA0807-PIN-VNR-AKZ             PIC S9(004) BINARY.
             10 PI-DRPA0807-KNR-MANDANT             PIC S9(004) BINARY.
      *
 
       01 PO-DRPA0807.
          05 PO-DRPA0807-RC                         PIC  X(003).
             88 PO-DRPA0807-RC-OK                   VALUE 'OK '.
             88 PO-DRPA0807-RC-MELDUNGSFEHLER       VALUE 'MFE'.
          05 PO-DRPA0807-DATEN.
             10 PO-DRPA0807-VNR-ALT                 PIC  X(015).
             10 PO-DRPA0807-PVER-ZUSTAND            PIC  9(002).
050115       10 PO-DRPA0807-STRUK-KDBETR            PIC  X(001).
050115       10 PO-DRPA0807-KD-BETR                 PIC  9(007).
      ******* ENDE COPY-MEMBER **************************************

