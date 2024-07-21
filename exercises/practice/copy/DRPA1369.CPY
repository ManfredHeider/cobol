      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1369.CPY  $
      *    $Revision:   2.9  $
      *
      *    $Log:   L:/mf/CPY/DRPA1369.CPv  $
      *
      *   Rev 2.9   Apr 10 2018 10:34:38   t13ex44
      *FPA01632: Verwendeter steuerl. Status ist falsch
      *
      *   Rev 2.8   Feb 03 2016 10:51:46   t13ex24
      *APA02169: Ges. 50 - Central neue PGVs zum Mein Gesundheitspl
      *an- KBP PEG
      *
      *   Rev 2.7   Jun 19 2012 09:30:10   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.6   May 21 2012 08:54:52   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.5   Mar 30 2012 12:45:30   t13ae37
      *APA01189: vertriebsbezogene Vorbewegungsermittlung bei Belas
      *tungen
      *
      *   Rev 2.4   Jun 14 2011 13:32:06   t13ae37
      *APA01189: vertriebsbezogene Vorbewegungsermittlung bei Belas
      *tungen
      *
      *   Rev 2.3   16 May 2007 06:45:26   t13ex44
      *APA00604: Anpassungen in Daisy zur Ablösung PEW
      *
      *   Rev 2.2   10 Jun 2000 08:39:22   T13EX22
      *APA00090: PINZ-VNR-AKZ ergänzt
      *
      *   Rev 2.1   Apr 28 2000 08:49:58   T13EX22
      *APA00090: Ergänzungen für GEVO Gutschrift
      *
      *   Rev 2.0   Mar 01 2000 09:58:18   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1369                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<PVW GEVO GUTSCHRIFT  >>                  *
      *    --------------------------------------------------------   *
      *    ERSTELLT: MUND                                             *
      *    DATUM:    10.04.2000                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00090!10.04.00! 01 ! NEU                       ! MUND     *
      * APA00090!10.06.00! 1.1! PIN-VNR-AKZ ERGÄNZT       ! MUND     *
      * APA00604!12.04.07! 2.3! ABSCHALTUNG PEW           ! SCHEIDEL *
      * APA01189!14.06.11! 2.4! VERTRIEBSBEZ. VORBEWEGUNG ! BUCH     *
      * APA01101!23.04.12! 2.6! VGES DURCH MANDANT ERSETZT! STORCH   *
      * APA02169!29.01.16! 2.7! NEUE PO-PARAMETER         ! KÖPRÜCÜ  *
      * FPA01632!27.03.18! 2.9! TABELLE DER STEUERSTATUS  ! SCHEIDEL *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1369                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1369.
          05 PI-DRPA1369-DATEN.
             10 PI-DRPA1369-PVER-ZUSTAND       PIC 9(002).
             10 PI-DRPA1369-ANTRAG-ZUSTAND     PIC 9(002).
             10 PI-DRPA1369-PIN-VNR-AKZ        PIC S9(004) BINARY.
             10 PI-DRPA1369-KNR-MANDANT        PIC S9(004) BINARY.
      *
       01 PO-DRPA1369.
          05 PO-DRPA1369-RC                    PIC  X(003).
             88 PO-DRPA1369-RC-OK                    VALUE 'OK '.
270318       88 PO-DRPA1369-RC-RNR                   VALUE 'RNR'.
             88 PO-DRPA1369-RC-MELDUNGSFEHLER        VALUE 'MFE'.
 
          05 PO-DRPA1369-DATEN.
             10 PO-DRPA1369-VNR-ALT            PIC  X(015).
             10 PO-DRPA1369-PVER-ZUSTAND       PIC  9(002).
290116       10 PO-DRPA1369-STRUK-KDBETR       PIC  X(001).
290116       10 PO-DRPA1369-KD-BETR            PIC  9(007).
270318    05 PO-DRPA1369-STEUSTAT.
270318       COPY DRPA1843.
 

