      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.02.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1724.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRPA1724.CPv  $
      *
      *   Rev 2.6   Feb 03 2020 12:45:14   t13ae37
      *FPA01759: Ges. 16/23 
      *
      *   Rev 2.5   Jan 10 2014 13:22:38   t13ex72
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): GeVo Leben (CBO)
      *
      *   Rev 2.4   Jun 19 2012 09:31:58   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.3   May 21 2012 08:56:54   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.2   Mar 30 2012 12:45:54   t13ae37
      *APA01189: vertriebsbezogene Vorbewegungsermittlung bei Belas
      *tungen
      *
      *   Rev 2.1   Mar 07 2011 10:59:06   t13ae37
      *APA01171: I191-ausgelagerte Bewegungungen heranziehen
      *
      *   Rev 2.0   Feb 14 2011 13:18:46   t13ae37
      *Archiverstellung
      *
      *****************************************************************
      *    COPY-MEMBER: DRPA1724                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA1724                    *
      *    DATENÜBERGABE ZUR ERMITTLUNG DES PROVISIONSSALDO AUS DER   *
      *    HISTORIE.                                                  *
      *    --------------------------------------------------------   *
      *    ERSTELLT: BUCH                                             *
      *    DATUM:    14.02.2011                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA01171!14.02.11!2.1 ! NEU                       ! BUCH     *
      * APA01101!10.04.12!2.2 ! VGES DURCH MANDANT ERSETZT! STORCH   *
      * APA01526!13.12.13!2.5 ! KALYPSO                   ! BÖTTCHER *
      * FPA01759!29.01.20!2.6 ! ERSTPRODUKTE MÖGLICH      ! BUCH     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA1724
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRPA1724.
           05  PI-DRPA1724-PRINTE              PIC S9(015)V COMP-3.
           05  PI-DRPA1724-PGEB-GES            PIC S9(004) BINARY.
           05  PI-DRPA1724-PROD-PROD-SCHL      PIC S9(004) BINARY.
230312     05  PI-DRPA1724-KNR-MANDANT         PIC S9(004) BINARY.
131213     05  PI-DRPA1724-UNR                 PIC S9(004) BINARY.
131213     05  PI-DRPA1724-SCHICHT-NR          PIC S9(004) BINARY.
 
290120     05  PI-DRPA1724-ERST-PRODUKTE.
290120        10 PI-DRPA1724-ERST-PROD-SCHL     PIC S9(004)  BINARY
290120                          OCCURS 10  INDEXED BY 1724-IX-EPS.
      *
       01  PO-DRPA1724.
           05  PO-DRPA1724-RC                  PIC  X(003).
               88 PO-DRPA1724-RC-OK            VALUE 'OK '.
               88 PO-DRPA1724-RC-NOT-FOUND     VALUE 'NFD'.
      *
           05  PO-DRPA1724-PROVISION           PIC S9(9)V9(2) COMP-3.
      *
      ******* ENDE COPY-MEMBER **************************************

