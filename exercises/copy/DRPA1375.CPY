      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1375.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRPA1375.CPv  $
      *
      *   Rev 2.6   Feb 03 2020 12:44:42   t13ae37
      *FPA01759: Ges. 16/23 
      *
      *   Rev 2.5   Jan 10 2014 13:13:50   t13ex72
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): GeVo Leben (CBO)
      *
      *   Rev 2.4   Jun 19 2012 09:30:18   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.3   May 21 2012 08:55:08   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.2   Mar 30 2012 12:45:48   t13ae37
      *APA01189: vertriebsbezogene Vorbewegungsermittlung bei Belas
      *tungen
      *
      *   Rev 2.1   May 04 2000 10:22:58   T13EX21
      *
      *
      *   Rev 2.0   Apr 11 2000 16:57:54   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1375                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA1375                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: STORCH                                           *
      *    DATUM:    02.05.2000                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00090!02.05.00!2.1 ! NEU                       ! STORCH   *
      * APA01101!10.04.12!2.2 ! VGES DURCH MANDANT ERSETZT! STORCH   *
      * APA01526!13.12.13!2.5 ! KALYPSO                   ! BÖTTCHER *
      * FPA01759!29.01.20!2.6 ! ERSTPRODUKTE MÖGLICH      ! BUCH     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA1375
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRPA1375.
           05  PI-DRPA1375-PRINTE              PIC S9(015)V COMP-3.
           05  PI-DRPA1375-PGEB-GES            PIC S9(004) BINARY.
           05  PI-DRPA1375-PROD-PROD-SCHL      PIC S9(004) BINARY.
230312     05  PI-DRPA1375-KNR-MANDANT         PIC S9(004) BINARY.
131213     05  PI-DRPA1375-UNR                 PIC S9(004) BINARY.
131213     05  PI-DRPA1375-SCHICHT-NR          PIC S9(004) BINARY.
 
290120     05  PI-DRPA1375-ERST-PRODUKTE.
290120        10 PI-DRPA1375-ERST-PROD-SCHL     PIC S9(004)  BINARY
290120                         OCCURS 10  INDEXED BY 1375-IX-EPS.
      *
       01  PO-DRPA1375.
           05  PO-DRPA1375-RC                  PIC  X(003).
               88 PO-DRPA1375-RC-OK            VALUE 'OK '.
               88 PO-DRPA1375-RC-NOT-FOUND     VALUE 'NFD'.
      *
           05  PO-DRPA1375-PROVISION           PIC S9(9)V9(2) COMP-3.
      *
      ******* ENDE COPY-MEMBER **************************************

