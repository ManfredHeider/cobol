      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1394.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRPA1394.CPv  $
      *
      *   Rev 2.6   Mar 12 2014 13:09:08   t13ae46
      *FPA01350: Überlauf von PDTA-TAB auf 500 erhöhen
      *
      *   Rev 2.5   Jun 19 2012 09:30:26   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.4   May 21 2012 08:55:14   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.3   Dec 10 2007 12:54:22   t13ex21
      *APA00622: Programmänderungen Martina
      *
      *   Rev 2.2   04 Sep 2000 11:25:32   T13EX23
      *APA00096 - Anzahl DTAs auf 50 erhöhen
      *
      *   Rev 2.1   12 Jul 2000 12:02:22   T13EX21
      *APA00096 - Neuerstellung
      *
      *   Rev 2.0   Apr 11 2000 16:58:08   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1394                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA1394                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: STORCH                                           *
      *    DATUM:    15.06.2000                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00096!15.06.00!2.1 ! NEU                       ! STORCH   *
      * APA00096!04.09.00!2.2 ! ANZAHL DTAS AUF 50 ERHÖHEN! GRAY     *
      * APA00622!10.09.07!2.2 ! PI-ERWEITERUNG            ! STORCH   *
      * APA01101!27.03.12!2.3 ! VGES DURCH MANDANT ERSETZT! STORCH   *
      * FPA01350!12.03.14!2.4 ! TAB AUF 500 ERHÖHEN       ! WANG     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA1394
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DTA-DATEN FÜR DIALOG "INFOCENTER" ERMITTELN  <<<
      *
      ****************************************************************
      *
       01  PI-DRPA1394.
           05  PI-DRPA1394-PGEB-GES           PIC S9(004) COMP.
           05 PI-DRPA1394-KNR-MANDANT         PIC  9(02).
           05 PI-DRPA1394-S-KNR-MANDANT       PIC  X(01).
      *
       01  PO-DRPA1394.
           05  PO-DRPA1394-RC                 PIC X(003).
               88 PO-DRPA1394-RC-OK               VALUE 'OK '.
               88 PO-DRPA1394-RC-NFD              VALUE 'NFD'.
      *
           05  PO-DRPA1394-PG-DTA-ANZ         PIC S9(004) BINARY.
           05  PO-DRPA1394-PG-DTA-ANZ-O       PIC S9(004) BINARY.
           05  PO-DRPA1394-MAN-DTA-ANZ        PIC S9(004) BINARY.
           05  PO-DRPA1394-MAN-DTA-ANZ-O      PIC S9(004) BINARY.
           05  PO-DRPA1394-SUM-PROV-BETR      PIC S9(011)V9(2) COMP-3.
120314*    05  PO-DRPA1394-PDTA-VSTAT-TAB     OCCURS 50.
120314     05  PO-DRPA1394-PDTA-VSTAT-TAB     OCCURS 500.
               10 PO-DRPA1394-DTA-VSTAT          PIC X(004).
      *
      ******* ENDE COPY-MEMBER **************************************
