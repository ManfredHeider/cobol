      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1395.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRPA1395.CPv  $
      *
      *   Rev 2.6   Jun 19 2012 09:30:34   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.5   May 21 2012 08:55:22   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.4   Dec 10 2007 12:59:04   t13ex21
      *APA00622: Programmänderungen Martina
      *
      *   Rev 2.3   Jun 16 2003 10:58:38   t13ae46
      *FPA00598: Neues Feld 
      *
      *   Rev 2.2   07 Sep 2000 14:52:36   T13EX23
      *APA00096 - Neues Feld im PI-Bereich - KNR-SPLIT-ART
      *
      *   Rev 2.1   12 Jul 2000 12:02:24   T13EX21
      * 
      *
      *   Rev 2.0   Apr 11 2000 16:58:10   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1395                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA1395                    *
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
      * APA00096!14.08.00!2.2 ! NEUES FELD PI-KNR-SPLIT-ART GRAY     *
      * FPA00598!13.06.03!2.3 ! NEUES FELD                ! WANG     *
      * APA00622!10.09.07!2.4 ! PI-ERWEITERUNG            ! STORCH   *
      * APA01101!23.02.12!2.5 ! VGES DURCH MANDANT ERSETZT! STORCH   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA1395
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT-ALL  <<<
      *
      ****************************************************************
      *
       01  PI-DRPA1395.
           05  PI-DRPA1395-PROGRAMM           PIC X(008).
           05  PI-DRPA1395-UFKT               PIC X(002).
      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST NEXT, 3=QUIT
           05  PI-DRPA1395-PGEB-GES           PIC S9(004) COMP.
           05  PI-DRPA1395-PDTA-VSTAT         PIC X(004).
           05  PI-DRPA1395-PDTA-VSTAT1        PIC X(004).
           05  PI-DRPA1395-KNR-DTA-ART        PIC S9(004) COMP.
           05  PI-DRPA1395-KNR-SPLIT-ART      PIC S9(004) COMP.
           05  PI-DRPA1395-MANDANT            PIC  9(02).
           05  PI-DRPA1395-S-MANDANT          PIC  X(01).
      *
       01  PO-DRPA1395.
           05  PO-DRPA1395-RC                 PIC X(003).
               88 PO-DRPA1395-RC-OK               VALUE 'OK '.
               88 PO-DRPA1395-RC-EOF              VALUE 'EOF'.
               88 PO-DRPA1395-RC-LEER             VALUE 'LER'.
      *
           05  PO-DRPA1395-DTA-ID             PIC S9(015)V COMP-3.
           05  PO-DRPA1395-DTA-KONTR-WERT     PIC S9(11)V9(2) COMP-3.
           05  PO-DRPA1395-DTA-VSTAT          PIC X(004).
      *
      ******* ENDE COPY-MEMBER **************************************
