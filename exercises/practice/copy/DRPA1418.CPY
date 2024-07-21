      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1418.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRPA1418.CPv  $
      *
      *   Rev 2.4   Jun 19 2012 09:30:50   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.3   May 21 2012 08:55:36   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.2   Dec 10 2007 13:08:14   t13ex21
      *APA00622: Programmänderungen Martina
      *
      *   Rev 2.1   07 Sep 2000 14:56:48   T13EX23
      *APA00096 - Neuerstellung
      *
      *   Rev 2.0   12 Jul 2000 12:32:46   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1418                                     *
      *    --------------------------------------------------------  *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA1418 DES BUFFER-OZP'S  *
      *    ZYPA0036                                                  *
      *    ( ERGEBNISMENGE BEREITSTELLEN                             *
      *      BISHER BENUTZT VON PGPA0051 IM DIALOG PA02              *
      *      DAISY GESELLSCHAFSTABHÄNGIGE DATEN ERMITTELN)           *
      *    --------------------------------------------------------  *
      *    ERSTELLT: GRAY                                            *
      *    DATUM:    08.08.2000                                      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00096!08.08.98!1.1 ! NEU                       ! GRAY     *
      * APA00622!10.09.07!2.2 ! PI-ERWEITERUNG            ! STORCH   *
      * APA01101!27.02.12!2.3 ! VGES DURCH MANDANT ERSETZT! STORCH   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA1418
      *    AUSSER: DEM FUNKTIONSNAMEN ( IN DRPA1417 ),
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION DRPA1418 DES BUFFER-OZP ZYPA0036 ERMITTELT
      *    DAISY GESELLSCHAFSTABHÄNGIGE DATEN FÜR PA02, DAISY
      *    INFOCENTER
      *
      ****************************************************************

       01  PI-DRPA1418.
           05  PI-DRPA1418-UFKT                    PIC  9(002).
           05  PI-DRPA1418-NAME-ZW-SPEICHER        PIC  X(008).
           05  PI-DRPA1418-LOG-SATZ-NR             PIC  9(005).
           05  PI-DRPA1418-AUSWAHL-KRITERIEN.
               10 PI-DRPA1418-KNR-MANDANT          PIC  9(02).
               10 PI-DRPA1418-S-MANDANT            PIC  X(01).

       01  PO-DRPA1418.
           05  PO-DRPA1418-RC                      PIC  X(03).
               88 PO-DRPA1418-RC-OK                     VALUE 'OK '.
               88 PO-DRPA1418-RC-EOF                    VALUE 'EOF'.
               88 PO-DRPA1418-RC-LEER                   VALUE 'LER'.
           05  PO-DRPA1418-ANZAHL-ERG-ZEILEN       PIC  9(05).
           05  PO-DRPA1418-LOG-SATZ-NR-1           PIC  9(05).

           05  PO-DRPA1418-ERGEBNISZEILE.
               COPY DRPA1419.
      *
      ******* ENDE COPY-MEMBER **************************************
