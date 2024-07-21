      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1416.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRPA1416.CPv  $
      *
      *   Rev 2.4   Jun 19 2012 09:30:42   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.3   May 21 2012 08:55:30   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.2   Dec 10 2007 13:03:38   t13ex21
      *APA00622: Programmänderungen Martina
      *
      *   Rev 2.1   07 Sep 2000 14:56:56   T13EX23
      *APA00096 - Neuerstellung
      *
      *   Rev 2.0   12 Jul 2000 12:32:44   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1416                                      *
      *    --------------------------------------------------------   *
      *    UEBERGABEBEREICH FUER PROGRAMM UPPA0093                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: GRAY                                             *
      *    DATUM:    08.08.2000                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * APA00096!08.08.00! 2.1 ! NEUERSTELLUNG            ! GRAY     *
      * APA00622!10.09.07! 2.2 ! PI-ERWEITERUNG           ! STORCH   *
      * APA01101!27.02.12! 2.3 ! VGES DURCH MANDANT ERSETZT! STORCH  *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT NUR DIE AUSGABEPARAMETER              *
      *    FÜR DIE DATENUEBERGABE VON UPPA0093 NACH DIALOG           *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01  PI-DRPA1416.
           05 PI-DRPA1416-UFKT                  PIC  9(002).
           05  PI-DRPA1416-AUSWAHL-KRITERIEN.
               10 PI-DRPA1416-MANDANT           PIC  9(02).
               10 PI-DRPA1416-S-MANDANT         PIC  X(01).
       01  PO-DRPA1416.
           05  PO-DRPA1416-RC                   PIC  X(003).
               88 PO-DRPA1416-RC-OK                  VALUE 'OK '.
               88 PO-DRPA1416-RC-LEER                VALUE 'LER'.
               88 PO-DRPA1416-RC-EOF                 VALUE 'EOF'.
           05  PO-DRPA1416-ERGEBNISZEILE.
               COPY DRPA1419.
      *
      ******* ENDE COPY-MEMBER **************************************
