      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1072.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1072.CPv  $
      *
      *   Rev 2.1   Dec 20 2013 09:26:06   t13ae37
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): SST-Abrechnung
      *
      *   Rev 2.0   May 28 1999 13:28:18   T01JM2E
      *APA00042 NT-Umstellung
      *
      *   Rev 1.0   20 Jul 1998 10:20:04   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1072                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF:  ARBEITSSATZPAKET ERMITTELN               *
      *    --------------------------------------------------------   *
      *    ERSTELLT: D.WILHELM                                        *
      *    DATUM:    17.09.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!17.09.98! 01 ! NEU                       !D.WILHELM *
      * APA01526!05.07.13!2.1 ! KNR-BEZ-KURZ HINZU        !BUCH      *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1072                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1072.
          03 PI-DRPA1072-UFKT-CODE               PIC  9(002).
          03 PI-DRPA1072-PROGRAMM                PIC  X(008).
          03 EIN-SATZ.
             COPY DRPA1070.
          03 PI-DRPA1072-BEW-HRKFT       OCCURS 20.
             05 PI-DRPA1072-BEW-HRKFT-BEZ        PIC  X(001).
      *
       01 PO-DRPA1072.
          03 PO-DRPA1072-RC                      PIC  X(003).
             88 PO-DRPA1072-RC-OK                     VALUE 'OK '.
             88 PO-DRPA1072-RC-NOT-FOUND              VALUE 'NFD'.
             88 PO-DRPA1072-RC-EOF                    VALUE 'EOF'.
          03 AUS-SATZ.
             COPY DRPA1069.
      *
      ******* ENDE COPY-MEMBER DRPA1072 *****************************
