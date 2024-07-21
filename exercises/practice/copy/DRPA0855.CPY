      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0855.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA0855.CPv  $
      *
      *   Rev 2.1   Jan 18 2000 14:37:32   T13EX22
      *APA00071: SF44 mit TBPA0026-Zugriff
      *
      *   Rev 2.0   Feb 27 1999 14:29:42   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   14 Jan 1998 12:46:46   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0855                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA0855                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: MUND                                             *
      *    DATUM:    22.11.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00071!22.11.99! 01 ! NEU                       ! MUND     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA0855
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION DRPA0855 DES BUFFER-OZP ZYPA0033 ERMITTELT
      *    DIE ERGEBNISMENGE FÜR DIALOG SF44 'PGV/ DAISY-PGV ZUORDNUNG'
      *
      ****************************************************************
      *
       01  PI-DRPA0855.
           05  PI-DRPA0855-UFKT                    PIC  9(002).
           05  PI-DRPA0855-NAME-ZW-SPEICHER        PIC  X(008).
           05  PI-DRPA0855-LOG-SATZ-NR             PIC  9(005).
           05  PI-DRPA0855-AUSWAHL-KRITERIEN.
               10 PI-DRPA0855-PGEB-GES             PIC S9(004)  BINARY.
               10 PI-DRPA0855-PGVZ-PGV             PIC  X(008).
               10 PI-DRPA0855-KNR-PGV              PIC S9(004)  BINARY.
               10 PI-DRPA0855-S-PGVZ-PGV           PIC  X(001).
               10 PI-DRPA0855-S-DAISY-PGV          PIC  X(001).
               10 PI-DRPA0855-S-SORT-DAISY-PGV     PIC  X(001).
      *
       01  PO-DRPA0855.
           05  PO-DRPA0855-RC                      PIC  X(03).
               88 PO-DRPA0855-RC-OK                     VALUE 'OK '.
               88 PO-DRPA0855-RC-EOF                    VALUE 'EOF'.
               88 PO-DRPA0855-RC-LEER                   VALUE 'LER'.

           05  PO-DRPA0855-ANZAHL-ERG-ZEILEN       PIC  9(05).

           05  PO-DRPA0855-ERGEBNISZEILE.
               COPY DRPA0955.
      *
      ********** ENDE COPY-MEMBER **************************************
