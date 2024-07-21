      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1410.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1410.CPv  $
      *
      *   Rev 2.1   21 Sep 2000 16:06:46   T13EX21
      *APA00106 - Neuerstellung
      *
      *   Rev 2.0   12 Jul 2000 12:32:36   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1410                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA1410                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: STORCH                                           *
      *    DATUM:    24.07.2000                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00106!24.07.00! 01 ! NEU                       ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA1410
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION DRPA1410 DES BUFFER-OZP ZYPA0035 ERMITTELT
      *    DIE ERGEBNISMENGE FÜR PO14, AUSKUNFT PROVISIONSNUMMERN
      *    (UNTERER MASKENTEIL: PIN'S)
      ****************************************************************
      *
       01  PI-DRPA1410.
           05  PI-DRPA1410-UFKT                    PIC  9(02).
           05  PI-DRPA1410-NAME-ZW-SPEICHER        PIC  X(008).
           05  PI-DRPA1410-LOG-SATZ-NR             PIC  9(005).
           05  PI-DRPA1410-PRINTE                  PIC S9(015) COMP-3.

           05 PI-DRPA1410-AUSWAHL-KRITERIEN.
               10 PI-DRPA1410-PGEB-GES             PIC 9(002).
      *
       01  PO-DRPA1410.
           05  PO-DRPA1410-RC                      PIC  X(03).
               88 PO-DRPA1410-RC-OK                        VALUE 'OK '.
               88 PO-DRPA1410-RC-EOF                       VALUE 'EOF'.
               88 PO-DRPA1410-RC-LEER                      VALUE 'LER'.
           05  PO-DRPA1410-ANZAHL-ERG-ZEILEN       PIC  9(05).
           05  PO-DRPA1410-LOG-SATZ-NR-1           PIC  9(05).

           05  PO-DRPA1410-ERGEBNISZEILE.
               COPY DRPA1411.
      *
      ******* ENDE COPY-MEMBER **************************************
