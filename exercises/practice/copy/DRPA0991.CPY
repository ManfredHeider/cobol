      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0991.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA0991.CPv  $
      *
      *   Rev 2.1   May 27 1999 10:24:12   T01JM2E
      *NT-Umstellung
      *
      *   Rev 1.0   01 Apr 1998 14:21:22   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0991                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA0991                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: D.WILHELM                                        *
      *    DATUM:    03.07.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!03.07.98! 01 ! NEU                       ! D.WILHELM*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA0991
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION DRPA0991 DES BUFFER-OZP ZYPA0017 ERMITTELT
      *    DIE ERGEBNISMENGE FüR PO14, AUSKUNFT PROVISIONSBEWEGUNGEN
      *
      ****************************************************************
      *
       01  PI-DRPA0991.
           05  PI-DRPA0991-UFKT                    PIC  9(02).
           05  PI-DRPA0991-NAME-ZW-SPEICHER        PIC  X(008).
           05  PI-DRPA0991-LOG-SATZ-NR             PIC  9(005).
           05  PI-DRPA0991-PRINTE                  PIC S9(015) COMP-3.

           05 PI-DRPA0991-AUSWAHL-KRITERIEN.
               10 PI-DRPA0991-PGEB-GES             PIC 9(002).
               10 PI-DRPA0991-VNR                  PIC X(015).


      *
       01  PO-DRPA0991.
           05  PO-DRPA0991-RC                      PIC  X(03).
               88 PO-DRPA0991-RC-OK                        VALUE 'OK '.
               88 PO-DRPA0991-RC-EOF                       VALUE 'EOF'.
               88 PO-DRPA0991-RC-LEER                      VALUE 'LER'.
           05  PO-DRPA0991-ANZAHL-ERG-ZEILEN       PIC  9(05).
           05  PO-DRPA0991-LOG-SATZ-NR-1           PIC  9(05).

           05  PO-DRPA0991-ERGEBNISZEILE.
               COPY DRPA0987.
      *
      ******* ENDE COPY-MEMBER **************************************
