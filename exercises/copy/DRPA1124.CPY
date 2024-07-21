      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1124.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA1124.CPv  $
      *
      *   Rev 2.0   May 28 1999 13:35:00   T01JM2E
      *APA00042 NT-Umstellung
      *
      *   Rev 1.0   21 Oct 1998 13:58:30   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1124                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA1124                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: D.WILHELM                                        *
      *    DATUM:    11.11.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!11.11.98! 01 ! NEU                       ! D.WILHELM*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA1124
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION DRPA1124 DES BUFFER-OZP ZYPA0029 ERMITTELT
      *    DIE ERGEBNISMENGE FüR PO16, AUSKUNFT EVS
      *
      ****************************************************************
      *
       01  PI-DRPA1124.
           05  PI-DRPA1124-UFKT                    PIC  9(02).
           05  PI-DRPA1124-NAME-ZW-SPEICHER        PIC  X(008).
           05  PI-DRPA1124-LOG-SATZ-NR             PIC  9(005).
           05  PI-DRPA1124-PRINTE                  PIC S9(015) COMP-3.

           05 PI-DRPA1124-AUSWAHL-KRITERIEN.
               10 PI-DRPA1124-PGEB-GES             PIC 9(002).
               10 PI-DRPA1124-VNR                  PIC X(015).
      *
       01  PO-DRPA1124.
           05  PO-DRPA1124-RC                      PIC  X(03).
               88 PO-DRPA1124-RC-OK                        VALUE 'OK '.
               88 PO-DRPA1124-RC-EOF                       VALUE 'EOF'.
               88 PO-DRPA1124-RC-LEER                      VALUE 'LER'.
           05  PO-DRPA1124-ANZAHL-ERG-ZEILEN       PIC  9(05).
           05  PO-DRPA1124-LOG-SATZ-NR-1           PIC  9(05).

           05  PO-DRPA1124-ERGEBNISZEILE.
               COPY DRPA1140.
      *
      ******* ENDE COPY-MEMBER **************************************
