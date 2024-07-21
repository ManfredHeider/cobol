      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1084.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1084.CPv  $
      *
      *   Rev 2.1   04 Oct 1999 15:07:14   T01JM2S
      *APA00057 - EINBAU MODUS-PROTOKOLL IN PK10
      *
      *   Rev 2.0   May 27 1999 12:01:52   T01JM2S
      *APA00042
      *
      *   Rev 1.0   20 Jul 1998 10:21:00   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1084                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<BUFFER PK10/PGPA0038>>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    06.10.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!06.10.98! 01 ! NEU                       ! WOLFF    *
      *         !        !    !                           !          *
      * FPA00057!01.10.99! 2.1! ERWEITERUNG PK10 UM       ! WOLFF    *
      *         !        !    ! MODUS-PROTOKOLL           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA1084
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION DRPA1084 DES BUFFER-OZP ZYPA0024 ERMITTELT
      *    DIE ERGEBNISMENGE FÜR PK10
      *
      ****************************************************************
      *
       01  PI-DRPA1084.
           05  PI-DRPA1084-UFKT                     PIC  9(002).
           05  PI-DRPA1084-NAME-ZW-SPEICHER         PIC  X(008).
           05  PI-DRPA1084-LOG-SATZ-NR              PIC  9(005).
           05  PI-DRPA1084-AUSWAHL-KRITERIEN.
               10 PI-DRPA1084-PROT-ART              PIC  9(003).
               10 PI-DRPA1084-S-PROT-ART            PIC  X(001).
               10 PI-DRPA1084-PROT-OBJ              PIC  9(003).
               10 PI-DRPA1084-S-PROT-OBJ            PIC  X(001).
               10 PI-DRPA1084-PROT-AUSPR            PIC  9(003).
               10 PI-DRPA1084-S-PROT-AUSPR          PIC  X(001).
               10 PI-DRPA1084-ERSTELL-DAT-AB        PIC  X(010).
               10 PI-DRPA1084-ERSTELL-DAT-BIS       PIC  X(010).
011099         10 PI-DRPA1084-PROT-PTR-ID           PIC S9(015)
011099                                                  PACKED-DECIMAL.
011099         10 PI-DRPA1084-S-PROT-PTR-ID         PIC  X(001).
      *
       01  PO-DRPA1084.
           05  PO-DRPA1084-RC                       PIC  X(003).
               88 PO-DRPA1084-RC-OK                       VALUE 'OK '.
               88 PO-DRPA1084-RC-EOF                      VALUE 'EOF'.
               88 PO-DRPA1084-RC-LEER                     VALUE 'LER'.
           05  PO-DRPA1084-ANZAHL-ERG-ZEILEN        PIC  9(005).

           05  PO-DRPA1084-ERGEBNISZEILE.
               COPY DRPA1091.
      *
      ******* ENDE COPY-MEMBER **************************************
