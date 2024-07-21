      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0351.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0351.CPv  $
      *
      *   Rev 2.0   May 26 1999 17:20:32   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   May 26 1997 15:22:38   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0351                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA0351                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SCHRIEBER                                        *
      *    DATUM:    03.06.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !03.06.97! 01 ! NEU                       ! SCHRIEBER*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA0351
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION DRPA0351 DES BUFFER-OZP ZYPA0002 ERMITTELT
      *    DIE ERGEBNISMENGE FüR PA15, ANTRäGE NACH FEHLERCODE.
      *
      ****************************************************************
      *
       01  PI-DRPA0351.
           05  PI-DRPA0351-UFKT                PIC  9(02).
           05  PI-DRPA0351-NAME-ZW-SPEICHER    PIC  X(08).
           05  PI-DRPA0351-LOG-SATZ-NR         PIC  9(05).
           05  PI-DRPA0351-AUSWAHL-KRITERIEN.
               10 PI-DRPA0351-PGEB-GES         PIC  9(02).
               10 PI-DRPA0351-S-PGEB-GES       PIC  X(01).
               10 PI-DRPA0351-DTA-ART          PIC  9(03).
               10 PI-DRPA0351-S-DTA-ART        PIC  X(01).
               10 PI-DRPA0351-VSTAT            PIC  X(04).
               10 PI-DRPA0351-S-VSTAT          PIC  X(01).
               10 PI-DRPA0351-UEBDAT-VON       PIC  X(10).
               10 PI-DRPA0351-S-UEBDAT-VON     PIC  X(01).
               10 PI-DRPA0351-UEBDAT-BIS       PIC  X(10).
               10 PI-DRPA0351-S-UEBDAT-BIS     PIC  X(01).
      *
       01  PO-DRPA0351.
           05  PO-DRPA0351-RC                  PIC  X(03).
               88 PO-DRPA0351-RC-OK                        VALUE 'OK '.
               88 PO-DRPA0351-RC-EOF                       VALUE 'EOF'.
               88 PO-DRPA0351-RC-LEER                      VALUE 'LER'.
           05  PO-DRPA0351-ANZAHL-ERG-ZEILEN   PIC  9(05).

           05  PO-DRPA0351-ERGEBNISZEILE.
               10 PO-DRPA0351-PGEB-GES         PIC  9(07).
               10 PO-DRPA0351-DTA-ART          PIC  X(03).
               10 PO-DRPA0351-ERSTELL-DAT      PIC  X(10).
               10 PO-DRPA0351-TRANSF-DAT       PIC  X(10).
               10 PO-DRPA0351-VSTAT            PIC  X(04).
               10 PO-DRPA0351-KONTR-WERT       PIC S9(09)V99.
      *
      ******* ENDE COPY-MEMBER **************************************
