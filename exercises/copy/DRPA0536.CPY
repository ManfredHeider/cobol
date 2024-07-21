      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0536.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA0536.CPv  $
      *
      *   Rev 2.1   Jan 04 2000 13:54:28   T13AE18
      *APA00068
      *
      *
      *   Rev 2.0   Jun 23 1999 11:47:02   T13EX21
      * 
      *
      *   Rev 1.0   12 Sep 1997 17:00:42   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0536                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA0536                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                        *
      *    DATUM:    21.10.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0020!21.10.97! 01 ! NEU                       ! BOHLINGER*
      * APA00068!10.11.99!2.01!PO-Bereich in copy DRPA0751!Bohlinger *
      *         !        !    !ausgelagert                !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA0536
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION DRPA0536 DES BUFFER-OZP ZYPA0009 ERMITTELT
      *    DIE ERGEBNISMENGE FÜR SF41, "ÜBERSICHT PRODUKT".
      *
      ****************************************************************
      *
       01  PI-DRPA0536.
           05  PI-DRPA0536-UFKT                PIC  9(02).
           05  PI-DRPA0536-NAME-ZW-SPEICHER    PIC  X(08).
           05  PI-DRPA0536-LOG-SATZ-NR         PIC  9(05).
           05  PI-DRPA0536-AUSWAHL-KRITERIEN.
               10 PI-DRPA0536-PGEB-GES         PIC S9(04) COMP.
               10 PI-DRPA0536-STICHTAG         PIC  X(10).
               10 PI-DRPA0536-S-STICHTAG       PIC  X(01).
      *
       01  PO-DRPA0536.
           05  PO-DRPA0536-RC                  PIC  X(03).
               88 PO-DRPA0536-RC-OK                        VALUE 'OK '.
               88 PO-DRPA0536-RC-EOF                       VALUE 'EOF'.
               88 PO-DRPA0536-RC-LEER                      VALUE 'LER'.
           05  PO-DRPA0536-ANZAHL-ERG-ZEILEN   PIC  9(05).

           05  PO-DRPA0536-ERGEBNISZEILE.
101199         COPY DRPA0752.
101199*        10 PO-DRPA0536-PROD-PROD-SCHL   PIC S9(004) COMP.
101199*        10 PO-DRPA0536-PROD-BEGINN      PIC  X(010).
101199*        10 PO-DRPA0536-PROD-ENDE        PIC  X(010).
101199*        10 PO-DRPA0536-PROD-BEZ         PIC  X(040).
      *
      ******* ENDE COPY-MEMBER **************************************
