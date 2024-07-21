      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0226.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0226.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:38   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 17:58:02   T13EX26
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   03 Dec 1997 15:48:02   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0226                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRAT0226                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                        *
      *    DATUM:    20.01.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  !AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00020!21.02.98!01.01!NEU                       ! BOHLINGER*
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRAT0226
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION DRAT0226 DES BUFFER-OZP ZYAT0002 ERMITTELT
      *    DIE ERGEBNISMENGE FüR XA15, "PFLEGE NOTIZ".
      *
      ****************************************************************
      *
       01  PI-DRAT0226.
           05  PI-DRAT0226-UFKT                     PIC  9(02).
           05  PI-DRAT0226-NAME-ZW-SPEICHER         PIC  X(08).
           05  PI-DRAT0226-LOG-SATZ-NR              PIC  9(05).
           05  PI-DRAT0226-KEY.
               10 PI-DRAT0226-NTZ-ID                PIC S9(015)
                                                    PACKED-DECIMAL.
      *
       01  PO-DRAT0226.
           05  PO-DRAT0226-RC                       PIC  X(03).
               88 PO-DRAT0226-RC-OK                        VALUE 'OK '.
               88 PO-DRAT0226-RC-EOF                       VALUE 'EOF'.
               88 PO-DRAT0226-RC-LEER                      VALUE 'LER'.
           05  PO-DRAT0226-ANZAHL-ERG-ZEILEN        PIC  9(05).

           05  PO-DRAT0226-ERGEBNISZEILE.
               10 PO-DRAT0226-KNR-OBJEKT-HKF-TAB    PIC S9(004) BINARY.
               10 PO-DRAT0226-KNR-NOTIZ-ART         PIC S9(004) BINARY.
               10 PO-DRAT0226-NTZ-KENNUNG           PIC  X(008).
               10 PO-DRAT0226-NTZ-LAST-UPDATE       PIC  X(026).
               10 PO-DRAT0226-NTZZ-DATEN.
                  15 PO-DRAT0226-NTZZ-ANZAHL        PIC S9(004) BINARY.
                  15 PO-DRAT0226-NTZZ-TABELLE       OCCURS 10.
                     20 PO-DRAT0226-NTZZ-ZEILE      PIC  X(080).
      *
      ******* ENDE COPY-MEMBER **************************************
