      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0232.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0232.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:38   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 17:58:08   T13EX26
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   03 Dec 1997 15:48:28   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0232                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRAT0232                    *
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
      *    FÜR DIE FUNKTION DRAT0232
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION DRAT0232 DES BUFFER-OZP ZYAT0002
      *    UPDATE EINER ERGEBNISSEITE FüR XA15, "PFLEGE NOTIZ".
      *
      ****************************************************************
      *
       01  PI-DRAT0232.
           05  PI-DRAT0232-UFKT                     PIC  9(02).
           05  PI-DRAT0232-NAME-ZW-SPEICHER         PIC  X(08).
           05  PI-DRAT0232-LOG-SATZ-NR              PIC  9(05).
           05  PI-DRAT0232-ERGEBNISZEILE.
               10 PI-DRAT0232-NTZ-DATEN.
                  15 PI-DRAT0232-KNR-OBJEKT-HKF-TAB PIC S9(004) BINARY.
                  15 PI-DRAT0232-KNR-NOTIZ-ART      PIC S9(004) BINARY.
                  15 PI-DRAT0232-NTZ-KENNUNG        PIC  X(008).
                  15 PI-DRAT0232-NTZ-LAST-UPDATE    PIC  X(026).
               10 PI-DRAT0232-NTZZ-DATEN.
                  15 PI-DRAT0232-NTZZ-ANZAHL        PIC S9(004) BINARY.
                  15 PI-DRAT0232-NTZZ-TABELLE       OCCURS 10.
                     25 PO-DRAT0232-NTZZ-ZEILE      PIC  X(080).
      *
       01  PO-DRAT0232.
           05  PO-DRAT0232-RC                       PIC  X(03).
               88 PO-DRAT0232-RC-OK                        VALUE 'OK '.

      *
      ******* ENDE COPY-MEMBER **************************************
