      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0231.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0231.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:38   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 17:58:06   T13EX26
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   03 Dec 1997 15:48:24   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0231                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRAT0231                    *
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
      *    FÜR DIE FUNKTION DRAT0231
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION DRAT0231 DES BUFFER-OZP ZYAT0002
      *    INSERT EINER ERGEBNISZEILE  "PFLEGE NOTIZ".
      *
      ****************************************************************
      *
       01  PI-DRAT0231.
           05  PI-DRAT0231-UFKT                     PIC  9(02).
           05  PI-DRAT0231-NAME-ZW-SPEICHER         PIC  X(08).
           05  PI-DRAT0231-LOG-SATZ-NR              PIC  9(05).
           05  PI-DRAT0231-ERGEBNISZEILE.
               10 PI-DRAT0231-NTZ-DATEN.
                  15 PI-DRAT0231-KNR-OBJEKT-HKF-TAB PIC S9(004) BINARY.
                  15 PI-DRAT0231-KNR-NOTIZ-ART      PIC S9(004) BINARY.
                  15 PI-DRAT0231-NTZ-KENNUNG        PIC  X(008).
                  15 PI-DRAT0231-NTZ-LAST-UPDATE    PIC  X(026).
               10 PI-DRAT0231-NTZZ-DATEN.
                  15 PI-DRAT0231-NTZZ-ANZAHL        PIC S9(004) BINARY.
                  15 PI-DRAT0231-NTZZ-TABELLE       OCCURS 10.
                     25 PO-DRAT0231-NTZZ-ZEILE      PIC  X(080).
      *
       01  PO-DRAT0231.
           05  PO-DRAT0231-RC                       PIC  X(03).
               88 PO-DRAT0231-RC-OK                        VALUE 'OK '.
           05  PO-DRAT0231-ANZAHL-ERG-ZEILEN        PIC  9(05).
      *
      ******* ENDE COPY-MEMBER **************************************
