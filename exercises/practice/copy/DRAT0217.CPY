      *---------------------------------------------------------------*
      *    Standard f?r alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0217.CPY  $
      *    $Revision:   1.1  $
      *
      *    $Log:   L:/MF/CPY/DRAT0217.CPv  $
      *
      *   Rev 1.1   12 Nov 1998 16:03:02   T13EX26
      *apa00033 - Neuerstellung
      *
      *   Rev 1.0   03 Dec 1997 15:47:26   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0217                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FšR DIE FUNKTION DRAT0217                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                        *
      *    DATUM:    05.01.1998                                       *
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
      *    DIESES COPY ENTHŽLT DIE EIN- UND AUSGABEPARAMETER
      *    FšR DIE FUNKTION DRAT0217
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION DRAT0217 DES BUFFER-OZP ZYAT0001 ERMITTELT
      *    DIE ERGEBNISMENGE F?R XA14, "?BERSICHT NOTIZEN".
      *
      ****************************************************************
      *
       01  PI-DRAT0217.
           05  PI-DRAT0217-UFKT                     PIC  9(02).
           05  PI-DRAT0217-NAME-ZW-SPEICHER         PIC  X(08).
           05  PI-DRAT0217-LOG-SATZ-NR              PIC  9(05).
           05  PI-DRAT0217-AUSWAHL-KRITERIEN.
               10 PI-DRAT0217-KNR-OBJEKT-HKF-TAB    PIC S9(004) BINARY.
               10 PI-DRAT0217-NTZ-PTR-ID            PIC S9(015)
                                                    PACKED-DECIMAL.
               10 PI-DRAT0217-NTZ-PTR-ST-NR         PIC S9(009) BINARY.
               10 PI-DRAT0217-NTZ-PTR-PIN           PIC S9(009) BINARY.
               10 PI-DRAT0217-NTZ-PTR-VNR           PIC  X(015).
      *
       01  PO-DRAT0217.
           05  PO-DRAT0217-RC                       PIC  X(03).
               88 PO-DRAT0217-RC-OK                        VALUE 'OK '.
               88 PO-DRAT0217-RC-EOF                       VALUE 'EOF'.
               88 PO-DRAT0217-RC-LEER                      VALUE 'LER'.
           05  PO-DRAT0217-ANZAHL-ERG-ZEILEN        PIC  9(05).

           05  PO-DRAT0217-ERGEBNISZEILE.
               20 PO-DRAT0217-NTZ-ID                PIC S9(015)
                                                    PACKED-DECIMAL.
               20 PO-DRAT0217-KNR-NOTIZ-ART         PIC S9(004) BINARY.
               20 PO-DRAT0217-NTZ-KENNUNG           PIC  X(008).
               20 PO-DRAT0217-NTZ-LAST-UPDATE       PIC  X(026).
               20 PO-DRAT0217-NTZZ-ZEILE-1          PIC  X(045).
      *
      ******* ENDE COPY-MEMBER **************************************
