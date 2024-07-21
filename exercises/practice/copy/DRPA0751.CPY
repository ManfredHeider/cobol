      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0751.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA0751.CPv  $
      *
      *   Rev 2.1   Jan 04 2000 13:09:14   T13AE18
      * 
      *
      *   Rev 2.0   Feb 27 1999 14:29:32   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   12 Dec 1997 09:45:06   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0751                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA0751                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                        *
      *    DATUM:    15.11.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
       01  PI-DRPA0751.
           05  PI-DRPA0751-UFKT                PIC  9(02).
           05  PI-DRPA0751-NAME-ZW-SPEICHER    PIC  X(08).
           05  PI-DRPA0751-AUSWAHL-KRITERIEN.
               10 PI-DRPA0751-PGEB-GES         PIC S9(04) COMP.
               10 PI-DRPA0751-STICHTAG         PIC  X(10).
               10 PI-DRPA0751-S-STICHTAG       PIC  X(01).
      *
       01  PO-DRPA0751.
           05  PO-DRPA0751-RC                  PIC  X(03).
               88 PO-DRPA0751-RC-OK                        VALUE 'OK '.
               88 PO-DRPA0751-RC-EOF                       VALUE 'EOF'.
               88 PO-DRPA0751-RC-LEER                      VALUE 'LER'.
           05  PO-DRPA0751-ANZAHL-ERG-ZEILEN   PIC  9(05).

           05  PO-DRPA0751-ERGEBNISZEILE.
               COPY DRPA0752.
      *
      ******* ENDE COPY-MEMBER **************************************
