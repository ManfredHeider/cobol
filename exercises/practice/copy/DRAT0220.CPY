      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0220.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0220.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:36   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 17:54:24   T13EX26
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   05 Sep 1997 09:21:34   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0220 FUNKTION ERMITTELN EINER NOTIZ      *
      *                                   ZU EINER ART VON OBJEKT    *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    29.12.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!29.12.97! 01 !NEU                        !BOHLINGER *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAT0220                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    FUNKTION: ERMITTELN EINER NOTIZ ZU EINER ART VON OBJEKT   *
      *                                                              *
      ****************************************************************
       01 PI-DRAT0220.
           05  PI-DRAT0220-UFKT                PIC  X(02).
      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST-NEXT, 3=QUIT
          05 PI-DRAT0220-AUSWAHL-KRITERIEN.
             10 PI-DRAT0220-NTZ-ID              PIC S9(015)
                                                         PACKED-DECIMAL.
      *
       01 PO-DRAT0220.
          05 PO-DRAT0220-RC                     PIC  X(003).
             88 PO-DRAT0220-RC-OK                        VALUE 'OK '.
             88 PO-DRAT0220-RC-LEER                      VALUE 'LER'.
             88 PO-DRAT0220-RC-EOF                       VALUE 'EOF'.
          05 PO-DRAT0220-NTZZ-DATEN.
             10 PO-DRAT0220-NTZZ-ZEILE          PIC  X(080).
      *
      ******** Ende Copy-Member **************************************
