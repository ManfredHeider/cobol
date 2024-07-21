      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1269.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1269.CPv  $
      *
      *   Rev 2.1   Jun 30 1999 12:43:32   T13AE18
      *apa00012
      *
      *
      *   Rev 2.0   Mar 30 1999 08:44:20   P13SP99
      *Neuerstellung
      *
      *   Rev 2.0   Mar 05 1999 10:18:48   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1269 FUNKTION PE-STRUKTUR VERGLEICHEN    *
      *                          FÜR UGPA0002                        *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    30.06.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  ! VER ! AENDERUNG                 ! NAME    *
      * -------------------------------------------------------------*
      * APA00012!30.06.99! 01  ! NEU                       !BOHLINGER*
      *         !        !     !                           !         *
      *         !        !     !                           !         *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1269                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1269.
          05 PI-DRPA1269-PEST-NEU           PIC S9(015) PACKED-DECIMAL.
          05 PI-DRPA1269-VERGLEICHS-PEST    PIC S9(015) PACKED-DECIMAL.

       01 PO-DRPA1269.
          05 PO-DRPA1269-RC                 PIC  X(003).
             88 PO-DRPA1269-RC-PEST-IDENTISCH           VALUE 'PID'.
             88 PO-DRPA1269-RC-PEST-GLEICH              VALUE 'PGL'.
             88 PO-DRPA1269-RC-PEST-UNGLEICH            VALUE 'PUG'.


      *---------------------------------------------------------------*
