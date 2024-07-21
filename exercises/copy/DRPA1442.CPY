      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1442.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1442.CPv  $
      *
      *   Rev 2.2   Mar 19 2013 16:10:54   i13az40
      *APA01539: Kalypso TP5: Anpassungen Datenmodell
      *
      *   Rev 2.1   05 Feb 2001 10:19:50   T13AE29
      *APA0118: Einführung der Gesellschaften 46 (BfG Invest) und 47 (Bf
      *
      *   Rev 2.0   12 Jul 2000 12:33:12   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1442  FUNKTION DELETE STANDARD-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Brenner                                         *
      *    DATUM:    05.01.2001                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00118!05.01.01! 01 ! NEUERSTELLUNG             ! Brenner  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1442                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  DELETE  <<<       TABELLE      TBPA0132 PMPEH        *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1442.
          05 PI-DRPA1442-UFKT                 PIC  X(002).
          05 PI-DRPA1442-PMPEH-LAST-UPDATE    PIC  X(026).
          05 PI-DRPA1442-PMPG-KEY.
             10 PI-DRPA1442-PMPG-ID           PIC S9(015) COMP-3.
             10 PI-DRPA1442-PMPG-VERSION      PIC S9(004) COMP.
      *
       01 PO-DRPA1442.
          05 PO-DRPA1442-RC                   PIC  X(003).
             88 PO-DRPA1442-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1442-RC-CHANGED                    VALUE 'CHA'.
             88 PO-DRPA1442-RC-NOT-FOUND                  VALUE 'NFD'.
      *
      ****** Ende Copy-Member ****************************************
