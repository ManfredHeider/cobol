      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1065.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1065.CPv  $
      *
      *   Rev 2.1   May 27 1999 10:41:36   T01JM2E
      *NT-Umstellung
      *
      *   Rev 1.0   20 Jul 1998 10:19:32   P13SP99
      *Initial revision.
      *
      ****************************************************************
      *    COPY-MEMBER: DRPA1065  Funktion Select Standard-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: D.WILHELM                                       *
      *    DATUM:    11.09.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!11.09.98! 01 ! NEUERSTELLUNG             !D.WILHELM *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1065                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  select  <<<       TABELLE PDPT TBPA0122              *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1065.
          05 PI-DRPA1065-PDPT-KEY.
             10 PI-DRPA1065-PDPT-ID             PIC S9(15)V COMP-3.
      *
       01 PO-DRPA1065.
          05 PO-DRPA1065-RC                   PIC  X(003).
             88 PO-DRPA1065-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1065-RC-CHANGED                    VALUE 'CHA'.
             88 PO-DRPA1065-RC-NOT-FOUND                  VALUE 'NFD'.
          05 PO-DRPA1065-PDPT-DATEN.
               10 PO-DRPA1065-PDPT-LAST-UPDATE     PIC X(26).
             07 PO-DRPA1065-PDPT-TEIL-DATEN.
               10 PO-DRPA1065-PDPT-KENNUNG         PIC X(8).
               10 PO-DRPA1065-PMPT-ID              PIC S9(15)V COMP-3.
               10 PO-DRPA1065-PMPT-VERSION         PIC S9(4) COMP.
               10 PO-DRPA1065-PEST-ID              PIC S9(15)V COMP-3.
               10 PO-DRPA1065-PDPT-BUCH-RFOLGE     PIC S9(4) COMP.
               10 PO-DRPA1065-PDPT-MA-ABR-DAT      PIC X(10).
               10 PO-DRPA1065-KNR-BUCHUNGS-STAND   PIC S9(4) COMP.
               10 PO-DRPA1065-KNR-BUCHUNGSART      PIC S9(4) COMP.
               10 PO-DRPA1065-KNR-BERCH-WAEHRUNG   PIC S9(4) COMP.
               10 PO-DRPA1065-PDPT-DGV             PIC S9(4) COMP.
               10 PO-DRPA1065-PDPT-PRODART         PIC S9(4) COMP.
             07 PO-DRPA1065-PDPT-FIRST-INSERT    PIC X(26).
             07 PO-DRPA1065-PDPT-FIRST-KENNUNG   PIC X(8).
      *
      ******* ENDE COPY-MEMBER ***************************************      *---
