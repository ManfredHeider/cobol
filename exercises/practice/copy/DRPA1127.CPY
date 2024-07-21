      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa1127.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drpa1127.cpv  $
      *
      *   Rev 2.1   Apr 21 1999 09:24:16   T13AE18
      *APA00012 Erweiterung des PO-Bereichs
      *
      *   Rev 2.0   Mar 25 1999 14:10:00   T13AE18
      *APA00012 Einführung EVS-Umbuchungs-
      *dialog
      *
      *   Rev 1.0   20 Jul 1998 10:22:02   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1127 FUNKTION GET Zustand Prov-daten     *
      *                          FüR UGPA0002                        *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    13.11.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!13.11.98! 01 !NEU                        !BOHLINGER *
      * APA00012!13.04.99! 01 !NEUER ZST 'KORREKTUR-OK'   !BOHLINGER *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1127                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1127.
             10 PI-DRPA1127-PRINTE             PIC S9(015)
                                               PACKED-DECIMAL.

       01 PO-DRPA1127.
          05 PO-DRPA1127-RC                    PIC  X(003).
             88 PO-DRPA1127-RC-OK                       VALUE 'OK '.

          05 PO-DRPA1127-ZST-PROVDATEN.
            10 PO-DRPA1127-ZST-PD                PIC  X(002).
             88 PO-DRPA1127-ZST-PD-INIT                 VALUE '01'.
             88 PO-DRPA1127-ZST-PD-GELESEN              VALUE '02'.
             88 PO-DRPA1127-ZST-PD-UMB-OK               VALUE '03'.
130499       88 PO-DRPA1127-ZST-PD-KORREKT-OK           VALUE '04'.
             88 PO-DRPA1127-ZST-PD-UPDATE-OK            VALUE '05'.

      *
      ******** Ende Copy-Member **************************************
