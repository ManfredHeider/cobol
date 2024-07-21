      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa0702.cpy  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\drpa0702.cpv  $
      *
      *   Rev 2.0   May 27 1999 12:45:14   T13EX25
      *APA00042, Neuerstellung V2.0
      *
      *   Rev 1.0   12 Dec 1997 09:41:26   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0702  FUNKTION Suche aktuellsten Eintrag *
      *                                    zu PRINTE                 *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Jäger-Sokoluk                                   *
      *    DATUM:    15.12.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME    *
      * -------------------------------------------------------------*
      * AFPA0012!15.12.97!01.01! NEUERSTELLUNG             ! Jäger   *
      *         !        !     !                           !         *
      *         !        !     !                           !         *
      *         !        !     !                           !         *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0702                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0702.
          05 PI-DRPA0702-PRINTE            PIC S9(015) PACKED-DECIMAL.
      *
       01 PO-DRPA0702.
          05 PO-DRPA0702-RC                PIC  X(003).
             88 PO-DRPA0702-RC-OK               VALUE 'OK '.
             88 PO-DRPA0702-RC-NOT-FOUND        VALUE 'NFD'.
          05 PO-DRPA0702-PNAP.
             COPY DRPA0407.
      *
      ****** Ende Copy-Member ****************************************
