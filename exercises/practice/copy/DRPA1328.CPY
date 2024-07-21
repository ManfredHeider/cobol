      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1328.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1328.CPv  $
      *
      *   Rev 2.1   Feb 07 2000 12:03:12   T13EX22
      *APA00077: Neue Funktion
      *
      *   Rev 2.0   Dec 14 1999 16:42:16   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1328 LÖSCHEN EINZELNES PNAP-OBJEKT        *
      *    -------------------------------------------------------    *
      *    ERSTELLT: T. MUND                                          *
      *    DATUM:    14.01.2000                                       *
      *****************************************************************
      *    VERSION   01                                               *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME      *
      * ------------------------------------------------------------- *
      * APA00077!14.01.00! 1.0 ! NEUEERSTELLUNG           ! MUND      *
      *         !        !     !                          !           *
      *         !        !     !                          !           *
      *         !        !     !                          !           *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER          *
      *    FÜR DIE FUNKTION DRPA1328                                  *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
       01 PI-DRPA1328.
          05 PI-DRPA1328-DATEN.
             10 PI-DRPA1328-PRINTE           PIC S9(015) PACKED-DECIMAL.
             10 PI-DRPA1328-FAELLIG-VON      PIC  X(010).
      *
       01 PO-DRPA1328.
          05 PO-DRPA1328-RC                  PIC  X(003).
             88 PO-DRPA1328-RC-OK            VALUE 'OK '.
             88 PO-DRPA1328-RC-NOT-FOUND     VALUE 'NFD'.
      *

