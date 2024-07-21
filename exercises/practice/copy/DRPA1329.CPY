      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1329.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1329.CPv  $
      *
      *   Rev 2.1   Feb 07 2000 12:03:14   T13EX22
      *APA00077: Neue Funktion
      *
      *   Rev 2.0   Dec 14 1999 16:42:16   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1329  FUNKTION SUCHE KLEINSTEN EINTRAG   *
      *                                    ZU PRINTE                 *
      *    -------------------------------------------------------   *
      *    ERSTELLT: MUND                                            *
      *    DATUM:    14.01.2000                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME    *
      * -------------------------------------------------------------*
      * APA00077!14.01.00!01.01! NEUERSTELLUNG             ! MUND    *
      *         !        !     !                           !         *
      *         !        !     !                           !         *
      *         !        !     !                           !         *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1329                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1329.
          05 PI-DRPA1329-PRINTE              PIC S9(015) PACKED-DECIMAL.
      *
       01 PO-DRPA1329.
          05 PO-DRPA1329-RC                  PIC  X(003).
             88 PO-DRPA1329-RC-OK                 VALUE 'OK '.
             88 PO-DRPA1329-RC-NOT-FOUND          VALUE 'NFD'.
          05 PO-DRPA1329-DATEN.
             10 PO-DRPA1329-PNAP-FAELLIG-VON PIC X(010).
      *
      ****** ENDE COPY-MEMBER ****************************************

