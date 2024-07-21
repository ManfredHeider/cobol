      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1541.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/MF/CPY/DRPA1541.CPv  $
      *
      *   Rev 2.2   Jun 19 2002 09:22:04   t13ex21
      *APA00191 - Korrektur
      *
      *   Rev 2.1   Jun 18 2002 14:47:48   t13ex21
      *APA00191 - Neuerstellung
      *
      *   Rev 2.0   04 Jul 2001 11:11:36   p13sp13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1541 FUNKTION PRÜFEN PRODUKT, OB ES ZU   *
      *                          BRUTTO-SUMME-BEITRAG GEHÖRT.        *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    13.06.2002                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00191!13.06.02!2.01! NEUERSTELLUNG             ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1541                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1541.
          05 PI-DRPA1541-DATEN.
             10 PI-DRPA1541-PGEB-GES             PIC S9(04) BINARY.
             10 PI-DRPA1541-PROD-PROD-SCHL       PIC S9(04) BINARY.
      *
       01 PO-DRPA1541.
          05 PO-DRPA1541-RC                      PIC  X(03).
             88 PO-DRPA1541-RC-BTG-MAX                VALUE 'BMX'.
             88 PO-DRPA1541-RC-KEIN-BTG-MAX           VALUE 'KBX'.
          05 PO-DRPA1541-DATEN.
             10 PO-DRPA1541-KNR-BTG-MAX          PIC S9(04) BINARY.
      *
      ******** ENDE COPY-MEMBER **************************************
