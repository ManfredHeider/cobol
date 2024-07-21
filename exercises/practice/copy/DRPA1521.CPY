      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1521.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/DRPA1521.CPv  $
      *
      *   Rev 2.1   Feb 05 2002 17:38:20   t13ex43
      *APA00165 : GeVo WIK als Folgezugang
      *
      *   Rev 2.0   04 Jul 2001 11:11:06   p13sp13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1521 ERMITTELN VORBEWEGUNG ZU WIK         *
      *                          IM PROVISIONSBESTAND                 *
      *    -----------------------------------------------------------*
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    23.01.2002                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00165!23.01.02!2.01! NEU                       ! KLEEMEIER*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1521                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1521.
          05 PI-DRPA1521-DATEN.
             10 PI-DRPA1521-PGEB-GES                PIC S9(04) COMP.
             10 PI-DRPA1521-PRINTE                  PIC S9(15) COMP-3.
             10 PI-DRPA1521-FAELLIG-VON             PIC  X(10).
             10 PI-DRPA1521-PROD-PROD-SCHL          PIC S9(04) COMP.
             10 PI-DRPA1521-PG-GSMT-PROV      PIC S9(09)V9(02) COMP-3.
      *
       01 PO-DRPA1521.
          05 PO-DRPA1521-RC                         PIC  X(03).
             88 PO-DRPA1521-RC-OK                        VALUE 'OK '.
             88 PO-DRPA1521-RC-NOT-FOUND                 VALUE 'NFD'.
          05 PO-DRPA1521-DATEN.
             10 PO-DRPA1521-SUCHFUNKTION            PIC  X(01).
             10 PO-DRPA1521-KNR-PMBE-ART            PIC S9(04) COMP.
      *
      ******* ENDE COPY-MEMBER **************************************
