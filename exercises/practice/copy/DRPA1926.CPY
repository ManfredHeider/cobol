      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  20.08.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1926.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1926.CPv  $
      *
      *   Rev 2.1   Aug 26 2019 12:11:30   t13ex67
      *APA02843: WEPA0008
      *
      *   Rev 2.0   Aug 20 2019 08:59:02   t13ex67
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1926  FUNKTION SUCHE ALLE EINTRÄGE ZU    *
      *                                    PNAP-VNR                  *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH-SCHÄFER                                  *
      *    DATUM:    28.08.2019                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME    *
      * -------------------------------------------------------------*
      * APA02843!20.08.19!02.01! NEUERSTELLUNG             ! STORCH  *
      *         !        !     !                           !         *
      *         !        !     !                           !         *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1926                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1926.
          05 PI-DRPA1926-PROGRAMM            PIC  X(008).
          05 PI-DRPA1926-UFKT-CODE           PIC  9(002).
          05 PI-DRPA1926-PNAP-VNR            PIC  X(015).
      *
       01 PO-DRPA1926.
          05 PO-DRPA1926-RC                PIC  X(003).
             88 PO-DRPA1926-RC-OK               VALUE 'OK '.
             88 PO-DRPA1926-RC-LEER             VALUE 'LER'.
             88 PO-DRPA1926-RC-EOF              VALUE 'EOF'.
          05 PO-DRPA1926-PNAP.
             COPY DRPA0407.
      *
      ****** ENDE COPY-MEMBER ****************************************

