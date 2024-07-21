      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  09.02.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1661.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1661.CPv  $
      *
      *   Rev 2.1   15 Feb 2007 11:19:04   t13ae46
      *AAN00655: Erweiterung des PS auf 3 Stellen - JWA 
      *
      *   Rev 2.0   09 Feb 2007 14:49:46   t13ae46
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1661 FUNKTION UPDATE STANDARD-OZP        *
      *    -------------------------------------------------------   *
      *    ERSTELLT: J.WANG                                          *
      *    DATUM:    09.02.2007                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN655  !09.02.07! 01 ! NEUERSTELLUNG             ! WANG     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1661                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE  <<<       TABELLE      TBPA0124 MPIN         *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1661.
          05 PI-DRPA1661-PGEB-GES          PIC S9(004) BINARY.
          05 PI-DRPA1661-MPIN-MAX-PIN      PIC S9(009) COMP.
      *
       01 PO-DRPA1661.
          05 PO-DRPA1661-RC                   PIC  X(003).
             88 PO-DRPA1661-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1661-RC-NOT-FOUND                  VALUE 'NFD'.
      *
      ****** ENDE COPY-MEMBER ****************************************
