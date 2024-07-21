      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.04.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3003.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3003.CPv  $
      *
      *   Rev 2.1   May 12 2015 09:19:26   t13ex73
      *AKI01360: §34f VB-SST Umsetzung
      *
      *   Rev 2.0   Apr 22 2015 13:51:50   t13ex73
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI3003  FUNKTION UPDATE STANDARD-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Dylong                                          *
      *    DATUM:    23.04.2015                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01342!23.04.15! 01 ! NEUERSTELLUNG             ! Dylong   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHAELT DIE EIN- UND AUSGABEPARAMETER        *
      *    FUER DIE FUNKTION DRKI3003                                *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE <<<        AUSSENDIENSTMA    "ADMA"           *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRKI3003.
          05 PI-DRKI3003-ADMAKEY.
             10 PI-DRKI3003-ADMAPERSONENID      PIC X(013).
             10 PI-DRKI3003-ADMAPERSONENROLLE   PIC X(001).
          05 PI-DRKI3003-ADMADATEN.
             10 PI-DRKI3003-NI-ADMA34FDAT       PIC  S9(004) COMP.
             10 PI-DRKI3003-ADMA34FDAT          PIC  X(010).
      *
       01 PO-DRKI3003.
          05 PO-DRKI3003-RC                     PIC  X(003).
             88 PO-DRKI3003-RC-OK                         VALUE 'OK '.
             88 PO-DRKI3003-RC-CHANGED                    VALUE 'CHA'.
             88 PO-DRKI3003-RC-NOT-FOUND                  VALUE 'NFD'.
      *
      ****** Ende Copy-Member ****************************************

