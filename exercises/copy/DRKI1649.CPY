      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01.03.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1649.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1649.CPv  $
      *
      *   Rev 2.1   Mar 11 2004 10:04:38   t13ex49
      *Einführung AdresseOrtsteil
      *
      *   Rev 2.0   Mar 01 2004 13:00:42   t13ex49
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI1649  FUNKTION SELECT STANDARD-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Pohl                                            *
      *    DATUM:    01.03.2004                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00268!01.03.04! 01 ! NEUERSTELLUNG             ! Pohl     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI1649                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SEL-ALL    <<<         ADRESSE "ADRE"                *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRKI1649.
          05 PI-DRKI1649-KEY.
             10 PI-DRKI1649-ADRESSEID            PIC  X(013).
      *
       01 PO-DRKI1649.
          05 PO-DRKI1649-RC                     PIC  X(003).
             88 PO-DRKI1649-RC-OK                     VALUE 'OK '.
             88 PO-DRKI1649-RC-NOT-FOUND              VALUE 'NFD'.
          05 PO-DRKI1649-NI-TAB.
             10 PO-DRKI1649-NI-ELE PIC S9(04) COMP OCCURS  9 TIMES.
          05 PO-DRKI1649-NIS  REDEFINES  PO-DRKI1649-NI-TAB.
             10 PO-DRKI1649-NI-ID                PIC  S9(004) COMP.
             10 PO-DRKI1649-NI-BDSLDID           PIC  S9(004) COMP.
             10 PO-DRKI1649-NI-LANID             PIC  S9(004) COMP.
             10 PO-DRKI1649-NI-ORT               PIC  S9(004) COMP.
             10 PO-DRKI1649-NI-PLZ               PIC  S9(004) COMP.
             10 PO-DRKI1649-NI-STRHAUSNUM        PIC  S9(004) COMP.
             10 PO-DRKI1649-NI-ISTVTADR          PIC  S9(004) COMP.
             10 PO-DRKI1649-NI-SUCHSTRASSE       PIC  S9(004) COMP.
             10 PO-DRKI1649-NI-GEPRUEFT          PIC  S9(004) COMP.
          05 PO-DRKI1649-ADRE-DATEN.
             10 PO-DRKI1649-ADRESSEID            PIC  X(013).
             10 PO-DRKI1649-ADRESSEBDSLDID       PIC  X(003).
             10 PO-DRKI1649-ADRESSELANDID        PIC  X(003).
             10 PO-DRKI1649-ADRESSEORT           PIC  X(030).
             10 PO-DRKI1649-ADRESSEPLZ           PIC  X(015).
             10 PO-DRKI1649-ADRESSESTRHAUSNUM    PIC  X(045).
             10 PO-DRKI1649-ADRESSEISTVTADR      PIC  X(001).
             10 PO-DRKI1649-ADRESSESUCHSTRASSE   PIC  X(045).
             10 PO-DRKI1649-ADRESSEGEPRUEFT      PIC  X(001).
             10 PO-DRKI1649-ADRESSEORTSTEIL      PIC  X(030).
             10 PO-DRKI1649-ADRESSEPOSTPRUEF     PIC  X(025).
             10 PO-DRKI1649-ADRESSEPOSTPRUEFID   PIC S9(004) COMP.
          05 PO-DRKI1649-LAST-UPDATE             PIC  X(026).
      *
      ****** Ende Copy-Member ****************************************

