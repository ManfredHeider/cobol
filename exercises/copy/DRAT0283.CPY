      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drat0283.cpy  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\drat0283.cpv  $
      *
      *   Rev 2.2   Apr 12 1999 12:02:16   T13AE34
      *AAT00005-Euro-Switch (hier: PVCS-Kopf)
      *
      *   Rev 2.1   Apr 06 1999 11:33:26   T13AE34
      *AAT00005-Euro-Swicht
      *
      *   Rev 2.0   Mar 01 1999 14:44:48   T13AE34
      *NT-Umstellung
      *
      *   Rev 1.0   13 Jul 1998 09:29:00   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0283                                     *
      *    FUNKTION   : EURO-SWITCH                                  *
      *                 UMRECHNEN MASKENFELDER                       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: HEIDER                                          *
      *    DATUM:    17.02.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAT00005!17.02.99!1.1 !NEU                        !HEIDER    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION drat0283                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRAT0283.
          05 PI-DRAT0283-DATEN.
             10 PI-DRAT0283-PROGRAMM             PIC  X(0008).
             10 PI-DRAT0283-BILDNR               PIC S9(0004) BINARY.
             10 PI-DRAT0283-MASKE                PIC  X(1920).
             10 PI-DRAT0283-AKTION               PIC S9(0004) BINARY.
                88 PI-DRAT0283-ALLES             VALUE 1.
                88 PI-DRAT0283-NUR-TEXTE         VALUE 2.
                88 PI-DRAT0283-NUR-BETRAEGE      VALUE 3.
      *
       01 PO-DRAT0283.
          05 PO-DRAT0283-RC                      PIC  X(0003).
             88 PO-DRAT0283-RC-OK                     VALUE 'OK '.
             88 PO-DRAT0283-RC-NOK                    VALUE 'NOK'.
          05 PO-DRAT0283-DATEN.
             10 PO-DRAT0283-MASKE                PIC  X(1920).
             10 PO-DRAT0283-STD-WAE              PIC S9(0004) BINARY.
             10 PO-DRAT0283-ALT-WAE              PIC S9(0004) BINARY.
      *
      ******** Ende Copy-Member **************************************
