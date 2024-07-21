      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0232.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0232.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:53:58   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Aug 1998 16:48:24   T13EX19
      *VB-SCHNITTSTELLE
      *
      *   Rev 1.0   23 Jun 1998 13:50:08   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI0232  FUNKTION SELECT STANDARD-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Veit                                            *
      *    DATUM:    13.07.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00001!13.07.98! 01 ! NEUERSTELLUNG             ! Veit     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI0232                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SEL-ALL    <<<         MITARBEITER                   *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRKI0232.
          05 PI-DRKI0232-MARBKEY.
             10 PI-DRKI0232-MITARBPERSONENID    PIC  X(013).
             10 PI-DRKI0232-MITARBPERSROLLE     PIC  X(001).
      *
       01 PO-DRKI0232.
          05 PO-DRKI0232-RC                     PIC  X(003).
             88 PO-DRKI0232-RC-OK                     VALUE 'OK '.
             88 PO-DRKI0232-RC-NOT-FOUND              VALUE 'NFD'.
          05 PO-DRKI0232-NI-TAB.
             10 PO-DRKI0232-NI-ELE PIC S9(04) COMP OCCURS 03 TIMES.
          05 PO-DRKI0232-NIS  REDEFINES  PO-DRKI0232-NI-TAB.
             10 PO-DRKI0232-NI-VORGESPERSID     PIC  S9(004) COMP.
             10 PO-DRKI0232-NI-VORGESROLLE      PIC  S9(004) COMP.
             10 PO-DRKI0232-NI-MITARBEITERNR    PIC  S9(004) COMP.
          05 PO-DRKI0232-MARBDATEN.
             10 PO-DRKI0232-MITARBVORGESPERSID  PIC  X(013).
             10 PO-DRKI0232-MITARBVORGESROLLE   PIC  X(001).
             10 PO-DRKI0232-MITARBEITERNR       PIC  S9(009) COMP.
      *
      ****** Ende Copy-Member ****************************************
