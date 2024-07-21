      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0222.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0222.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:53:52   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Aug 1998 16:48:00   T13EX19
      *VB-SCHNITTSTELLE
      *
      *   Rev 1.0   23 Jun 1998 13:49:26   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI0222  FUNKTION SELECT STANDARD-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Veit                                            *
      *    DATUM:    15.07.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00001!08.07.98! 01 ! NEUERSTELLUNG             ! Veit     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI0222                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SEL        <<<         MITARBEITER                   *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRKI0222.
          05 PI-DRKI0222-KEY.
             10 PI-DRKI0222-MITARBEITERNR        PIC  S9(9) COMP.
      *
       01 PO-DRKI0222.
          05 PO-DRKI0222-RC                     PIC  X(003).
             88 PO-DRKI0222-RC-OK                     VALUE 'OK '.
             88 PO-DRKI0222-RC-NOT-FOUND              VALUE 'NFD'.
          05 PO-DRKI0222-NI-TAB.
             10 PO-DRKI0222-NI-ELE PIC S9(04) COMP OCCURS  2 TIMES.
          05 PO-DRKI0222-NIS  REDEFINES  PO-DRKI0222-NI-TAB.
             10 PO-DRKI0222-NI-VORGESPERSID      PIC  S9(004) COMP.
             10 PO-DRKI0222-NI-VORGESROLLE       PIC  S9(004) COMP.
          05 PO-DRKI0222-MITA-DATEN.
             10 PO-DRKI0222-MITARBPERSONENID     PIC  X(013).
             10 PO-DRKI0222-MITARBPERSROLLE      PIC  X(001).
             10 PO-DRKI0222-MITARBVORGESPERSID   PIC  X(013).
             10 PO-DRKI0222-MITARBVORGESROLLE    PIC  X(001).
      *
      ****** Ende Copy-Member ****************************************
