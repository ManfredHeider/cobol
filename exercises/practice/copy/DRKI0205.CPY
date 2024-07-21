      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0205.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0205.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:53:44   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Aug 1998 16:47:16   T13EX19
      *VB-SCHNITTSTELLE
      *
      *   Rev 1.0   23 Jun 1998 13:47:58   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI0205  FUNKTION SELECT STANDARD-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Veit                                            *
      *    DATUM:    23.06.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00001!23.06.98! 01 ! NEUERSTELLUNG             ! Veit     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI0205                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SEL-ALL    <<<         ADRESSE "ADRE"                *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRKI0205.
          05 PI-DRKI0205-KEY.
             10 PI-DRKI0205-ADRESSEID            PIC  X(013).
      *
       01 PO-DRKI0205.
          05 PO-DRKI0205-RC                     PIC  X(003).
             88 PO-DRKI0205-RC-OK                     VALUE 'OK '.
             88 PO-DRKI0205-RC-NOT-FOUND              VALUE 'NFD'.
          05 PO-DRKI0205-NI-TAB.
             10 PO-DRKI0205-NI-ELE PIC S9(04) COMP OCCURS  7 TIMES.
          05 PO-DRKI0205-NIS  REDEFINES  PO-DRKI0205-NI-TAB.
             10 PO-DRKI0205-NI-ID                PIC  S9(004) COMP.
             10 PO-DRKI0205-NI-BDSLDID           PIC  S9(004) COMP.
             10 PO-DRKI0205-NI-LANID             PIC  S9(004) COMP.
             10 PO-DRKI0205-NI-ORT               PIC  S9(004) COMP.
             10 PO-DRKI0205-NI-PLZ               PIC  S9(004) COMP.
             10 PO-DRKI0205-NI-STRHAUSNUM        PIC  S9(004) COMP.
             10 PO-DRKI0205-NI-ISTVTADR          PIC  S9(004) COMP.
          05 PO-DRKI0205-ADRE-DATEN.
             10 PO-DRKI0205-ADRESSEID            PIC  X(013).
             10 PO-DRKI0205-ADRESSEBDSLDID       PIC  X(003).
             10 PO-DRKI0205-ADRESSELANDID        PIC  X(003).
             10 PO-DRKI0205-ADRESSEORT           PIC  X(030).
             10 PO-DRKI0205-ADRESSEPLZ           PIC  X(015).
             10 PO-DRKI0205-ADRESSESTRHAUSNUM    PIC  X(045).
             10 PO-DRKI0205-ADRESSEISTVTADR      PIC  X(001).
          05 PO-DRKI0205-LAST-UPDATE             PIC  X(026).
      *
      ****** Ende Copy-Member ****************************************
