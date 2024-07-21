      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0207.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/DRKI0207.CPv  $
      *
      *   Rev 2.1   Oct 13 2003 11:46:04   t13ex49
      *AKI00114 - Dublettenbereinigung
      *
      *   Rev 2.0   Feb 27 1999 12:53:44   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Aug 1998 16:47:22   T13EX19
      *VB-SCHNITTSTELLE
      *
      *   Rev 1.0   23 Jun 1998 13:48:10   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI0207  FUNKTION UPDATE STANDARD-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Veit                                            *
      *    DATUM:    22.06.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00001!23.06.98! 01 ! NEUERSTELLUNG             ! Veit     *
070703* AKI00114!05.04.02!    ! Neue Felder               !Peter Pohl*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI0207                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE <<<        ADRESSE 'ADRE'                     *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRKI0207.
          05 PI-DRKI0207-ADRESSEKEY.
             10 PI-DRKI0207-ADRESSEID            PIC X(13).
          05 PI-DRKI0207-NI-TAB.
070703*      10 PI-DRKI0207-NI-ELE PIC S9(04) COMP OCCURS  6 TIMES.
070703       10 PI-DRKI0207-NI-ELE PIC S9(04) COMP OCCURS  8 TIMES.
          05 PI-DRKI0207-NIS  REDEFINES  PI-DRKI0207-NI-TAB.
             10 PI-DRKI0207-NI-BDSLDID           PIC  S9(004) COMP.
             10 PI-DRKI0207-NI-LANDID            PIC  S9(004) COMP.
             10 PI-DRKI0207-NI-ORT               PIC  S9(004) COMP.
             10 PI-DRKI0207-NI-PLZ               PIC  S9(004) COMP.
             10 PI-DRKI0207-NI-STRHAUSNUM        PIC  S9(004) COMP.
             10 PI-DRKI0207-NI-ISTVTADR          PIC  S9(004) COMP.
070703       10 PI-DRKI0207-NI-SUCHSTRASSE       PIC  S9(004) COMP.
070703       10 PI-DRKI0207-NI-GEPRUEFT          PIC  S9(004) COMP.
          05 PI-DRKI0207-ADRESSEDATEN.
             10 PI-DRKI0207-ADRESSEBDSLDID       PIC X(003).
             10 PI-DRKI0207-ADRESSELANDID        PIC X(003).
             10 PI-DRKI0207-ADRESSEORT           PIC X(030).
             10 PI-DRKI0207-ADRESSEPLZ           PIC X(015).
             10 PI-DRKI0207-ADRESSESTRHAUSNUM    PIC X(045).
             10 PI-DRKI0207-ADRESSEISTVTADR      PIC X(001).
070703       10 PI-DRKI0207-ADRESSESUCHSTRASSE   PIC X(045).
070703       10 PI-DRKI0207-ADRESSEGEPRUEFT      PIC X(001).
          05 PI-DRKI0207-lAST-UPDATE             PIC X(026).
      *
       01 PO-DRKI0207.
          05 PO-DRKI0207-RC                  PIC  X(003).
             88 PO-DRKI0207-RC-OK                         VALUE 'OK '.
             88 PO-DRKI0207-RC-CHANGED                    VALUE 'CHA'.
             88 PO-DRKI0207-RC-NOT-FOUND                  VALUE 'NFD'.
      *
      ****** Ende Copy-Member ****************************************
