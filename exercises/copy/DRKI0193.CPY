      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0193.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI0193.CPv  $
      *
      *   Rev 2.2   Oct 06 2009 10:48:54   t13ex51
      *AKI00852: KI-Host - Kundensuche
      *
      *   Rev 2.1   Sep 30 2009 14:49:24   t13ex51
      *AKI00852: KI-Host - Kundensuche
      *
      *   Rev 2.0   Feb 27 1999 12:53:36   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Aug 1998 16:46:42   T13EX19
      *VB-SCHNITTSTELLE
      *
      *   Rev 1.0   29 Sep 1997 11:56:36   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI0193  FUNKTION SELECT STANDARD-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Veit                                            *
      *    DATUM:    08.07.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00001!08.07.98! 01 ! NEUERSTELLUNG             ! Veit     *
      * -------------------------------------------------------------*
      * AKI00852!31.08.09! 02 ! SUCHNAME AUF 30 STELLEN   ! HRO      *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI0193                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SEL-ALL    <<<         PERSON                        *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRKI0193.
          05 PI-DRKI0193-KEY.
             10 PI-DRKI0193-PERSONENID         PIC  X(013).
      *
       01 PO-DRKI0193.
          05 PO-DRKI0193-RC                     PIC  X(003).
             88 PO-DRKI0193-RC-OK                     VALUE 'OK '.
             88 PO-DRKI0193-RC-NOT-FOUND              VALUE 'NFD'.
          05 PO-DRKI0193-NI-TAB.
             10 PO-DRKI0193-NI-ELE PIC S9(04) COMP OCCURS  7 TIMES.
          05 PO-DRKI0193-NIS  REDEFINES  PO-DRKI0193-NI-TAB.
             10 PO-DRKI0193-NI-ANZNAME           PIC  S9(004) COMP.
             10 PO-DRKI0193-NI-ANZSUCHNAME       PIC  S9(004) COMP.
310809*      10 PO-DRKI0193-NI-NAMEPHONCODE      PIC  S9(004) COMP.
             10 PO-DRKI0193-NI-ANZVORNAME        PIC  S9(004) COMP.
             10 PO-DRKI0193-NI-SUCHNAME          PIC  S9(004) COMP.
             10 PO-DRKI0193-NI-SUCHVORNAME       PIC  S9(004) COMP.
             10 PO-DRKI0193-NI-GEBGRUENDDAT      PIC  S9(004) COMP.
             10 PO-DRKI0193-NI-ANREDEID          PIC  S9(004) COMP.
          05 PO-DRKI0193-PERS-DATEN.
             10 PO-DRKI0193-PERSONANZNAME        PIC  X(030).
310809*      10 PO-DRKI0193-PERSONNAMEPHONCODE   PIC  X(015).
310809*      10 PO-DRKI0193-PERSONANZVORNAME     PIC  X(015).
310809*      10 PO-DRKI0193-PERSONSUCHNAME       PIC  X(015).
310809*      10 PO-DRKI0193-PERSONSUCHVORNAME    PIC  X(015).
310809       10 PO-DRKI0193-PERSONANZVORNAME     PIC  X(030).
310809       10 PO-DRKI0193-PERSONSUCHNAME       PIC  X(030).
310809       10 PO-DRKI0193-PERSONSUCHVORNAME    PIC  X(030).
             10 PO-DRKI0193-PERSONGEBGRUENDDAT   PIC  X(010).
             10 PO-DRKI0193-PERSONANREDEID       PIC  S9(004).
310809*      10 PO-DRKI0193-PERSONKUNDENART      PIC  X(001).
          05 PO-DRKI0193-LAST-UPDATE             PIC  X(026).
      *
      ****** Ende Copy-Member ****************************************
