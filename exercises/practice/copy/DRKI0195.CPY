      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0195.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI0195.CPv  $
      *
      *   Rev 2.2   Oct 21 2009 13:55:40   t13ex51
      *AKI00852: KI-Host - Kundensuche
      *
      *   Rev 2.1   Sep 30 2009 14:49:44   t13ex51
      *AKI00852: KI-Host - Kundensuche
      *
      *   Rev 2.0   Feb 27 1999 12:53:38   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Aug 1998 16:46:46   T13EX19
      *VB-SCHNITTSTELLE
      *
      *   Rev 1.0   29 Sep 1997 11:56:50   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI0195  FUNKTION UPDATE STANDARD-OZP       *
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
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI0195                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE <<<        PERSON  'PERS'                     *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRKI0195.
          05 PI-DRKI0195-PERS-KEY.
             10 PI-DRKI0195-PERSONENID            PIC X(13).
          05 PI-DRKI0195-NI-TAB.
             10 PI-DRKI0195-NI-ELE PIC S9(04) COMP OCCURS 06 TIMES.
          05 PI-DRKI0195-NIS  REDEFINES  PI-DRKI0195-NI-TAB.
             10 PI-DRKI0195-NI-ANREDEID           PIC  S9(004) COMP.
             10 PI-DRKI0195-NI-ANZNAME            PIC  S9(004) COMP.
             10 PI-DRKI0195-NI-ANZVORNAME         PIC  S9(004) COMP.
             10 PI-DRKI0195-NI-SUCHNAME           PIC  S9(004) COMP.
             10 PI-DRKI0195-NI-SUCHVORNAME        PIC  S9(004) COMP.
310809*      10 PI-DRKI0195-NI-NAMEPHONCODE       PIC  S9(004) COMP.
             10 PI-DRKI0195-NI-GEBGRUENDDAT       PIC  S9(004) COMP.
          05 PI-DRKI0195-PERS-DATEN.
             10 PI-DRKI0195-PERSONANREDEID        PIC  S9(004) COMP.
310809*      10 PI-DRKI0195-PERSONKUNDENART       PIC  X(001).
             10 PI-DRKI0195-PERSONANZNAME         PIC  X(030).
310809*      10 PI-DRKI0195-PERSONANZVORNAME      PIC  X(015).
310809*      10 PI-DRKI0195-PERSONSUCHNAME        PIC  X(015).
310809*      10 PI-DRKI0195-PERSONSUCHVORNAME     PIC  X(015).
310809       10 PI-DRKI0195-PERSONANZVORNAME      PIC  X(030).
310809       10 PI-DRKI0195-PERSONSUCHNAME        PIC  X(030).
310809       10 PI-DRKI0195-PERSONSUCHVORNAME     PIC  X(030).
310809*      10 PI-DRKI0195-PERSONNAMEPHONCODE    PIC  X(015).
             10 PI-DRKI0195-PERSONGEBGRUENDDAT    PIC  X(010).
          05 PI-DRKI0195-LAST-UPDATE              PIC  X(026).
      *
       01 PO-DRKI0195.
          05 PO-DRKI0195-RC                  PIC  X(003).
             88 PO-DRKI0195-RC-OK                         VALUE 'OK '.
             88 PO-DRKI0195-RC-CHANGED                    VALUE 'CHA'.
             88 PO-DRKI0195-RC-NOT-FOUND                  VALUE 'NFD'.
      *
      ****** Ende Copy-Member ****************************************
