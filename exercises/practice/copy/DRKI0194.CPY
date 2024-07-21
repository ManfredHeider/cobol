      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0194.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI0194.CPv  $
      *
      *   Rev 2.1   Sep 30 2009 14:49:36   t13ex51
      *AKI00852: KI-Host - Kundensuche
      *
      *   Rev 2.0   Feb 27 1999 12:53:36   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Aug 1998 16:46:44   T13EX19
      *VB-SCHNITTSTELLE
      *
      *   Rev 1.0   29 Sep 1997 11:56:44   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER:        FUNKTION INSERT  PERSON - TCKI0043    *
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
      *    FÜR DIE FUNKTION DRKI0194                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT <<<        PERSON - TCKI0043                  *
      ****************************************************************
      *
       01 PI-DRKI0194.
          05 PI-DRKI0194-PERSON-KEY.
             10 PI-DRKI0194-PERSONENID            PIC X(13).

          05 PI-DRKI0194-PERSON-DATEN.
             10 PI-DRKI0194-PERSONANREDEID        PIC  S9(004) COMP.
310809*      10 PI-DRKI0194-PERSONKUNDENART       PIC  X(001).
             10 PI-DRKI0194-PERSONANZNAME         PIC  X(030).
310809*      10 PI-DRKI0194-PERSONANZVORNAME      PIC  X(015).
310809*      10 PI-DRKI0194-PERSONSUCHNAME        PIC  X(015).
310809*      10 PI-DRKI0194-PERSONSUCHVORNAME     PIC  X(015).
310809       10 PI-DRKI0194-PERSONANZVORNAME      PIC  X(030).
310809       10 PI-DRKI0194-PERSONSUCHNAME        PIC  X(030).
310809       10 PI-DRKI0194-PERSONSUCHVORNAME     PIC  X(030).
310809*      10 PI-DRKI0194-PERSONNAMEPHONCODE    PIC  X(015).
             10 PI-DRKI0194-PERSONGEBGRUENDDAT    PIC  X(010).
310809*      10 PI-DRKI0194-PERSONNOTIZID         PIC  X(013).
310809*      10 PI-DRKI0194-PERSONEMPFOHLVON.
310809*        15 PI-DRKI0194-PERSONEMPFOHLVON-L  PIC S9(004) COMP.
310809*        15 PI-DRKI0194-PERSONEMPFOHLVON-T  PIC  X(050).
310809*      10 PI-DRKI0194-PERSONSTEUERBERAT.
310809*        15 PI-DRKI0194-PERSONSTEUERBER-L   PIC S9(004) COMP.
310809*        15 PI-DRKI0194-PERSONSTEUERBER-T   PIC  X(050).
310809*      10 PI-DRKI0194-PERSONSTEUERBTELEF.
310809*        15 PI-DRKI0194-PERSONSTEUERBTEL-L  PIC S9(004) COMP.
310809*        15 PI-DRKI0194-PERSONSTEUERBTEL-T  PIC  X(020).
             10 PI-DRKI0194-LAST-UPDATE           PIC  X(026).

          05 PI-DRKI0194-PERSON-NULL-INDIK.
             10 PI-DRKI0194-NI-ANREDEID           PIC  S9(004) COMP.
             10 PI-DRKI0194-NI-ANZNAME            PIC  S9(004) COMP.
             10 PI-DRKI0194-NI-ANZVORNAME         PIC  S9(004) COMP.
             10 PI-DRKI0194-NI-SUCHNAME           PIC  S9(004) COMP.
             10 PI-DRKI0194-NI-SUCHVORNAME        PIC  S9(004) COMP.
310809*      10 PI-DRKI0194-NI-NAMEPHONCODE       PIC  S9(004) COMP.
             10 PI-DRKI0194-NI-GEBGRUENDDAT       PIC  S9(004) COMP.
310809*      10 PI-DRKI0194-NI-NOTIZID            PIC  S9(004) COMP.
310809*      10 PI-DRKI0194-NI-EMPFOHLVON         PIC  S9(004) COMP.
310809*      10 PI-DRKI0194-NI-STEUERBERAT        PIC  S9(004) COMP.
310809*      10 PI-DRKI0194-NI-STEUERBTELEF       PIC  S9(004) COMP.
      *
       01 PO-DRKI0194.
          05 PO-DRKI0194-RC                       PIC  X(003).
             88 PO-DRKI0194-RC-OK                         VALUE 'OK '.
             88 PO-DRKI0194-RC-DUPLICATE-VALUE            VALUE 'DVA'.
      *
      ****** ENDE COPY-MEMBER DRKI0194 *******************************
