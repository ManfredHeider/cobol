      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0234.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI0234.CPv  $
      *
      *   Rev 2.2   Aug 14 2007 18:35:44   t13ex51
      *AKI00664: Allfinanz: INSERT für ZTKI0068 für Mandant anpassen
      *
      *   Rev 2.1   Aug 09 2007 12:33:08   t13ex51
      *AKI00664: Allfinanz: INSERT für ZTKI0068 für Mandant anpassen
      *
      *   Rev 2.0   Feb 27 1999 12:53:58   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Aug 1998 16:48:30   T13EX19
      *VB-SCHNITTSTELLE
      *
      *   Rev 1.0   23 Jun 1998 13:50:18   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI0234 Funktion INSERT MITARBEITER         *
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
070806* AKI00664!06.08.07! 01 ! neues Feld MITARBMANDANTNR! Rößler   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI0234                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT <<<        NATUERLICHE PERSON                 *
      ****************************************************************
      *
       01 PI-DRKI0234.
          05 PI-DRKI0234-MARB-KEY.
             10 PI-DRKI0234-MITARBPERSONENID    PIC X(013).
             10 PI-DRKI0234-MITARBPERSROLLE     PIC X(001).
          05 PI-DRKI0234-NI-TAB.
             10 PI-DRKI0234-NI-ELE PIC S9(04) COMP OCCURS 04 TIMES.
          05 PI-DRKI0234-NIS  REDEFINES  PI-DRKI0234-NI-TAB.
             10 PI-DRKI0234-NI-VORGESPERSID     PIC  S9(004) COMP.
             10 PI-DRKI0234-NI-VORGESROLLE      PIC  S9(004) COMP.
             10 PI-DRKI0234-NI-MITARBEITERNR    PIC  S9(004) COMP.
070806       10 PI-DRKI0234-NI-MITARBMANDANTNR  PIC  S9(004) COMP.
          05 PI-DRKI0234-MARB-DATEN.
             10 PI-DRKI0234-MITARBVORGESPERSID  PIC  X(013).
             10 PI-DRKI0234-MITARBVORGESROLLE   PIC  X(001).
             10 PI-DRKI0234-MITARBEITERNR       PIC  S9(009) COMP.
070806       10 PI-DRKI0234-MITARBMANDANTNR     PIC  S9(009) COMP.

      *
       01 PO-DRKI0234.
          05 PO-DRKI0234-RC                     PIC  X(003).
             88 PO-DRKI0234-RC-OK                         VALUE 'OK '.
             88 PO-DRKI0234-RC-DUPLICATE-VALUE            VALUE 'DVA'.
      *
      ****** Ende Copy-Member ****************************************
