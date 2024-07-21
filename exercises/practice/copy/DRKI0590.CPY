      * P13SP99   Stand: 06/12/98    18:01:56
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0590.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0590.CPv  $
      *
      *   Rev 2.0   Apr 16 1999 13:16:00   T13EX19
      *aki00028
      *
      *   Rev 1.0   07 Aug 1998 14:50:18   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI0590 FUNKTION:DIREKTIONAENDERUNG         *
      *                                   AUFLISTEN                  *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BRENNECKE                                       *
      *    DATUM:                                                    *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00001!17.08.98! 01 !NEU                        !BRENNECKE *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI0590                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    FUNKTION: DIREKTIONAENDERUNG LIST                         *
      *                                                              *
      ****************************************************************
       01 PI-DRKI0590.
           05  PI-DRKI0590-UFKT                 PIC  X(002).
      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST-NEXT, 3=QUIT
      ***
       01 PO-DRKI0590.
          05 PO-DRKI0590-RC                     PIC  X(003).
             88 PO-DRKI0590-RC-OK                        VALUE 'OK '.
             88 PO-DRKI0590-RC-LEER                      VALUE 'LER'.
             88 PO-DRKI0590-RC-EOF                       VALUE 'EOF'.
          05 PO-DRKI0590-NI-TAB.
             10 PO-DRKI0590-NI-ELE PIC S9(04) COMP OCCURS  4 TIMES.
          05 PO-DRKI0590-NIS  REDEFINES  PO-DRKI0590-NI-TAB.
             10 PO-DRKI0590-NI-DIRAENDID          PIC  S9(004) COMP.
             10 PO-DRKI0590-NI-DIRAENDPERSONEN    PIC  S9(004) COMP.
             10 PO-DRKI0590-NI-DIRAENDPERSROLL    PIC  S9(004) COMP.
             10 PO-DRKI0590-NI-DIRAENDDTAERLED    PIC  S9(004) COMP.
          05 PO-DRKI0590-DATEN.
             10 PO-DRKI0590-DIRAENDID             PIC   X(013).
             10 PO-DRKI0590-DIRAENDPERSONENID     PIC   X(013).
             10 PO-DRKI0590-DIRAENDPERSROLLE      PIC   X(001).
             10 PO-DRKI0590-DIRAENDDTAERLEDIGT    PIC   X(001).

      *
      ******** Ende Copy-Member **************************************
