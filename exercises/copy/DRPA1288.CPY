      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1288.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1288.CPv  $
      *
      *   Rev 2.1   Jul 27 1999 10:21:52   T13AE18
      *APA00012
      *
      *   Rev 2.0   Mar 30 1999 08:44:24   P13SP99
      *Neuerstellung
      *
      *   Rev 2.0   Mar 05 1999 10:18:54   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1288 FUNKTION 'BEREITSTELLEN TEILMENGE Y'*
      *                                  FüR DAS BUFFER-OZP ZYPA0031 *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    26.07.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!16.07.99! 01 !NEU                        !BOHLINGER *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1288                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01  PI-DRPA1288.
           05  PI-DRPA1288-NAME-ZW-SPEICHER        PIC  X(008).
           05  PI-DRPA1288-TAB-HAUPTEINTRAEGE      OCCURS 10.
               10  PI-DRPA1288-LOG-SATZ-NR-1       PIC  9(005).
               10  PI-DRPA1288-STRG-KOMP           PIC  9(002).
      ***  die steuerung zur kompriermierung kann folgende Werte
      ***  annehmen :
      ***    01 HAUPEINTRAG UND NACHFOLGENDE UNTEREINTRäGE üBERNEHMEN
      ***    02 HAUPEINTRAG üBERNEHMEN, NACHFOLGENDE UNTEREINTRäGE KOMP.

      *
       01  PO-DRPA1288.
           05  PO-DRPA1288-RC                      PIC  X(03).
               88 PO-DRPA1288-RC-OK                        VALUE 'OK '.
           05  PO-DRPA1288-ANZAHL-ERG-ZEILEN       PIC  9(05).

      *
      ******* ENDE COPY-MEMBER **************************************
      *---------------------------------------------------------------*
