      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1051.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA1051.CPv  $
      *
      *   Rev 2.0   May 26 1999 15:45:48   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   14 Jul 1998 14:01:04   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1051 FUNKTION 'BEREITSTELLEN TEILMENGE Y'*
      *                                  FüR DAS BUFFER-OZP ZYPA0012 *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    24.08.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!24.08.98! 01 !NEU                        !BOHLINGER *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1051                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01  PI-DRPA1051.
           05  PI-DRPA1051-NAME-ZW-SPEICHER        PIC  X(008).
           05  PI-DRPA1051-TAB-HAUPTEINTRAEGE      OCCURS 10.
               10  PI-DRPA1051-LOG-SATZ-NR-1       PIC  9(005).
090299         10  PI-DRPA1051-STRG-KOMP           PIC  9(002).
090299***  die steuerung zur kompriermierung kann folgende Werte
      ***  annehmen :
      ***    01 HAUPEINTRAG UND NACHFOLGENDE UNTEREINTRäGE üBERNEHMEN
      ***    02 HAUPEINTRAG üBERNEHMEN, NACHFOLGENDE UNTEREINTRäGE KOMP.

      *
       01  PO-DRPA1051.
           05  PO-DRPA1051-RC                      PIC  X(03).
               88 PO-DRPA1051-RC-OK                        VALUE 'OK '.
           05  PO-DRPA1051-ANZAHL-ERG-ZEILEN       PIC  9(05).

      *
      ******* ENDE COPY-MEMBER **************************************
