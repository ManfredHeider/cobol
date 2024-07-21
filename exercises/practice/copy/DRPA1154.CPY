      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa1154.cpy  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\drpa1154.cpv  $
      *
      *   Rev 2.0   May 27 1999 12:56:10   T13EX25
      *APA00042, Neuerstellung V2.0
      *
      *   Rev 1.0   21 Oct 1998 14:01:30   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1154 FUNKTION 'BEREITSTELLEN TEILMENGE Y'*
      *                                  FüR DAS BUFFER-OZP ZYPA0016 *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    18.01.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!18.01.99! 01 !NEU                        !BOHLINGER *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1154                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01  PI-DRPA1154.
           05  PI-DRPA1154-NAME-ZW-SPEICHER        PIC  X(008).
           05  PI-DRPA1154-TAB-HAUPTEINTRAEGE      OCCURS 11.
               10  PI-DRPA1154-LOG-SATZ-NR-1       PIC  9(005).

      *
       01  PO-DRPA1154.
           05  PO-DRPA1154-RC                      PIC  X(03).
               88 PO-DRPA1154-RC-OK                        VALUE 'OK '.
           05  PO-DRPA1154-ANZAHL-ERG-ZEILEN       PIC  9(05).

      *
      ******* ENDE COPY-MEMBER **************************************
