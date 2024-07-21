      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1028.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1028.CPv  $
      *
      *   Rev 2.1   Nov 03 2015 09:13:42   t13ae46
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: PEG (B
      *atch)
      *
      *   Rev 2.0   May 26 1999 13:42:28   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   14 Jul 1998 13:59:48   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1028                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<Buchung neutralisieren>>                 *
      *    --------------------------------------------------------   *
      *    ERSTELLT: FOLTZ                                            *
      *    DATUM:    28.07.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!28.07.98! 01 ! NEU                       ! FOLTZ    *
      * APA01932!23.10.15! 2.1! PI-DRPA1028-PGEB-GES HINZU! WANG     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1028                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1028.
          05 PI-DRPA1028-BUCHUNG.
             COPY DRPA0451.
          05 PI-DRPA1028-B-PEST-PEDB.
             COPY DRPA0433.
      *
          05 PI-DRPA1028-GEGENBUCHUNG.
             COPY DRPA0451.
          05 PI-DRPA1028-GB-PEST-PEDB.
             COPY DRPA0433.

231015    05 PI-DRPA1028-PGEB-GES                   PIC S9(004) BINARY.
      *
       01 PO-DRPA1028.
          05 PO-DRPA1028-RC                         PIC  X(003).
             88 PO-DRPA1028-RC-NEUT-UNZULAESS            VALUE 'NUZ'.
             88 PO-DRPA1028-RC-NEUT-OK-NOT-FND           VALUE 'NNF'.
             88 PO-DRPA1028-RC-NEUT-OK                   VALUE 'NEO'.
             88 PO-DRPA1028-RC-NEUT-OK-ALLE              VALUE 'NEA'.
      *
          05 PO-DRPA1028-GEGENBUCHUNG.
             COPY DRPA0451.
          05 PO-DRPA1028-BUCHUNG.
             COPY DRPA0451.

      ******* ENDE COPY-MEMBER **************************************

