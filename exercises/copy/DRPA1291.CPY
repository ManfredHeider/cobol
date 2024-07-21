      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1291.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRPA1291.CPv  $
      *
      *   Rev 2.5   05 Mar 2018 11:36:58   t13ex67
      *APA02481: Anzeige des Buchungsdatums im PG11 - MSS
      *
      *   Rev 2.4   Jul 16 2015 15:09:58   t13ae46
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System:
      * Date
      *nmodellanpassungen - JWA
      *
      *   Rev 2.3   Dec 10 2007 12:40:06   t13ex21
      *APA00622: Programmänderungen Martina
      *
      *   Rev 2.2   May 05 2006 14:46:08   t13ae46
      *FPA00844: Korrektur Kontrolle manueller Datensätze
      *
      *   Rev 2.1   Aug 23 1999 11:45:28   T13EX21
      *APA00049 - Neuerstellung
      *
      *   Rev 2.0   Mar 30 1999 08:44:26   P13SP99
      *Neuerstellung
      *
      *   Rev 2.0   Mar 05 1999 10:18:54   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1291  INSERT                             *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    02.08.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00046!02.08.99! 01 ! NEUERSTELLUNG             ! STORCH   *
      * FPA00844!05.05.06! 2.2! PI-DRPA1291-DATEN ERW.    ! WANG     *
      * APA00622!23.05.07! 2.3! NEUE LÄNGE                ! STORCH   *
      * APA01932!22.04.15! 2.4! NEUE LÄNGE                ! WANG     *
      * APA02481!10.07.17! 2.5! NEUE LÄNGE                ! STORCH   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1291                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT <<<                                           *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1291.
          05 PI-DRPA1291-UFKT               PIC  9(002).
050506*   05 PI-DRPA1291-DATEN              PIC  X(145).
050506*   05 PI-DRPA1291-DATEN              PIC  X(171).
220415*   05 PI-DRPA1291-DATEN              PIC  X(173).
220415*   05 PI-DRPA1291-DATEN              PIC  X(179).
100717    05 PI-DRPA1291-DATEN              PIC  X(189).
      *
       01 PO-DRPA1291.
          05 PO-DRPA1291-RC                 PIC  X(003).
             88 PO-DRPA1291-RC-OK           VALUE 'OK '.
      *
      ***** ENDE-COPY-MEMBER *****************************************


