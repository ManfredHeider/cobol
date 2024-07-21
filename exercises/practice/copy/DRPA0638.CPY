      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0638.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA0638.CPv  $
      *
      *   Rev 2.2   Jul 16 2015 15:07:42   t13ae46
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System:
      * Date
      *nmodellanpassungen - JWA
      *
      *   Rev 2.1   Jun 02 2014 11:09:30   t13ex67
      *FPA01364: ZIPA0105 Mandant hinzufügen
      *
      *   Rev 2.0   May 27 1999 09:10:28   T13EX25
      *APA00042, Erfassung V2.0
      *
      *   Rev 1.0   11 Nov 1997 16:56:50   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0638  INSERT                             *
      *    -------------------------------------------------------   *
      *    ERSTELLT: J. Jäger-Sokoluk                                *
      *    DATUM:    09.12.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!09.12.97!01.01! NEUERSTELLUNG            ! J. JÄGER *
      * APA00012! 9.09.98! 1.02! ANPASSUNG DATENLÄNGE AN  ! PARENT   *
      *         !        !     ! DCLGEN CTPA0105          !          *
      * FPA01364!02.06.14! 2.01! PI-DATEN ERWEITERN       ! STORCH   *
      *         !        !     !(MANDANT HINZU)           !          *
      * APA01932!05.05.15! 2.2 ! PI-DATEN ERWEITERN (     ! WANG     *
      *         !        !     !PG-UST-BETRAG,PG-UST-SATZ)!          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0638                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT <<<                                           *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0638.
          05 PI-DRPA0638-UFKT               PIC  9(002).
090998*   05 PI-DRPA0638-DATEN              PIC  X(145).
050515*   05 PI-DRPA0638-DATEN              PIC  X(147).
050515    05 PI-DRPA0638-DATEN              PIC  X(159).
      *
       01 PO-DRPA0638.
          05 PO-DRPA0638-RC                 PIC  X(003).
             88 PO-DRPA0638-RC-OK           VALUE 'OK '.
      *
      ***** ENDE-COPY-MEMBER *****************************************

