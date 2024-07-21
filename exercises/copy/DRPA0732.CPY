      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0732.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRPA0732.CPv  $
      *
      *   Rev 2.5   Feb 28 2017 09:54:46   t13ae37
      *APA02275: DAISY-Erweiterungen zu FlexN - PVW 2
      *
      *   Rev 2.4   Apr 15 2016 12:55:52   t13ae46
      *APA02220: 4 Vor- und Nachkommastellen zur Konditionsermittlung
      *
      *   Rev 2.3   Nov 24 2015 09:30:00   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System
      *
      *   Rev 2.2   Mar 10 2014 16:46:40   t13ae23
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g) Migration
      *
      *   Rev 2.1   Jul 26 2013 11:41:44   t13ae23
      *APA01539: Kalypso TP5: Anpassungen Datenmodell
      *
      *   Rev 2.0   May 26 1999 15:16:04   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   12 Dec 1997 09:43:48   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: drpa0732  INSERT                             *
      *    -------------------------------------------------------   *
      *    ERSTELLT: J.OPRITZ                                        *
      *    DATUM:    22.12.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0022!22.12.97! 01 ! NEUERSTELLUNG             ! J.OPRITZ *
      *         !        !    !                           !          *
      * APA00012!09.09.98! 1.2! Anpassung Datenlänge an   ! Parent   *
      *         !        !    ! DCLGEN CTPA0117           !          *
260713* APA01539!16.07.13!2.1 ! NEUE LÄNGE 193 F. KALYPSO ! RAHNER   *
      * APA01932!24.11.15!2.2 ! NEUE LÄNGE 203 FÜR LUCY   ! KÖPRÜCÜ  *
      * APA02220!15.04.16!2.4 ! NEUE LÄNGE 204 /4 VOR-UND ! KÖPRÜCÜ  *
      *         !        !    ! NACHKOMMASTELLEN/PROVSATZ ! WANG     *
      * APA02275!31.01.17!2.5 ! NEUE LÄNGE WEGEN FLEXN    ! BUCH     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION drpa0732                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT <<<                                           *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0732.
          05 PI-DRPA0732-UFKT               PIC  9(002).
090998*   05 PI-DRPA0732-DATEN              PIC  X(189).
260713*   05 PI-DRPA0732-DATEN              PIC  X(192).
260713*   05 PI-DRPA0732-DATEN              PIC  X(193).
150416*   05 PI-DRPA0732-DATEN              PIC  X(203).
310117*   05 PI-DRPA0732-DATEN              PIC  X(204).
310117    05 PI-DRPA0732-DATEN              PIC  X(214).
      *
       01 PO-DRPA0732.
          05 PO-DRPA0732-RC                 PIC  X(003).
             88 PO-DRPA0732-RC-OK           VALUE 'OK '.
      *
      ***** Ende-Copy-Member *****************************************
 
 

