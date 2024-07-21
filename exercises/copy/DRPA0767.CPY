      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0767.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRPA0767.CPv  $
      *
      *   Rev 2.5   Mar 28 2017 08:19:34   t13ae37
      *APA02353: Neuer Produktpartner TeleSon - OZPs 
      *
      *   Rev 2.4   Nov 26 2015 09:40:50   t13ex44
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System
      *
      *   Rev 2.3   Mar 10 2014 16:46:46   t13ae23
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g) Migration
      *
      *   Rev 2.2   Jul 26 2013 11:41:48   t13ae23
      *APA01539: Kalypso TP5: Anpassungen Datenmodell
      *
      *   Rev 2.1   Dec 10 2007 10:43:02   t13ex21
      *APA00622: Programmänderungen Martina
      *
      *   Rev 2.0   Feb 27 1999 14:29:32   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 09:18:06   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   12 Dec 1997 09:46:10   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0767  INSERT                             *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    02.01.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!02.01.98! 01 ! NEUERSTELLUNG             ! STORCH   *
      * APA00012!09.09.98! 1.1! VERLÄNGERT UM 25 BYTE     ! PARENT   *
      * APA00622!23.05.07! 2.1! VERLÄNGERT UM 2 BYTES     ! STORCH   *
260713* APA01539!26.07.13! 2.2! FÜR KALYPSO AUF 435 BYTES ! RAHNER   *
      * APA02353!20.02.17! 2.5! FÜR Telson  AUF 461 BYTES ! Buch     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0767                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT <<<                                           *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0767.
          05 PI-DRPA0767-UFKT               PIC  9(002).
090998*   05 PI-DRPA0767-DATEN              PIC  X(221).
090998*   05 PI-DRPA0767-DATEN              PIC  X(246).
260713*   05 PI-DRPA0767-DATEN              PIC  X(248).
261115*   05 PI-DRPA0767-DATEN              PIC  X(435).
261115*   05 PI-DRPA0767-DATEN              PIC  X(439).
200217    05 PI-DRPA0767-DATEN              PIC  X(461).
      *
       01 PO-DRPA0767.
          05 PO-DRPA0767-RC                 PIC  X(003).
             88 PO-DRPA0767-RC-OK           VALUE 'OK '.
      *
      ***** ENDE-COPY-MEMBER *****************************************
 

