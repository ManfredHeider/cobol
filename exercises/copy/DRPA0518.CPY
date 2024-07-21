      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0518.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRPA0518.CPv  $
      *
      *   Rev 2.6   22 May 2018 08:45:26   t13ae37
      *APA02579: FriWi: Provisionsverteilung- und Abrechnung - Copy
      * ABU
      *
      *   Rev 2.5   Mar 27 2017 16:47:04   t13ae46
      *APA02353: Neuer Produktpartner TeleSon - PEG
      *
      *   Rev 2.4   Sep 17 2015 08:51:08   t13ex67
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: Datenm
      *odellanpassungen - MSS
      *
      *   Rev 2.3   Jul 16 2015 15:00:48   t13ex67
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: Datenm
      *odellanpassungen - MSS
      *
      *   Rev 2.2   Mar 10 2014 16:45:28   t13ae23
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g) Migration
      *
      *   Rev 2.1   Jul 26 2013 11:40:50   t13ae23
      *APA01539: Kalypso TP5: Anpassungen Datenmodell
      *
      *   Rev 2.0   Feb 27 1999 14:29:12   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 09:25:30   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   12 Sep 1997 16:59:30   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0518  INSERT                             *
      *    -------------------------------------------------------   *
      *    ERSTELLT: T. MUND                                         *
      *    DATUM:    25.09.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA....!25.09.97! 01 ! Neuerstellung             ! T. MUND  *
      *         !        !    !                           !          *
      * AFPA0012!20.11.97!    ! DATENBEREICH AUF 506      ! WOLFF    *
      *         !        !    ! BYTES GEÄNDERT            !          *
      * APA01539!26.07.13!2.1 ! DATENBEREICH AUF 752      !          *
      *         !        !    ! BYTES GEÄNDERT (KALYPSO)  ! RAHNER   *
      * APA01932!22.06.15!2.2 ! DATENBEREICH VERLÄNGERT   ! STORCH   *
      *         !16.09.15!2.3 ! DATENBEREICH VERLÄNGERT   ! STORCH   *
      * APA02353!09.03.17!2.5 ! DATENBEREICH VERLÄNGERT   ! WANG     *
      * APA02579!18.05.18!2.6 ! DATENBEREICH VERLÄNGERT   ! BUCH     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0518                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT <<<                                           *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0518.
          05 PI-DRPA0518-UFKT               PIC  9(002).
201197*   05 PI-DRPA0518-DATEN              PIC  X(500).
201197*   05 PI-DRPA0518-DATEN              PIC  X(506).
260713*   05 PI-DRPA0518-DATEN              PIC  X(751).
220615*   05 PI-DRPA0518-DATEN              PIC  X(763).
090317*   05 PI-DRPA0518-DATEN              PIC  X(765).
090317*   05 PI-DRPA0518-DATEN              PIC  X(785).
180518    05 PI-DRPA0518-DATEN              PIC  X(791).
      *
       01 PO-DRPA0518.
          05 PO-DRPA0518-RC                 PIC  X(003).
             88 PO-DRPA0518-RC-OK           VALUE 'OK '.
      *
      ***** Ende-Copy-Member *****************************************
 
 
 

