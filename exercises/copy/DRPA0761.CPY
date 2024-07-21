      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0761.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA0761.CPv  $
      *
      *   Rev 2.3   Feb 03 2016 14:58:08   T13AE37
      *APA02208: Anpssungen Korrekturprogramm USt
      *
      *   Rev 2.2   Nov 24 2015 09:30:10   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System
      *
      *   Rev 2.1   Feb 17 2000 17:41:38   T13EX44
      *APA00081
      *PGES-GES aus Standard-Status-Bereich
      *
      *   Rev 2.0   May 26 1999 16:08:26   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   12 Dec 1997 09:45:46   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0761  INSERT                             *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    23.12.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!23.12.97! 01 ! NEUERSTELLUNG             ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      * apa00012!24.08.98! 02 ! erweitert um 722 Byte     ! Parent   *
      *         !        !    ! ( PDPE-Daten ) Denormalis.!          *
      *         !        !    ! 720 Daten und 2 Länge     !          *
      *         !        !    ! PEST verlängert um 2 Byte !          *
      * APA00081!01.02.00!2.01! ERWEITERUNG UM 2 STELLEN  ! SCHEIDEL *
      *         !        !    ! WEGEN PGEB-GES            !          *
      * APA01932!24.11.15!2.2 ! NEUE LÄNGE 1344 FÜR LUCY  ! KÖPRÜCÜ  *
      * APA02208!01.02.16!2.3 ! NEUE LÄNGE 1346 UST-JAHR  ! BUCH     *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0761                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT <<<                                           *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0761.
          05 PI-DRPA0761-UFKT               PIC  9(002).
240898*   05 PI-DRPA0761-DATEN              PIC  X(116).
240898*   05 PI-DRPA0761-DATEN              PIC  X(844).
010216*   05 PI-DRPA0761-DATEN              PIC  X(1344).
010216    05 PI-DRPA0761-DATEN              PIC  X(1346).
      *
       01 PO-DRPA0761.
          05 PO-DRPA0761-RC                 PIC  X(003).
             88 PO-DRPA0761-RC-OK           VALUE 'OK '.
      *
      ***** Ende-Copy-Member *****************************************
 

