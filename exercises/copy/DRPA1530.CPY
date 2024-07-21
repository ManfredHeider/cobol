      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1530.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1530.CPv  $
      *
      *   Rev 2.2   Jun 18 2007 11:33:32   t13ex21
      *FPA00915: Korrektur Lesen Gesamtprovision bei I137
      *
      *   Rev 2.1   Mar 20 2002 13:00:38   t13ex43
      *APA00177 : AMV-Rückbelastung mit alten Provisionssätzen
      *
      *   Rev 2.0   04 Jul 2001 11:11:20   p13sp13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1530  FUNKTIONSAUFRUFE FÜR ZWPA0023      *
      *    -------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                       *
      *    DATUM:    19.03.2002                                      *
      ****************************************************************
      *    VERSION   2.01                                            *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00177!19.03.02!2.01! NEU                       ! KLEEMEIER*
      * APA00622!15.06.07!2.02! NEUE FUNKTION DRPA1664    ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
          01 PI-ZWPA0023.
           05 PI-ZWPA0023-FUNKTION     PIC  X(008).
            88 PI-ZWPA0023-DRPA1531    VALUE 'DRPA1531'.
            88 PI-ZWPA0023-DRPA1664    VALUE 'DRPA1664'.
      *
