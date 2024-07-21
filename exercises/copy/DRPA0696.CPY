      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0696.CPY  $
      *    $Revision:   1.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA0696.CPv  $
      *
      *   Rev 1.3   Jan 13 2014 09:47:50   t13ex67
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): PO23, PO21, PF14, PF15
      *
      *   Rev 1.2   Apr 16 2002 14:59:20   T13ex22
      *FPA00346: Änderungen für CRPA0108
      *
      *   Rev 1.1   Apr 21 1999 09:34:26   T13AE18
      *APA00012 neue Funktion DRPA1218
      *
      *   Rev 1.0   11 Nov 1997 17:00:52   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0696  FUNKTIONSAUFRUFE FÜR ZTPA0113      *
      *    -------------------------------------------------------   *
      *    ERSTELLT: PARENT                                          *
      *    DATUM:    10.12.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!10.12.97! 01 ! NEUERSTELLUNG             ! PARENT/  *
      *         !10.02.98!    ! NEUE FUNKTION DRAT0228    ! STORCH   *
      *         !16.04.99!    ! NEUE FUNKTION DRPA1218    ! BOHLINGER*
      * FPA00346!11.03.02!1.02! NEUE FUNKTION DRPA1533    ! MUND     *
      * APA01526!04.12.13!2.01! NEUE FUNKTION DRPA1777    ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01 PI-ZTPA0113.
          05 PI-ZTPA0113-FUNKTION            PIC  X(008).
             88 PI-ZTPA0113-DRPA0697-INS          VALUE 'DRPA0697'.
             88 PI-ZTPA0113-DRPA0698-UPD          VALUE 'DRPA0698'.
             88 PI-ZTPA0113-DRPA0699-DEL          VALUE 'DRPA0699'.
             88 PI-ZTPA0113-DRPA0700-SEL-ALL      VALUE 'DRPA0700'.
             88 PI-ZTPA0113-DRAT0228-COM          VALUE 'DRAT0228'.
             88 PI-ZTPA0113-DRPA1218-UPD-FAELL    VALUE 'DRPA1218'.
             88 PI-ZTPA0113-DRPA1533-DEL-OVW      VALUE 'DRPA1533'.
041213       88 PI-ZTPA0113-DRPA1777-UPD-FOLG     VALUE 'DRPA1777'.
