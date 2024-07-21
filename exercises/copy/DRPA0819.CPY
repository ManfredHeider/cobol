      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0819.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0819.CPv  $
      *
      *   Rev 2.0   May 26 1999 17:23:10   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   14 Jan 1998 12:44:16   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0819  FUNKTIONSAUFRUFE FÜR UPPA0091      *
      *    -------------------------------------------------------   *
      *    ERSTELLT: OPRITZ                                          *
      *    DATUM:    28.01.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!28.01.98! 01 ! NEUERSTELLUNG             ! OPRITZ   *
      * AFPA0012!28.07.98! 02 ! Alle Funktionen mit Ausn. ! Scheidel *
      *         !        !    ! von DRPA0866 deaktiviert  !          *
      *         !        !    !                           !          *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-UPPA0091.
           05  PI-UPPA0091-FUNKTION                  PIC  X(008).
               88 PI-UPPA0091-DRPA0866-VNR-PIN       VALUE 'DRPA0866'.
      *        88 PI-UPPA0091-DRPA0610-PRINTE        VALUE 'DRPA0610'.
      *        88 PI-UPPA0091-DRPA0625-SEL-PVPT      VALUE 'DRPA0625'.
      *        88 PI-UPPA0091-DRPA0660-SEL-PMBE      VALUE 'DRPA0660'.
      *        88 PI-UPPA0091-DRPA0661-SEL-PMBE      VALUE 'DRPA0661'.
      *        88 PI-UPPA0091-DRPA0667-SEL-PEST      VALUE 'DRPA0667'.
      *        88 PI-UPPA0091-DRPA0672-SEL-PEBD      VALUE 'DRPA0672'.
      *        88 PI-UPPA0091-DRPA0691-SEL-PMPR      VALUE 'DRPA0691'.
      *        88 PI-UPPA0091-DRPA0691-SLE-PMPR      VALUE 'DRPA0691'.
      *        88 PI-UPPA0091-DRPA0692-SEL-PMPT      VALUE 'DRPA0692'.
      *        88 PI-UPPA0091-DRPA0700-SEL-PMBE      VALUE 'DRPA0700'.
      *        88 PI-UPPA0091-DRPA0714-SEL-PDPE      VALUE 'DRPA0714'.
      *        88 PI-UPPA0091-DRPA0719-SEL-PDPT      VALUE 'DRPA0719'.
      *        88 PI-UPPA0091-DRPA0765-SEL-PDPT      VALUE 'DRPA0765'.
      *        88 PI-UPPA0091-DRPA0795-SEL-PIN       VALUE 'DRPA0795'.
      *        88 PI-UPPA0091-DRPA0800-SEL-PMPR      VALUE 'DRPA0800'.
      *        88 PI-UPPA0091-DRPA0805-SEL-PMPT      VALUE 'DRPA0805'.
      *        88 PI-UPPA0091-DRPA0928-ID            VALUE 'DRPA0928'.
