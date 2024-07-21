      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0796.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/DRPA0796.CPv  $
      *
      *   Rev 2.1   Apr 16 2002 14:59:46   T13ex22
      *FPA00346: Änderungen für CRPA0108
      *
      *   Rev 2.0   May 26 1999 13:16:04   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   12 Dec 1997 09:48:20   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0796  FUNKTIONSAUFRUFE FÜR ZTPA0115      *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    14.01.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!14.01.98! 01 ! NEUERSTELLUNG             ! STORCH   *
      *         !10.02.98!    ! NEUE FUNKTION DRAT0228    ! STORCH   *
      * FPA00346!11.03.02!2.03 ! NEUE FUNKTION DRPA1534   ! MUND     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-ZTPA0115.
           05  PI-ZTPA0115-FUNKTION            PIC  X(008).
               88 PI-ZTPA0115-DRPA0797-INS          VALUE 'DRPA0797'.
               88 PI-ZTPA0115-DRPA0798-UPD          VALUE 'DRPA0798'.
               88 PI-ZTPA0115-DRPA0799-DEL          VALUE 'DRPA0799'.
               88 PI-ZTPA0115-DRPA0800-SEL-ALL      VALUE 'DRPA0800'.
100298         88 PI-ZTPA0115-DRAT0228-COM          VALUE 'DRAT0228'.
110302         88 PI-ZTPA0115-DRPA1534-DEL-OVW      VALUE 'DRPA1534'.
