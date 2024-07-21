      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0801.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRPA0801.CPv  $
      *
      *   Rev 2.4   Oct 26 2004 12:03:54   t13ex44
      *APA00218: Auslagerung von Provisionsdaten
      *
      *   Rev 2.3   Apr 16 2002 15:00:04   T13ex22
      *FPA00346: Änderungen für CRPA0108
      *
      *   Rev 2.2   Feb 18 2002 14:24:46   t13ex43
      *FPA00352 : Korrekturbuchung für halbe Einheiten
      *
      *   Rev 2.1   Jan 18 2000 12:02:26   T13AE18
      *APA00051 'Umbuchen einer PMPT'
      *
      *   Rev 2.0   Jun 24 1999 16:57:20   T13EX21
      *APA00012 - Neue Funktion
      *
      *   Rev 1.1   Apr 21 1999 09:23:18   T13AE18
      *APA00012 Neue Funktion DRPA1219
      *
      *   Rev 1.0   14 Jan 1998 12:42:54   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0801  FUNKTIONSAUFRUFE FÜR ZTPA0117      *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    15.01.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!15.01.98! 01 ! NEUERSTELLUNG             ! STORCH   *
      *         !10.02.98!    ! NEUE FUNKTION DRAT0228    ! STORCH   *
      *         !24.02.98!    ! NEUE FUNKTION DRPA1178    ! LÜTTGES  *
      *         !16.04.99!    ! NEUE FUNKTION DRPA1219    ! BOHLINGER*
      *         !24.06.99!    ! NEUE FUNKTION DRPA1271    ! STORCH   *
      * APA00051!22.06.99!2.1 ! NEUE FUNKTION DRPA0577    ! BOHLINGER*
      * FPA00352!14.02.02!2.2 ! NEUE FUNKTION DRPA1524    ! KLEEMEIER*
      * FPA00346!11.03.02!2.3 ! NEUE FUNKTION DRPA1535    ! MUND     *
      * APA00295!15.07.04!2.4 ! NEUE FUNKTION DRPA1638    ! SCHEIDEL *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-ZTPA0117.
           05  PI-ZTPA0117-FUNKTION            PIC  X(008).
               88 PI-ZTPA0117-DRPA0802-INS          VALUE 'DRPA0802'.
               88 PI-ZTPA0117-DRPA0803-UPD          VALUE 'DRPA0803'.
               88 PI-ZTPA0117-DRPA0804-DEL          VALUE 'DRPA0804'.
               88 PI-ZTPA0117-DRPA0805-SEL-ALL      VALUE 'DRPA0805'.
100298         88 PI-ZTPA0117-DRAT0228-COM          VALUE 'DRAT0228'.
240298         88 PI-ZTPA0117-DRPA1178-UPD-VSTAT    VALUE 'DRPA1178'.
160499         88 PI-ZTPA0117-DRPA1219-UPD-FAELL    VALUE 'DRPA1219'.
240699         88 PI-ZTPA0117-DRPA1271-UPD-PROV     VALUE 'DRPA1271'.
220999         88 PI-ZTPA0117-DRPA0577-INS-VERS     VALUE 'DRPA0577'.
140202         88 PI-ZTPA0117-DRPA1524-UPD-EINH     VALUE 'DRPA1524'.
110302         88 PI-ZTPA0117-DRPA1535-DEL-OVW      VALUE 'DRPA1535'.
150704         88 PI-ZTPA0117-DRPA1638-SEL-VER      VALUE 'DRPA1638'.
      *
