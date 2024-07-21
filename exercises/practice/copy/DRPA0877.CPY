      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0877.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRPA0877.CPv  $
      *
      *   Rev 2.5   Nov 03 2004 11:54:04   t13ex25
      *APA00295: Daisy-KÜ
      *
      *   Rev 2.4   Dec 17 2003 11:13:48   t13ex21
      *APA00258 - Neue Funktion DRPA1565
      *
      *   Rev 2.3   Aug 12 2003 09:36:44   t13ex21
      *APA00245 - Neue Funktionen
      *
      *   Rev 2.2   21 Sep 2000 16:05:36   T13EX21
      *APA00106 - Neue Funktion
      *
      *   Rev 2.1   Jun 05 1999 16:34:52   T13AE18
      *APA00012 Neue Funktionen
      *
      *   Rev 2.0   Mar 25 1999 14:09:54   T13AE18
      *APA00012 Einführung EVS-Umbuchungs-
      *dialog
      *
      *   Rev 1.0   14 Jan 1998 12:48:12   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0877  MODUL 'VERTRAG'                    *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    26.02.1998                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!26.02.98! 01 ! NEUERSTELLUNG             ! STORCH   *
      * APA00012!07.12.98!1.02! NEUE FUNKTION DRPA1144    !BOHLINGER *
      * APA00012!15.02.99!1.03! NEUE FUNKTION DRPA1166    !BOHLINGER *
      * APA00012!22.02.99!1.04! NEUE FUNKTIONEN           !BOHLINGER *
      * APA00012!02.06.99!2.01! NEUE FUNKTIONEN           !BOHLINGER *
      * APA00106!20.07.00!2.02! NEUE FUNKTION             ! STORCH   *
      * APA00245!29.07.03!2.03! NEUE FUNKTIONEN DRPA1561  ! STORCH   *
      *         !        !    ! UND DRPA1562              !          *
      * APA00258!02.12.03!2.04! NEUE FUNKTION             ! STORCH   *
      * APA00295!15.10.04!2.05! Neue Funktion DRPA1624    ! Jäger    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-UGPA0001.
           05 PI-UGPA0001-FUNKTION            PIC X(008).
              88 PI-UGPA0001-DRPA0871             VALUE 'DRPA0871'.
              88 PI-UGPA0001-DRPA0872             VALUE 'DRPA0872'.
              88 PI-UGPA0001-DRPA0873             VALUE 'DRPA0873'.
              88 PI-UGPA0001-DRPA0874             VALUE 'DRPA0874'.
              88 PI-UGPA0001-DRPA0875             VALUE 'DRPA0875'.
              88 PI-UGPA0001-DRPA0876             VALUE 'DRPA0876'.
              88 PI-UGPA0001-DRPA0948             VALUE 'DRPA0948'.
              88 PI-UGPA0001-DRPA0957             VALUE 'DRPA0957'.
              88 PI-UGPA0001-DRPA1098             VALUE 'DRPA1098'.
              88 PI-UGPA0001-DRPA1099             VALUE 'DRPA1099'.
              88 PI-UGPA0001-DRPA1125             VALUE 'DRPA1125'.
              88 PI-UGPA0001-DRPA1144             VALUE 'DRPA1144'.
              88 PI-UGPA0001-DRPA1145             VALUE 'DRPA1145'.
              88 PI-UGPA0001-DRPA1147             VALUE 'DRPA1147'.
              88 PI-UGPA0001-DRPA1166             VALUE 'DRPA1166'.
220299        88 PI-UGPA0001-DRPA1167             VALUE 'DRPA1167'.
220299        88 PI-UGPA0001-DRPA1168             VALUE 'DRPA1168'.
220299        88 PI-UGPA0001-DRPA1170             VALUE 'DRPA1170'.
020699        88 PI-UGPA0001-DRPA1249             VALUE 'DRPA1249'.
020699        88 PI-UGPA0001-DRPA1250             VALUE 'DRPA1250'.
200700        88 PI-UGPA0001-DRPA1408             VALUE 'DRPA1408'.
290703        88 PI-UGPA0001-DRPA1561             VALUE 'DRPA1561'.
290703        88 PI-UGPA0001-DRPA1562             VALUE 'DRPA1562'.
021203        88 PI-UGPA0001-DRPA1565             VALUE 'DRPA1565'.
151004        88 PI-UGPA0001-DRPA1624             VALUE 'DRPA1624'.
