      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1044.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1044.CPv  $
      *
      *   Rev 2.1   Jan 09 2009 13:17:22   t13ex21
      *FPA01113: Prov-Multi in Notiz zum I162 anzeigen
      *
      *   Rev 2.0   May 27 1999 13:25:44   T01JM2S
      *APA00042
      *
      *   Rev 1.0   14 Jul 1998 14:00:42   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1044                                      *
      *    --------------------------------------------------------   *
      *    EVS-NOTIZ                                                  *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    13.08.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!13.08.98! 01 ! NEU                       ! WOLFF    *
      * FPA01113!09.01.09!2.01! PROV-MULTI HINZU          ! STORCH   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE DATEN ZUR NOTIZ "VTF BERECHNEN"   *
      *                                                              *
      ****************************************************************
      *
       01  DRPA1044-NZPA0010.
           05 DRPA1044-ZEILE-1.
              10 FILLER                             PIC  X(080)
                                    VALUE 'NZPA0010 Rundungsdifferenzen
      -                                   'bei VTF-Berechnung'.
      *
           05 DRPA1044-ZEILE-2.
              10 FILLER                             PIC  X(019)
                                    VALUE ' ppfl. Beitrag     '.
              10 DRPA1044-PPFL-BEITRAG              PIC  ----.--9,99.
090109        10 FILLER                             PIC  X(014)
090109                              VALUE '   Prov-Multi '.
090109        10 DRPA1044-PROV-MULTI                PIC --9,999999.
090109*       10 DRPA1044-FILLER-ZL-2               PIC  X(050).
090109        10 DRPA1044-FILLER-ZL-2               PIC  X(026).
      *
           05 DRPA1044-ZEILE-3.
              10 FILLER                             PIC  X(019)
                                    VALUE '*     VTF-Satz     '.
              10 FILLER                             PIC  X(003)
                                    VALUE '   '.
              10 DRPA1044-VTF-SATZ                  PIC  Z9,999.
              10 FILLER                             PIC  X(002)
                                    VALUE ' %'.
              10 DRPA1044-FILLER-ZL-3               PIC  X(050).
      *
           05 DRPA1044-ZEILE-4.
              10 FILLER                             PIC  X(019)
                                    VALUE '--------------     '.
              10 FILLER                             PIC  X(011)
                                    VALUE '-----------'.
              10 DRPA1044-FILLER-ZL-4               PIC  X(050).
      *
           05 DRPA1044-ZEILE-5.
              10 FILLER                             PIC  X(019)
                                    VALUE '      VTF-Prov     '.
              10 DRPA1044-VTF-PROV-1                PIC  ----.--9,99.
              10 FILLER                             PIC  X(023)
                                    VALUE '            VTF-Prov   '.
              10 DRPA1044-VTF-PROV-2                PIC  ----.--9,99.
              10 DRPA1044-FILLER-ZL-5               PIC  X(016).
      *
           05 DRPA1044-ZEILE-6.
              10 FILLER                             PIC  X(019)
                                    VALUE '      VWF-Prov     '.
              10 DRPA1044-VWF-PROV                  PIC  ----.--9,99.
              10 FILLER                             PIC  X(023)
                                    VALUE '   - Summe Prov PE-S   '.
              10 DRPA1044-SUM-PE-PROV               PIC  ----.--9,99.
              10 DRPA1044-FILLER-ZL-6               PIC  X(016).
      *
           05 DRPA1044-ZEILE-7.
              10 FILLER                             PIC  X(019)
                                    VALUE '--------------     '.
              10 FILLER                             PIC  X(011)
                                    VALUE '-----------'.
              10 FILLER                             PIC  X(023)
                                    VALUE '   -----------------   '.
              10 FILLER                             PIC  X(011)
                                    VALUE '-----------'.
              10 DRPA1044-FILLER-ZL-7               PIC  X(016).
      *
           05 DRPA1044-ZEILE-8.
              10 FILLER                             PIC  X(019)
                                    VALUE '     GSMT-Prov     '.
              10 DRPA1044-GSMT-PROV                 PIC  ----.--9,99.
              10 FILLER                             PIC  X(023)
                                    VALUE '       eff. VTF-Prov   '.
              10 DRPA1044-VTF-PROV-EFF              PIC  ----.--9,99.
              10 DRPA1044-FILLER-ZL-8               PIC  X(016).
      *
           05 DRPA1044-ZEILE-9.
              10 FILLER                             PIC  X(019)
                                    VALUE '- PG-GSMT-Prov     '.
              10 DRPA1044-PG-GSMT-PROV              PIC  ----.--9,99.
              10 DRPA1044-FILLER-ZL-9               PIC  X(050).
      *
           05 DRPA1044-ZEILE-10.
              10 FILLER                             PIC  X(019)
                                    VALUE '     Differenz     '.
              10 DRPA1044-DIFFERENZ                 PIC  ----.--9,99.
              10 DRPA1044-FILLER-ZL-10              PIC  X(050).
      *
      *
       01  DRPA1044-NOTIZZEILEN REDEFINES DRPA1044-NZPA0010.
           05 DRPA1044-NOTIZZEILE                   OCCURS 10
                                             INDEXED BY DRPA1044-NZ-IX.
              10 FILLER                             PIC  X(080).
