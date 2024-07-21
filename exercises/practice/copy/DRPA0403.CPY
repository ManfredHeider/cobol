      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0403.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRPA0403.CPv  $
      *
      *   Rev 2.4   Apr 07 2016 10:10:58   T13EX67
      *APA02220: 4 Vor- und Nachkommastellen zur Konditionsermittlu
      *ng - TBPA0023/MSS
      *
      *   Rev 2.3   Mar 19 2003 09:21:44   t13ex21
      *APA00223 - Vergrößerung Prov-Multi
      *
      *   Rev 2.2   Nov 21 2002 17:48:46   t13ex22
      *APA00219: PROV-MULTI hinzugefügt
      *
      *   Rev 2.1   Apr 03 2000 11:23:26   T13EX25
      *APA00089, Summe im PI-Bereich neu
      *
      *   Rev 2.0   May 27 1999 12:24:08   T13EX25
      *APA00042, Neuerstellung V2.0
      *
      *   Rev 1.0   23 Jul 1997 12:32:22   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0403  FUNKTION VTF PROVISION BERECHNEN   *
      *                           (UPPA0045)
      *    --------------------------------------------------------- *
      *    ERSTELLT: JÄGER-SOKOLUK                                   *
      *    DATUM:    06.08.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !06.08.97! 01 ! NEUERSTELLUNG             ! JÄGER    *
      * APA00012!20.04.98! 01 ! PRODART HINZUGEFÜGT       ! MUND     *
      * AFPA0012!13.08.98!    ! EINBAU NOTIZ              ! WOLFF    *
      * APA00089!23.03.00! 03 ! SUMME EINGEFÜGT / OHNE ZVP! JÄGER    *
      * APA00219!13.11.02! 04 ! PROV-MULTI ERGÄNZT        ! MUND     *
      * APA00223!12.03.03!2.3 ! PROV-MULTI VERGRÖSSERT    ! STORCH   *
      * APA02220!10.03.16!2.4 ! ÄNDERUNG FORMAT %-SÄTZE   ! STORCH   *
      * DSY04545!08.03.22!    ! PGEB-GES HINZU            ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0403                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0403.
          05 PI-DRPA0403-DATEN.
             10 PI-DRPA0403-KNR-OBJEKT-HKF-TAB      
                                            PIC S9(004) BINARY.
             10 PI-DRPA0403-NTZ-PTR-ID      PIC S9(015) PACKED-DECIMAL.
             10 PI-DRPA0403-GSMT-PROV       PIC S9(009)V9(02)
                                            PACKED-DECIMAL.
             10 PI-DRPA0403-VWF-PROV        PIC S9(009)V9(02)
                                            PACKED-DECIMAL.
             10 PI-DRPA0403-PE-GSMT-PROV    PIC S9(009)V9(02)
                                            PACKED-DECIMAL.
             10 PI-DRPA0403-TOLERANZ        PIC S9(001)V9(02)
                                            PACKED-DECIMAL.
             10 PI-DRPA0403-KNR-REGEL       PIC S9(004)  BINARY.
             10 PI-DRPA0403-BEITRAG         PIC S9(009)V9(02)
                                            PACKED-DECIMAL.
             10 PI-DRPA0403-SUMME           PIC S9(009)V9(02)
                                            PACKED-DECIMAL.
             10 PI-DRPA0403-VTF-PROV-STZ    PIC S9(004)V9(04)
                                            PACKED-DECIMAL.
             10 PI-DRPA0403-PRODART         PIC S9(004) BINARY.
             10 PI-DRPA0403-PROV-MULTI      PIC S9(003)V9(06)
                                            PACKED-DECIMAL.
             10 PI-DRPA0403-PGEB-GES        PIC S9(004) BINARY.
      *
       01 PO-DRPA0403.
          05 PO-DRPA0403-RC                  PIC  X(003).
             88 PO-DRPA0403-RC-OK             VALUE 'OK '.
             88 PO-DRPA0403-RC-REGEL-FALSCH   VALUE 'REF'.
             88 PO-DRPA0403-RC-VSTAT-GEFUELLT VALUE 'VSG'.
          05 PO-DRPA0403-DATEN.
             10 PO-DRPA0403-VTF-PROV-BTRG    PIC S9(009)V9(02)
                                             PACKED-DECIMAL.
      *
      ******** Ende Copy-Member **************************************

