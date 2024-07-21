      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0898.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRPA0898.CPv  $
      *
      *   Rev 2.4   Jun 05 2019 11:40:22   u130076
      *APA02796: Neuer Dialog für Korrekturen PRO-RATA Faktoren
      *
      *   Rev 2.3   Dec 18 2013 14:34:36   t13ae46
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): PO-Dialoge (PO11-PO13)
      *
      *   Rev 2.2   Jun 30 1999 12:40:52   T13AE18
      *apa00012
      *
      *   Rev 2.1   Apr 21 1999 09:22:30   T13AE18
      *APA00012 neue Funktion DRPA1211
      *
      *   Rev 2.0   Mar 25 1999 14:09:56   T13AE18
      *APA00012 Einführung EVS-Umbuchungs-
      *dialog
      *
      *   Rev 1.0   14 Jan 1998 12:49:34   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0898 FUNKTIONEN FüR                      *
      *                          PROVISIONSDATENMODUL UGPA0002       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Bohlinger                                       *
      *    DATUM:    20.04.1998                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  !AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!20.04.98! 01  !NEUERSTELLUNG             ! BOHLINGER*
      *         !        !1.02 !NEUE FKT. DRPA1056        ! BOHLINGER*
      *         !        !1.03 !NEUE FKT.DRPA1057 DRPA1058! BOHLINGER*
      *         !        !     !DRPA1064                  !          *
      *         !        !1.04 !NEUE FKT.DRPA1092 DRPA1093! BOHLINGER*
      *         !        !     !DRPA1094 DRPA1095 DRPA1110!          *
      *         !        !1.05 !NEUE FKT.DRPA1127 DRPA1128! BOHLINGER*
      *         !        !1.06 !NEUE FKT.DRPA1137 DRPA1141! BOHLINGER*
      *         !        !1.07 !NEUE FKT.DRPA1142 DRPA1151! BOHLINGER*
      *         !22.02.99!1.08 !NEUE FKT.DRPA1169         ! BOHLINGER*
      *         !01.03.99!1.08 !NEUE FKT.DRPA1145         ! BOHLINGER*
      *         !19.03.99!1.08 !NEUE FKT.DRPA1193         ! BOHLINGER*
      *         !12.04.99!1.09 !NEUE FKT.DRPA1211         ! BOHLINGER*
      *         !        !     !                          !          *
      *         !30.06.99!2.02 !NEUE FKT.DRPA1269         ! BOHLINGER*
      * APA01526!13.03.13!2.3  !NEUE FKT.DRPA1739         ! WANG     *
      *         !28.03.13!     !NEUE FKT.DRPA1743         !          *
      *         !04.12.13!     !NEUE FKT.DRPA1778         !          *
      * APA02796!03.04.19!2.4  !Neue FKT.DRPA1915         ! KÖPRÜCÜ  *
      *         !        !     !                                     *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-UGPA0002.
           03 PI-UGPA0002-FUNKTION            PIC X(008).
              88 PI-UGPA0002-DRPA0899             VALUE 'DRPA0899'.
              88 PI-UGPA0002-DRPA0900             VALUE 'DRPA0900'.
              88 PI-UGPA0002-DRPA0904             VALUE 'DRPA0904'.
              88 PI-UGPA0002-DRPA0905             VALUE 'DRPA0905'.
              88 PI-UGPA0002-DRPA0906             VALUE 'DRPA0906'.
              88 PI-UGPA0002-DRPA0907             VALUE 'DRPA0907'.
              88 PI-UGPA0002-DRPA0935             VALUE 'DRPA0935'.
              88 PI-UGPA0002-DRPA1014             VALUE 'DRPA1014'.
              88 PI-UGPA0002-DRPA1015             VALUE 'DRPA1015'.
              88 PI-UGPA0002-DRPA1056             VALUE 'DRPA1056'.
              88 PI-UGPA0002-DRPA1057             VALUE 'DRPA1057'.
              88 PI-UGPA0002-DRPA1058             VALUE 'DRPA1058'.
              88 PI-UGPA0002-DRPA1064             VALUE 'DRPA1064'.
              88 PI-UGPA0002-DRPA1078             VALUE 'DRPA1078'.
              88 PI-UGPA0002-DRPA1092             VALUE 'DRPA1092'.
              88 PI-UGPA0002-DRPA1093             VALUE 'DRPA1093'.
              88 PI-UGPA0002-DRPA1094             VALUE 'DRPA1094'.
              88 PI-UGPA0002-DRPA1095             VALUE 'DRPA1095'.
              88 PI-UGPA0002-DRPA1110             VALUE 'DRPA1110'.
              88 PI-UGPA0002-DRPA1127             VALUE 'DRPA1127'.
              88 PI-UGPA0002-DRPA1128             VALUE 'DRPA1128'.
              88 PI-UGPA0002-DRPA1137             VALUE 'DRPA1137'.
              88 PI-UGPA0002-DRPA1141             VALUE 'DRPA1141'.
              88 PI-UGPA0002-DRPA1142             VALUE 'DRPA1142'.
              88 PI-UGPA0002-DRPA1151             VALUE 'DRPA1151'.
220299        88 PI-UGPA0002-DRPA1169             VALUE 'DRPA1169'.
010399        88 PI-UGPA0002-DRPA1145             VALUE 'DRPA1145'.
190399        88 PI-UGPA0002-DRPA1193             VALUE 'DRPA1193'.
120499        88 PI-UGPA0002-DRPA1211             VALUE 'DRPA1211'.
200699        88 PI-UGPA0002-DRPA1269             VALUE 'DRPA1269'.
130313        88 PI-UGPA0002-DRPA1739             VALUE 'DRPA1739'.
280313        88 PI-UGPA0002-DRPA1743             VALUE 'DRPA1743'.
041213        88 PI-UGPA0002-DRPA1778             VALUE 'DRPA1778'.
030419        88 PI-UGPA0002-DRPA1915             VALUE 'DRPA1915'.

